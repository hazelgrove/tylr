open Util;
open OptUtil.Syntax;

[@deriving sexp]
type t =
  | Mark
  | Move(Direction.t)
  | Delete(Direction.t)
  | Construct(Unsorted.Tessera.Shape.t);

module type S_INPUT = {
  module Tm: Term.S;
  module T: Tile.S with module Tm := Tm;
  module F: Frame.S with module Tm := Tm;
  module Z:
    Zipper_intf.S with module Tm := Tm and module T := T and module F := F;

  let mk_zipper: Z.t => Zipper.t;
  let append_frame: (Zipper.t, F.bidelimited) => option(Zipper.t);

  let move_into_root: (Direction.t, Tm.t, F.t) => option(Zipper.t);
  let move_into_frame: (Direction.t, Tm.t, F.bidelimited) => option(Zipper.t);
};

module Make =
       (
         Tm: Term.S,
         T: Tile.S with module Tm := Tm,
         F: Frame.S with module Tm := Tm,
         Z: Zipper.S with module Tm := Tm and module T := T and module F := F,
         P: Parser.S with module Tm := Tm and module T := T and module F := F,
         I:
           S_INPUT with
             module Tm := Tm and
             module T := T and
             module F := F and
             module Z := Z,
       ) => {
  let append_frame = I.append_frame;

  let move_into_tile =
      (d: Direction.t, (prefix, z, suffix): ZZList.t(T.t, T.t), frame: F.t)
      : option(Zipper.t) => {
    let n = List.length(prefix);
    let ts = prefix @ [z, ...suffix];
    let rec go = (skel: Skel.t, frame: F.t) => {
      let t = List.nth(ts, Skel.root_index(skel));
      let move_into_root = () =>
        I.move_into_root(d, P.term_of_skel(skel, ts), frame);
      switch (skel) {
      | Op(m) =>
        assert(n == m);
        move_into_root();
      | Pre(m, r) =>
        n == m
          ? move_into_root() : go(r, Uni(Pre_r(Tile.get_pre(t), frame)))
      | Post(l, m) =>
        n == m
          ? move_into_root() : go(l, Uni(Post_l(frame, Tile.get_post(t))))
      | Bin(l, m, r) =>
        let bin = Tile.get_bin(t);
        if (n < m) {
          go(l, Uni(Bin_l(frame, bin, P.term_of_skel(r, ts))));
        } else if (n > m) {
          go(r, Uni(Bin_r(P.term_of_skel(l, ts), bin, frame)));
        } else {
          move_into_root();
        };
      };
    };
    go(P.mk_skel(ts), frame);
  };

  let tesserae_of_shape:
    Unsorted.Tessera.Shape.t =>
    ZZList.t(Unsorted.Tessera.t, Unsorted.Tessera.t) =
    Unsorted.Tessera.(
      fun
      | Text(s) => ([], Text(s), [])
      | Paren_l => ([], Paren_l, [Paren_r])
      | Paren_r => ([Paren_l], Paren_r, [])
      | Lam => ([], Lam([Op(OpHole)]), [])
      | Let_eq => ([], Let_eq([Op(OpHole)]), [Let_in])
      | Let_in => ([Let_eq([Op(OpHole)])], Let_in, [])
      | Ann => ([], Ann([Op(OpHole)]), [])
      | Plus => ([], Plus, [])
      | Arrow => ([], Arrow, [])
    );

  // focused tessera is the one just constructed if parsing
  // directly after restructured
  exception Invalid_restructure;
  let parse_restructured =
      (prefix: Selection.t(T.t), suffix: Selection.t(T.t)): Zipper.t => {
    let rec go =
            (~rev_prefix=[], (prefix, suffix))
            : (Zipper.t, Selection.t(T.t)) =>
      switch (prefix) {
      | [Selection.Tile(tile), ...prefix] =>
        go(~rev_prefix=[tile, ...rev_prefix], (prefix, suffix))
      | [Tessera(open_), ...prefix] =>
        let (zipper, tail) = go((prefix, suffix));
        let (close, tail) = ListUtil.split_first(tail);
        let close = Selection.get_tessera(close);
        let (suffix, tail) =
          ListUtil.take_while(
            fun
            | Selection.Tile(_) => true
            | Tessera(_) => false,
            tail,
          );
        let frame =
          P.assemble_open_bidelimited_frame((
            List.rev(rev_prefix),
            ((open_, []), (close, [])),
            suffix,
          ));
        let zipper =
          OptUtil.get(() => failwith("blah"), append_frame(zipper, frame));
        (zipper, tail);
      | [] =>
        let (suffix, tail) =
          ListUtil.take_while(
            fun
            | Selection.Tile(_) => true
            | Tessera(_) => false,
            suffix,
          );
        let zipper =
          I.mk_zipper((Pointing((List.rev(rev_prefix), (), suffix)), None));
        (zipper, tail);
      };
    let (prefix, suffix) =
      TupleUtil.map2(P.assemble_tiles_in_selection, (prefix, suffix));
    let (zipper, tail) = go((prefix, suffix));
    assert(tail == []);
    zipper;
  };

  // recursive to support action chaining
  let rec perform = (a: t, (subject, frame): Z.t): option(Zipper.t) =>
    switch (subject) {
    | Pointing(pointing) => perform_pointing(a, pointing, frame)
    | Selecting(selecting) => perform_selecting(a, selecting, frame)
    | Restructuring(restructuring) =>
      perform_restructuring(a, restructuring, frame)
    }
  and perform_pointing =
      (
        a: t,
        (prefix, (), suffix) as pointing: Subject.pointing(T.t),
        frame: option(F.bidelimited),
      )
      : option(Zipper.t) =>
    switch (a) {
    | Mark =>
      let selecting = (
        List.map(Selection.tile, prefix),
        (Direction.Left, []),
        List.map(Selection.tile, suffix),
      );
      Some(I.mk_zipper((Selecting(selecting), frame)));
    | Move(d) =>
      let j = List.length(prefix);
      let tiles = prefix @ suffix;
      let exit = () => {
        let* bidelimited = frame;
        // TODO first try moving to next child
        I.move_into_frame(d, P.associate(tiles), bidelimited);
      };
      switch (d) {
      | Left when j == 0 => exit()
      | Right when j == List.length(tiles) => exit()
      | _ =>
        let n = d == Left ? j - 1 : j;
        switch (move_into_tile(d, ZZList.split_at(n, tiles), Bi(frame))) {
        | Some(_) as entered => entered
        | None =>
          let j = d == Left ? j - 1 : j + 1;
          let (prefix, suffix) = ListUtil.split_n(j, tiles);
          Some(I.mk_zipper((Pointing((prefix, (), suffix)), frame)));
        };
      };
    | Delete(Left) =>
      let subject = prefix @ suffix;
      let (prefix, tile, suffix) =
        ListUtil.split_nth(List.length(prefix) - 1, subject);
      // break up nth tile into alternating sequence of tesserae + open children
      let (hd, tl) = AltList.rev(P.disassemble_tile(tile));
      switch (tl) {
      | [] =>
        // if consists of a single tessera:
        //   remove, fix empty holes, reassemble into pointing mode
        let (prefix, suffix) =
          [prefix, suffix]
          |> List.map(List.map(Selection.tile))
          |> P.fix_empty_holes
          |> ListUtil.take_2
          |> TupleUtil.map2(Selection.get_whole);
        Some(I.mk_zipper((Pointing((prefix, (), suffix)), frame)));
      | [_, ..._] =>
        // else:
        //   enter restructuring mode on one of the end tesserae:
        //   flatten the open children into tiles
        //   convert the tesserae into selections
        //   put the other tesserae and tiles on prefix or suffix
        let inner_prefix =
          tl
          |> List.map(((open_child, tessera)) =>
               List.rev_map(Either.l, P.dissociate(open_child))
               @ [Either.R([Selection.Tessera(tessera)])]
             )
          |> List.flatten
          |> List.rev;
        let prefix = List.map(Either.l, prefix) @ inner_prefix;
        let suffix = List.map(Either.l, suffix);
        let z = ([], [Selection.Tessera(hd)], []);
        Some(I.mk_zipper((Restructuring((prefix, z, suffix)), frame)));
      };
    | Delete(Right) =>
      let subject = prefix @ suffix;
      let (prefix, tile, suffix) =
        ListUtil.split_nth(List.length(prefix), subject);
      // break up nth tile into alternating sequence of tesserae + open children
      let (hd, tl) = P.disassemble_tile(tile);
      switch (tl) {
      | [] =>
        // if consists of a single tessera:
        //   remove, fix empty holes, reassemble into pointing mode
        let (prefix, suffix) =
          [prefix, suffix]
          |> List.map(List.map(Selection.tile))
          |> P.fix_empty_holes
          |> ListUtil.take_2
          |> TupleUtil.map2(Selection.get_whole);
        Some(I.mk_zipper((Pointing((prefix, (), suffix)), frame)));
      | [_, ..._] =>
        // else:
        //   enter restructuring mode on one of the end tesserae:
        //   flatten the open children into tiles
        //   convert the tesserae into selections
        //   put the other tesserae and tiles on prefix or suffix
        let inner_suffix =
          tl
          |> List.map(((open_child, tessera)) =>
               [
                 Either.R([Selection.Tessera(tessera)]),
                 ...List.map(Either.l, P.dissociate(open_child)),
               ]
             )
          |> List.flatten;
        let prefix = List.map(Either.l, prefix);
        let suffix = inner_suffix @ List.map(Either.l, suffix);
        let z = ([], [Selection.Tessera(hd)], []);
        Some(I.mk_zipper((Restructuring((prefix, z, suffix)), frame)));
      };

    | Construct(shape) =>
      let (ts_before, tessera, ts_after) = tesserae_of_shape(shape);
      if (Unsorted.Tessera.has_child(tessera)) {
        let (ts_before, ts_after) =
          TupleUtil.map2(
            List.map(Selection.tessera),
            (ts_before, ts_after),
          );
        let (prefix, suffix) =
          TupleUtil.map2(List.map(Selection.tile), (prefix, suffix));
        // TODO need to connect to existing frame
        let constructed =
          parse_restructured(
            prefix @ ts_before,
            [Selection.Tessera(tessera), ...ts_after] @ ts_after,
          );
        perform(Move(Right), constructed);
      } else {
        switch (ts_before, ts_after) {
        | ([], []) =>
          let (prefix, suffix) =
            TupleUtil.map2(List.map(Selection.tile), (prefix, suffix));
          // TODO need to connect to existing frame
          Some(parse_restructured(prefix @ [Tessera(tessera)], suffix));
        | _ =>
          let (prefix, suffix) =
            TupleUtil.map2(List.map(Either.l), (prefix, suffix));
          let (ss_before, ss_after) =
            TupleUtil.map2(
              List.map(tessera => Either.R([Tessera(tessera)])),
              (ts_before, ts_after),
            );
          let selections = (ss_before, [Tessera(tessera)], ss_after);
          Some(
            I.mk_zipper((
              Restructuring((prefix, selections, suffix)),
              frame,
            )),
          );
        };
      };
    }
  and perform_selecting =
      (a: t, {prefix, z: (side, selection), suffix} as selecting, frame) =>
    switch (a) {
    | Mark =>
      let upgrade =
        List.map(
          fun
          | L(tile) => L(tile)
          | R(tessera) => R([tessera]),
        );
      let restructuring = {
        prefix: upgrade(prefix),
        z: (side, [selection]),
        suffix: upgrade(suffix),
      };
      Some(I.mk((Restructuring(restructuring), frame)));

    | Move(d) =>
      let moved_within_zipped = (~prefix=prefix, ~suffix=suffix, selection) =>
        Some(
          I.mk((Selecting({prefix, z: (side, selection), suffix}), frame)),
        );
      if (d != side) {
        if (selection == []) {
          move_selecting(d, {prefix, z: (d, selection), suffix}, frame);
        } else {
          switch (side) {
          | Left =>
            let (first, trailing) = ListUtil.split_first(selection);
            switch (first) {
            | R(_) =>
              let prefix = Term.parse_selection(prefix @ [first]);
              moved_within_zipped(~prefix, trailing);
            | L(tile) =>
              let (open_, body, close) = HTile.flatten_tile(tile);
              let prefix = prefix @ [R(open_)];
              let selection = body @ [R(close), ...trailing];
              moved_within_zipped(~prefix, selection);
            };
          | Right =>
            let (leading, last) = ListUtil.split_last(selection);
            switch (last) {
            | R(_) =>
              let suffix = Term.parse_selection([last, ...suffix]);
              moved_within_zipped(~suffix, selection);
            | L(tile) =>
              let (open_, body, close) = HTile.flatten_tile(tile);
              let suffix = [R(close), ...suffix];
              let selection = leading @ [R(open_), ...body];
              moved_within_zipped(~suffix, selection);
            };
          };
        };
      } else {
        switch (side) {
        | Left =>
          switch (ListUtil.split_last_opt(prefix)) {
          | None => failwith("todo: move into frame")
          | Some((leading, R(tessera))) =>
            let selection = Term.parse_selection([R(tessera), ...selection]);
            moved_within_zipped(~suffix=leading, selection);
          | Some((leading, L(tile))) =>
            let (open_, body, close) = T.flatten_tile(tile);
            let prefix =
              prefix
              @ [R(Open(open_)), ...List.map(Either.l, T.flatten(body))];
            let selection = [R(Close(close)), ...selection];
            moved_within_zipped(~prefix, selection);
          }
        | Right =>
          switch (suffix) {
          | [] => failwith("todo: move into frame")
          | [R(tessera), ...trailing] =>
            let selection = Term.parse_selection(selection @ [R(tessera)]);
            moved_within_zipped(~suffix=trailing, selection);
          | [L(tile), ...trailing] =>
            let (open_, body, close) = T.flatten_tile(tile);
            let suffix =
              List.map(Either.l, T.flatten(body))
              @ [R(Close(close)), ...suffix];
            let selection = selection @ [R(Open(open_))];
            moved_within_zipped(~suffix, selection);
          }
        };
      };

    | Delete(_) =>
      // upgrade tesserae in prefix/suffix into selections
      // upgrade current selection into list of selections
      //  and put in restructuring mode, side depending on delete direction
      failwith("todo")

    | Construct(_) =>
      // if current selection not whole:
      //   fail
      // else:
      //   open/close shapes will return a left/right-focused list of tesserae
      //   if unfocused list empty:
      //     overwrite current selection
      //   else:
      //     position the two tesserae wrapping the first open child
      //      around the current selection
      //     insert other tesserae and fix empty holes
      //     prepend current level of tiles/tesserae on frame
      //     finish with same selection
      failwith("todo")
    }
  and perform_restructuring =
      (
        a: t,
        (prefix, (side, selections), suffix): Z.Subject.restructuring,
        frame: F.bidelimited,
      )
      : option(Zipper.t) =>
    switch (a) {
    | Mark =>
      let flatten = elems =>
        elems
        |> List.map(
             fun
             | L(tile) => [L(tile)]
             | R(selection) => selection,
           )
        |> List.flatten;
      switch (side) {
      | Left =>
        let (first, trailing) = ListUtil.split_first(selections);
        let suffix = [Either.R(first), ...suffix];
        switch (trailing) {
        | [] => Some(parse_restructured((prefix, suffix)))
        | [_, ..._] =>
          Some(
            mk((Restructuring((prefix, (side, trailing), suffix)), frame)),
          )
        };
      | Right =>
        let (leading, last) = ListUtil.split_last(selections);
        let prefix = prefix @ [Either.R(last)];
        switch (leading) {
        | [] => Some(parse_restructured((prefix, suffix)))
        | [_, ..._] =>
          Some(
            mk((Restructuring((prefix, (side, leading), suffix)), frame)),
          )
        };
      };

    | Move(d) =>
      let picked_up_selection = (~prefix=prefix, ~suffix=suffix, selections) =>
        Some(I.mk((Restructuring({prefix, z: selections, suffix}), frame)));
      let picked_up_all_selections =
        List.for_all(Either.is_L, prefix)
        && List.for_all(Either.is_L, suffix);
      let move_through_tile = (prefix, tile, suffix) =>
        if (picked_up_all_selections) {
          let prefix = List.filter_map(Either.get_L, prefix);
          let suffix = List.filter_map(Either.get_L, suffix);
          let+ pointing = move_pointing(d, {prefix, z: (), suffix}, frame);
          G.restructuring_of_pointing(pointing);
        } else {
          let (prefix, suffix) =
            switch (d) {
            | Left => (prefix, [tile, ...suffix])
            | Right => (prefix @ [tile], suffix)
            };
          Some(I.mk_g({prefix, z, suffix}, frame));
        };
      switch (d) {
      | Left =>
        switch (ListUtil.split_last_opt(prefix)) {
        | None => failwith("todo")
        | Some((leading, L(_) as tile)) =>
          move_through_tile(leading, tile, suffix)
        | Some((leading, R(_) as selection)) =>
          picked_up_selection(
            ~prefix=leading,
            (Left, [selection, ...selections]),
          )
        }
      | Right =>
        switch (suffix) {
        | [] => failwith("todo")
        | [L(_) as tile, ...trailing] =>
          move_through_tile(prefix, tile, suffix)
        | [R(selection), ...trailing] =>
          picked_up_selection(
            ~suffix=trailing,
            (Right, selections @ [selection]),
          )
        }
      };

    | Delete(_) =>
      let delete_selections = affix =>
        affix
        |> List.map(
             fun
             | L(tile) => [tile]
             | R(_) => [],
           )
        |> List.flatten;
      let (prefix, suffix) =
        TupleUtil.map2(delete_selections, (prefix, suffix));
      Some(I.mk(Pointing((prefix, (), suffix), frame)));

    | Construct(_) => None
    };
};
