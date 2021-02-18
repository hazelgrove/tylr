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
  module Z: Zipper.S with module Tm := Tm and module T := T and module F := F;

  let mk_pointing: Z.pointing => EditState.pointing;
  let mk_edit_state: Z.t => EditState.t;
  let append_frame: (Z.t, F.bidelimited) => option(Z.t);

  let move_into_root: (Direction.t, Tm.t, F.t) => option(EditState.pointing);
  let move_into_frame:
    (Direction.t, Tm.t, F.bidelimited) => option(EditState.pointing);
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
  let move_into_tile =
      (d: Direction.t, (prefix, z, suffix): ZZList.t(T.t, T.t), frame: F.t)
      : option(EditState.pointing) => {
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
      (prefix: Selection.t(T.t), suffix: Selection.t(T.t)): Z.t => {
    let rec go =
            (~rev_prefix=[], (prefix, suffix)): (Z.t, Selection.t(T.t)) =>
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
          OptUtil.get(
            () => failwith("blah"),
            I.append_frame(zipper, frame),
          );
        (zipper, tail);
      | [] =>
        let (suffix, tail) =
          ListUtil.take_while(
            fun
            | Selection.Tile(_) => true
            | Tessera(_) => false,
            suffix,
          );
        let zipper = (
          Subject.Pointing((List.rev(rev_prefix), (), suffix)),
          F.root,
        );
        (zipper, tail);
      };
    let (prefix, suffix) =
      TupleUtil.map2(P.assemble_tiles_in_selection, (prefix, suffix));
    let (zipper, tail) = go((prefix, suffix));
    assert(tail == []);
    zipper;
  };

  let move_pointing =
      (d: Direction.t, ((prefix, (), suffix), frame): Z.pointing)
      : option(EditState.pointing) => {
    let j = List.length(prefix);
    let tiles = prefix @ suffix;
    let exit = () =>
      // TODO first try moving to next child
      I.move_into_frame(d, P.associate(tiles), frame);
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
        Some(I.mk_pointing(((prefix, (), suffix), frame)));
      };
    };
  };

  // recursive to support action chaining
  let rec perform = (a: t, (subject, frame): Z.t): option(EditState.t) =>
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
        frame: F.bidelimited,
      )
      : option(EditState.t) =>
    switch (a) {
    | Mark =>
      let selecting = (
        List.map(Selection.tile, prefix),
        (Direction.Left, []),
        List.map(Selection.tile, suffix),
      );
      Some(I.mk_edit_state((Selecting(selecting), frame)));
    | Move(d) =>
      Option.map(EditState.of_pointing, move_pointing(d, (pointing, frame)))
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
        Some(I.mk_edit_state((Pointing((prefix, (), suffix)), frame)));
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
        Some(I.mk_edit_state((Restructuring((prefix, z, suffix)), frame)));
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
        Some(I.mk_edit_state((Pointing((prefix, (), suffix)), frame)));
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
        Some(I.mk_edit_state((Restructuring((prefix, z, suffix)), frame)));
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
            [Selection.Tessera(tessera), ...ts_after] @ ts_after @ suffix,
          );
        perform(Move(Right), constructed);
      } else {
        switch (ts_before, ts_after) {
        | ([], []) =>
          let (prefix, suffix) =
            TupleUtil.map2(List.map(Selection.tile), (prefix, suffix));
          // TODO need to connect to existing frame
          Some(
            I.mk_edit_state(
              parse_restructured(prefix @ [Tessera(tessera)], suffix),
            ),
          );
        | _ =>
          let (prefix, suffix) =
            TupleUtil.map2(List.map(Either.l), (prefix, suffix));
          let (ss_before, ss_after) =
            TupleUtil.map2(
              List.map(tessera => [Selection.Tessera(tessera)]),
              (ts_before, ts_after),
            );
          let selections = (
            ss_before,
            [Selection.Tessera(tessera)],
            ss_after,
          );
          Some(
            I.mk_edit_state((
              Restructuring((prefix, selections, suffix)),
              frame,
            )),
          );
        };
      };
    }
  and perform_selecting =
      (
        a: t,
        (prefix, (side, selection), suffix): Subject.selecting(T.t),
        frame,
      ) =>
    switch (a) {
    | Mark =>
      let upgrade =
        List.map(
          fun
          | Selection.Tile(tile) => Either.L(tile)
          | Tessera(tessera) => R([Selection.Tessera(tessera)]),
        );
      let restructuring = (
        upgrade(prefix),
        ([], selection, []),
        upgrade(suffix),
      );
      Some(I.mk_edit_state((Restructuring(restructuring), frame)));

    | Move(d) =>
      let adjust_subject = (~prefix=prefix, ~suffix=suffix, selection) =>
        Some(
          I.mk_edit_state((
            Selecting((prefix, (side, selection), suffix)),
            frame,
          )),
        );
      if (d != side) {
        if (selection == []) {
          perform_selecting(
            Move(d),
            (prefix, (d, selection), suffix),
            frame,
          );
        } else {
          switch (side) {
          | Left =>
            let (first, trailing) = ListUtil.split_first(selection);
            switch (first) {
            | Tessera(t) =>
              let prefix =
                P.assemble_tiles_in_selection(prefix @ [Tessera(t)]);
              adjust_subject(~prefix, trailing);
            | Tile(tile) =>
              let (hd, tl) = Parser_unsorted.disassemble_tile(tile);
              let prefix = prefix @ Selection.[Tessera(hd)];
              let selection =
                (
                  tl
                  |> List.map(((open_child, tessera)) =>
                       List.map(Selection.tile, open_child)
                       @ [Selection.Tessera(tessera)]
                     )
                  |> List.flatten
                )
                @ trailing;
              adjust_subject(~prefix, selection);
            };
          | Right =>
            let (leading, last) = ListUtil.split_last(selection);
            switch (last) {
            | Tessera(t) =>
              let suffix =
                P.assemble_tiles_in_selection([Tessera(t), ...suffix]);
              adjust_subject(~suffix, selection);
            | Tile(tile) =>
              let (hd, tl) =
                AltList.rev(Parser_unsorted.disassemble_tile(tile));
              let suffix = Selection.[Tessera(hd), ...suffix];
              let selection =
                leading
                @ (
                  tl
                  |> List.map(((open_child, tessera)) =>
                       List.rev_map(Selection.tile, open_child)
                       @ [Selection.Tessera(tessera)]
                     )
                  |> List.flatten
                  |> List.rev
                );
              adjust_subject(~suffix, selection);
            };
          };
        };
      } else {
        switch (side) {
        | Left =>
          switch (ListUtil.split_last_opt(prefix)) {
          | None => failwith("todo: move into frame")
          | Some((leading, Tessera(t))) =>
            let selection =
              Parser_unsorted.assemble_tiles_in_selection([
                Selection.Tessera(t),
                ...selection,
              ]);
            adjust_subject(~suffix=leading, selection);
          | Some((leading, Tile(tile))) =>
            let (hd, tl) = AltList.rev(P.disassemble_tile(tile));
            let prefix =
              leading
              @ (
                tl
                |> List.map(((open_child, tessera)) =>
                     List.rev_map(Selection.tile, P.dissociate(open_child))
                     @ [Selection.Tessera(tessera)]
                   )
                |> List.flatten
                |> List.rev
              );
            let selection = Selection.[Tessera(hd), ...selection];
            adjust_subject(~prefix, selection);
          }
        | Right =>
          switch (suffix) {
          | [] => failwith("todo: move into frame")
          | [Tessera(t), ...trailing] =>
            let selection =
              Parser_unsorted.assemble_tiles_in_selection(
                selection @ [Tessera(t)],
              );
            adjust_subject(~suffix=trailing, selection);
          | [Tile(tile), ...trailing] =>
            let (hd, tl) = P.disassemble_tile(tile);
            let suffix =
              (
                tl
                |> List.map(((open_child, tessera)) =>
                     List.map(Selection.tile, P.dissociate(open_child))
                     @ [Selection.Tessera(tessera)]
                   )
                |> List.flatten
              )
              @ trailing;
            let selection = selection @ [Tessera(hd)];
            adjust_subject(~suffix, selection);
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
        (prefix, selections, suffix): Subject.restructuring(T.t),
        frame: F.bidelimited,
      )
      : option(EditState.t) =>
    switch (a) {
    | Mark =>
      let flatten = elems =>
        elems
        |> List.map(
             fun
             | Either.L(tile) => [Selection.Tile(tile)]
             | R(selection) => selection,
           )
        |> List.flatten;
      let (ss_before, selection, ss_after) = selections;
      let+ selection =
        selection
        |> List.map(
             fun
             | Selection.Tile(tile) =>
               Option.map(Selection.tile, P.sort(tile))
             | Tessera(t) => Some(Tessera(t)),
           )
        |> OptUtil.sequence;
      // TODO add side to focused selection in restructuring
      let suffix = [Either.R(selection), ...suffix];
      switch (ss_before, ss_after) {
      | ([], []) =>
        I.mk_edit_state(
          parse_restructured(flatten(prefix), flatten(suffix)),
        )
      | ([s, ...ss_before], _) =>
        let restructuring = (prefix, ([], s, ss_before @ ss_after), suffix);
        I.mk_edit_state((Restructuring(restructuring), frame));
      | ([], [s, ...ss_after]) =>
        let restructuring = (prefix, ([], s, ss_after), suffix);
        I.mk_edit_state((Restructuring(restructuring), frame));
      };

    | Move(d) =>
      let move_through_tile = (prefix, tile, suffix) => {
        let prefix_is_all_tiles =
          OptUtil.sequence(List.map(Either.get_L, prefix));
        let suffix_is_all_tiles =
          OptUtil.sequence(List.map(Either.get_L, suffix));
        switch (prefix_is_all_tiles, suffix_is_all_tiles) {
        | (None, _)
        | (_, None) =>
          let tile = Either.L(tile);
          let (prefix, suffix) =
            switch (d) {
            | Left => (prefix, [tile, ...suffix])
            | Right => (prefix @ [tile], suffix)
            };
          Some(
            I.mk_edit_state((
              Restructuring((prefix, selections, suffix)),
              frame,
            )),
          );
        | (Some(prefix), Some(suffix)) =>
          let (prefix, suffix) =
            switch (d) {
            | Left => (prefix @ [tile], suffix)
            | Right => (prefix, [tile, ...suffix])
            };
          let+ moved = move_pointing(d, ((prefix, (), suffix), frame));
          EditState.restructuring_of_pointing(selections, moved);
        };
      };
      let pick_up_selection = (~prefix=prefix, ~suffix=suffix, selection) => {
        let selection = Selection.map_tile(P.unsort, selection);
        let selections = {
          let ss = ZZList.erase(s => s, selections);
          switch (d) {
          | Left => ([], selection, ss)
          | Right => (ss, selection, [])
          };
        };
        Some(
          I.mk_edit_state((
            Restructuring((prefix, selections, suffix)),
            frame,
          )),
        );
      };
      switch (d) {
      | Left =>
        switch (ListUtil.split_last_opt(prefix)) {
        | None => failwith("todo")
        | Some((leading, L(tile))) =>
          move_through_tile(leading, tile, suffix)
        | Some((leading, R(selection))) =>
          pick_up_selection(~prefix=leading, selection)
        }
      | Right =>
        switch (suffix) {
        | [] => failwith("todo")
        | [L(tile), ...trailing] =>
          move_through_tile(prefix, tile, trailing)
        | [R(selection), ...trailing] =>
          pick_up_selection(~suffix=trailing, selection)
        }
      };

    | Delete(_) =>
      let delete_selections = affix =>
        affix
        |> List.map(
             fun
             | Either.L(tile) => [tile]
             | R(_) => [],
           )
        |> List.flatten;
      let (prefix, suffix) =
        TupleUtil.map2(delete_selections, (prefix, suffix));
      let _ = failwith("need to fix empty holes");
      Some(
        I.mk_edit_state((Subject.Pointing((prefix, (), suffix)), frame)),
      );

    | Construct(_) => None
    };
};
