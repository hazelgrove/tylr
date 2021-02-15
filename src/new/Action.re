open Util;

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

  let mk_zipper: Z.t => Zipper.t;

  let move_into_root: (Direction.t, Tm.t, F.t) => option(Z.t);
  let move_into_frame: (Direction.t, Tm.t, F.bidelimited) => option(Z.t);
};

module Make =
       (
         Tm: Term.S,
         T: Tile.S with module Tm := Tm,
         F: Frame.S with module Tm := Tm,
         Z: Zipper.S with module Tm := Tm and module T := T and module F := F,
         I: S_INPUT,
       ) => {
  // TODO use append_frame here
  let move_into_tile =
      (
        d: Direction.t,
        {prefix, z, suffix}: ZList.t(Term.tile, Term.tile),
        frame: Frame.t,
      )
      : option(Zipper.t) => {
    let n = List.length(prefix);
    let ts = prefix @ [z, ...suffix];
    let rec go = (skel: Skel.t, frame: Frame.t) => {
      let t = List.nth(ts, Skel.root_index(skel));
      let move_into_root = () =>
        I.move_into_root(d, Term.of_skel(skel, ts), frame);
      switch (skel) {
      | Op(m) =>
        assert(n == m);
        move_into_root();
      | Pre(m, r) =>
        n == m ? move_into_root() : go(r, Pre_r(Tile.get_pre(t), frame))
      | Post(l, m) =>
        n == m ? move_into_root() : go(l, Post_l(frame, Tile.get_post(t)))
      | Bin(l, m, r) =>
        let bin = Tile.get_bin(t);
        if (n < m) {
          go(l, Bin_l(frame, bin, r));
        } else if (n > m) {
          go(r, Bin_r(l, bin, frame));
        } else {
          move_into_root();
        };
      };
    };
    go(HExp.associate(ts), frame);
  };

  let tesserae_of_shape: HTessera.Shape.t => ZList.t(HTessera.t, HTessera.t) =
    fun
    | Text(s) => ZList.mk(~z=Text(s), ())
    | Paren_l => ZList.mk(~z=Paren_l, ~suffix=[Paren_r], ())
    | Paren_r => ZList.mk(~prefix=[Paren_l], ~z=Paren_r, ())
    | Lam => ZList.mk(~z=Lam([Op(OpHole)]), ())
    | Let_eq => ZList.mk(~z=Let_eq([Op(OpHole)]), ~suffix=[Let_in], ())
    | Let_in => ZList.mk(~prefix=[Let_eq([Op(OpHole)])], ~z=Let_in, ())
    | Ann => ZList.mk(~z=Ann([Op(OpHole)]), ())
    | Plus => ZList.mk(~z=Plus, ())
    | Arrow => ZList.mk(~z=Arrow, ());

  // focused tessera is the one just constructed if parsing
  // directly after restructured
  exception Invalid_restructure;
  let parse_restructured =
      (
        (prefix, z, suffix):
          ZList.t(option(HTessera.t), Term.Selection.elem),
      )
      : Zipper.t => {
    let rec go =
            (~rev_prefix=[], (prefix, z, suffix): 'ss)
            : (Zipper.t, HTerm.Selection.t) =>
      switch (prefix) {
      | [Tile(tile), ...prefix] =>
        go(~rev_prefix=[tile, ...rev_prefix], (prefix, suffix))
      | [Tessera(tessera), ...prefix] =>
        let open_ = Tessera.get_open(tessera);
        let (zipper, tail) = go((prefix, suffix));
        let (close, tail) = ListUtil.split_first(tail);
        let close = Tessera.get_close(close);
        let (suffix, tail) =
          ListUtil.take_while(
            fun
            | Tile(_) => true
            | Tessera(_) => false,
            tail,
          );
        let zipper =
          append_frame(
            zipper,
            (List.rev(rev_prefix), (open_, close), suffix),
          );
        (zipper, tail);
      | [] =>
        // TODO handle z...
        let (suffix, tail) =
          ListUtil.take_while(
            fun
            | Tile(_) => true
            | Tessera(_) => false,
            suffix,
          );
        let zipper = I.mk(((List.rev(rev_prefix), (), suffix), None));
        (zipper, tail);
      };
    let (prefix, suffix) =
      TupleUtil.map2(Term.Selection.parse, (prefix, suffix));
    let (zipper, tail) = go((prefix, z, suffix));
    assert(tail == []);
    zipper;
  };

  let perform_pointing =
      (
        a: Action.t,
        (prefix, (), suffix) as pointing: Z.Subject.pointing,
        frame: Frame.bidelimited,
      )
      : option(Zipper.t) =>
    switch (a) {
    | Mark =>
      let selecting = (
        List.map(Either.l, prefix),
        (Left, []),
        List.map(Either.l, suffix),
      );
      Some(I.mk((Selecting(selecting), frame)));
    | Move(d) =>
      let j = List.length(prefix);
      let exit = () => {
        let+ tile = frame;
        // TODO first try moving to next child
        I.move_into_frame(d, Term.of_tiles(prefix @ suffix), tile);
      };
      switch (d) {
      | Left when j == 0 => exit()
      | Right when j == List.length(zipped) => exit()
      | _ =>
        let zipped = prefix @ suffix;
        let n = d == Left ? j - 1 : j;
        switch (
          move_into_tile(d, ZList.split_at(n, zipped), Bidelimited(frame))
        ) {
        | Some(_) as entered => entered
        | None =>
          let j = d == Left ? j - 1 : j + 1;
          let (prefix, suffix) = ListUtil.split_n(j, zipped);
          Some(I.mk((Pointing({prefix, z: (), suffix}), frame)));
        };
      };
    | Delete(Left) =>
      let subject = prefix @ suffix;
      let (prefix, tile, suffix) =
        ListUtil.split_nth(List.length(prefix) - 1, subject);
      // break up nth tile into alternating sequence of tesserae + open children
      let A(hd, tl) = AltList.rev(Term.flatten_tile(tile));
      switch (tl) {
      | None =>
        // if consists of a single tessera:
        //   remove, fix empty holes, reassemble into pointing mode
        let (prefix, suffix) =
          ListUtil.take_2(Term.Selection.fix_empty_holes([prefix, suffix]));
        Some(I.mk((Pointing((prefix, (), suffix)), frame)));
      | Some(tl) =>
        // else:
        //   enter restructuring mode on one of the end tesserae:
        //   flatten the open children into tiles
        //   convert the tesserae into selections
        //   put the other tesserae and tiles on prefix or suffix
        let inner_prefix =
          tl
          |> AltList.even_to_list(
               open_child => List.map(Either.l, Term.flatten(open_child)),
               tessera => [Either.R([tessera])],
             )
          |> List.rev
          |> List.flatten;
        let prefix = List.map(Either.l, prefix) @ inner_prefix;
        let suffix = List.map(Either.l, suffix);
        let z = (Left, [[Selection.Tessera(hd)]]);
        Some(I.mk((Restructuring((prefix, z, suffix)), frame)));
      };
    | Delete(Right) =>
      let subject = prefix @ suffix;
      let (prefix, tile, suffix) =
        ListUtil.split_nth(List.length(prefix), subject);
      // break up nth tile into alternating sequence of tesserae + open children
      let A(hd, tl) = Term.flatten_tile(tile);
      switch (tl) {
      | None =>
        // if consists of a single tessera:
        //   remove, fix empty holes, reassemble into pointing mode
        let (prefix, suffix) =
          ListUtil.take_2(Term.Selection.fix_empty_holes([prefix, suffix]));
        Some(I.mk((Pointing((prefix, (), suffix)), frame)));
      | Some(tl) =>
        // else:
        //   enter restructuring mode on one of the end tesserae:
        //   flatten the open children into tiles
        //   convert the tesserae into selections
        //   put the other tesserae and tiles on prefix or suffix
        let inner_suffix =
          tl
          |> AltList.even_to_list(
               open_child => List.map(Either.l, Term.flatten(open_child)),
               tessera => [Either.R([Selection.Tessera(tessera)])],
             )
          |> List.flatten;
        let prefix = List.map(Either.l, prefix);
        let suffix = inner_suffix @ List.map(Either.l, suffix);
        let z = (Right, [[Selection.Tessera(hd)]]);
        Some(I.mk((Restructuring((prefix, z, suffix)), frame)));
      };

    | Construct(shape) =>
      let (ts_before, tessera, ts_after) = tesserae_of_shape(shape);
      if (HTessera.has_child(z)) {
        let (ts_before, ts_after) =
          TupleUtil.map2(List.map(Either.r), (ts_before, ts_after));
        let (prefix, suffix) =
          TupleUtil.map2(List.map(Either.l), (prefix, suffix));
        parse_restructured((
          prefix @ ts_before,
          Some(tessera),
          ts_after @ suffix,
        ));
      } else {
        switch (ts_before, ts_after) {
        | ([], []) =>
          let (prefix, suffix) =
            TupleUtil.map2(List.map(Either.l), (prefix, suffix));
          parse_restructured((prefix, Some(tessera), suffix));
        | _ =>
          // need to repeat this because different either types
          let (prefix, suffix) =
            TupleUtil.map2(List.map(Either.l), (prefix, suffix));
          let (ss_before, ss_after) =
            TupleUtil.map2(
              List.map(tessera => [Tessera(tessera)]),
              (ts_before, ts_after),
            );
          let selections = (ss_before, [Tessera(tessera)], ss_after);
          Some(I.mk((Restructuring((prefix, selections, suffix)), frame)));
        };
      };
    };

  let perform_selecting =
      (
        a: Action.t,
        {prefix, z: (side, selection), suffix} as selecting,
        frame,
      ) =>
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
    };

  let perform_restructuring =
      (
        a: Action.t,
        (prefix, (side, selections), suffix): Z.Subject.restructuring,
        frame: Frame.bidelimited,
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

  let perform = (a: t, (subject, frame): Z.t): option(Zipper.t) =>
    switch (subject) {
    | Normal(normal) => perform_normal(a, subject, frame)
    | Selecting(selecting) => perform_selecting(a, selecting, frame)
    | Restructuring(restructuring) =>
      perform_restructuring(a, restructuring, frame)
    };
};

module Exp = {
  module Make_input = {
    let mk = z => Zipper.Exp(z);

    let move_into_root = (d: Direction.t, subject: HExp.t, frame: Frame.Exp.t) => {
      let mk_normal = tiles =>
        switch (d) {
        | Left => ZList.mk(~prefix=tiles, ~z=(), ())
        | Right => ZList.mk(~z=(), ~suffix=tiles, ())
        };
      subject
      |> HTerm.get(
           fun
           | OpHole
           | Num(_)
           | Var(_) => None
           | Paren(body) => {
               let subject = mk_normal(HExp.flatten(body));
               let frame = Some(Frame.Exp.Paren_body(frame));
               Some(Zipper.Exp((subject, frame)));
             },
           fun
           | (Lam(p), body) => {
               let subject = mk_normal(HPat.flatten(p));
               let frame = Some(Pre(Frame.Pat.Lam_pat(frame), body));
               Some(Zipper.Pat((subject, frame)));
             }
           | (Let(p, def), body) =>
             switch (d) {
             | Left =>
               let subject = mk_normal(HExp.flatten(def));
               let frame = Some(Pre(Frame.Exp.Let_def(frame), body));
               Some(Zipper.Exp((subject, frame)));
             | Right =>
               let subject = mk_normal(HPat.flatten(p));
               let frame = Some(Pre(Frame.Pat.Let_pat(frame), body));
               Some(Zipper.Pat((subject, frame)));
             },
           fun
           | Ap(_) => failwith("todo"),
           fun
           | (_, Plus, _)
           | (_, BinHole, _) => None,
         );
    };

    let move_into_frame =
        (d: Direction.t, subject: HExp.t, frame: Frame.Exp.bidelimited) => {
      open Term.Exp;
      open Frame.Exp;
      let+ tile = frame;
      tile
      |> Tile.get(
           fun
           | Paren_body(frame) => {
               let subject = HTerm.Op(Paren(subject));
               let zipped =
                 Zipper.Exp.zip_to_nearest_bidelimited_frame(subject, frame);
               Zipper.Exp(zipped);
             },
           fun
           | Let_def(p, frame, body) => {
               let subject = HTerm.Pre(Let(p, subject), body);
               let zipped =
                 Zipper.Exp.zip_to_nearest_bidelimited_frame(subject, frame);
               Zipper.Exp(zipped);
             },
           fun
           | Ap_arg(_) => failwith("todo"),
           fun
           | () => raise(Void_bin),
         );
    };
  };
  include Make(Term.Exp, Frame.Exp, Zipper.Exp, Make_input);
};

let perform = (a: t, zipper: Zipper.t): option(Zipper.t) =>
  switch (zipper) {
  | `Typ(zipper) => Typ.perform(a, zipper)
  | `Pat(zipper) => Pat.perform(a, zipper)
  | `Exp(zipper) => Exp.perform(a, zipper)
  };
