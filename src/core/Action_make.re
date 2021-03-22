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

  let mk_pointing: Z.pointing => EditState_pointing.t;
  let mk_edit_state: Z.t => EditState.t;

  let move_into_root:
    (Direction.t, Tm.t, F.t) => option(EditState_pointing.t);
  let move_into_frame:
    (Direction.t, Tm.t, F.bidelimited) => option(EditState_pointing.t);

  let select_into_frame: Z.selecting => option(EditState_selecting.t);
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
  // TODO reuse Parser.associate_frame_around_root
  let move_into_tile =
      (
        d: Direction.t,
        z: T.t,
        (prefix, suffix): ListUtil.frame(T.t),
        frame: F.t,
      )
      : option(EditState_pointing.t) => {
    let n = List.length(prefix);
    let ts = ListUtil.of_frame(~subject=[z], (prefix, suffix));
    let rec go = (skel: Skel.t, frame: F.t) => {
      let t = List.nth(ts, Skel.root_index(skel));
      let move_into_root = () => {
        let (term, _) = P.term_of_skel(skel, ts);
        I.move_into_root(d, term, frame);
      };
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
          let (r, _) = P.term_of_skel(r, ts);
          go(l, Uni(Bin_l(frame, bin, r)));
        } else if (n > m) {
          let (l, _) = P.term_of_skel(l, ts);
          go(r, Uni(Bin_r(l, bin, frame)));
        } else {
          move_into_root();
        };
      };
    };
    go(P.mk_skel(ts), frame);
  };

  let tesserae_of_shape:
    Unsorted.Tessera.Shape.t =>
    (Unsorted.Tessera.t, ListUtil.frame(Unsorted.Tessera.t)) =
    Unsorted.Tessera.(
      fun
      | Text(s) => (Text(s), ([], []))
      | Paren_l => (Paren_l, ([], [Paren_r]))
      | Paren_r => (Paren_r, ([Paren_l], []))
      | Lam => (Lam([Op(OpHole)]), ([], []))
      | Let_eq => (Let_eq([Op(OpHole)]), ([], [Let_in]))
      | Let_in => (Let_in, ([Let_eq([Op(OpHole)])], []))
      | Ann => (Ann([Op(OpHole)]), ([], []))
      | Plus => (Plus, ([], []))
      | Arrow => (Arrow, ([], []))
    );

  let parse_pointing =
      (
        (prefix, suffix): ListUtil.frame(Selection.elem(T.t)),
        frame: F.bidelimited,
      )
      : option(Z.pointing) => {
    let rec go =
            (d, affix)
            : option(
                AltList.t(
                  list(T.t),
                  AltList.t(Unsorted.Tessera.t, list(T.t)),
                ),
              ) =>
      switch (affix) {
      | [] => Some(([], []))
      | [Selection.Tile(tile), ...affix] =>
        let+ (inner_affix, tile_halves__affixes) = go(d, affix);
        ([tile, ...inner_affix], tile_halves__affixes);
      | [Tessera(tessera), ...affix] =>
        let* (rest_of_tile, affix) =
          Parser_unsorted.find_rest_of_tile(d, tessera, affix);
        let+ (inner_affix, tile_halves__affixes) = go(d, affix);
        (
          [],
          [((tessera, rest_of_tile), inner_affix), ...tile_halves__affixes],
        );
      };
    let* (inner_prefix, tile_halves_l__prefixes) =
      go(Left, P.assemble_tiles_in_selection(~direction=Left, prefix));
    let* (inner_suffix, tile_halves_r__suffixes) =
      go(Right, P.assemble_tiles_in_selection(~direction=Right, suffix));
    let* tiles_affixes =
      ListUtil.combine_opt(tile_halves_l__prefixes, tile_halves_r__suffixes);
    let+ frame =
      List.fold_right(
        (((tile_half_l, prefix), (tile_half_r, suffix)), frame) => {
          let* frame = frame;
          let tile_halves =
            (tile_half_l, tile_half_r)
            |> TupleUtil.map2(AltList.map_b(P.associate));
          let+ open_frame =
            P.assemble_open_frame(tile_halves, (prefix, suffix), frame);
          F.Open(open_frame);
        },
        tiles_affixes,
        Some(frame),
      );
    ((inner_prefix, inner_suffix), frame);
  };

  let move_pointing =
      (d: Direction.t, ((prefix, suffix), frame): Z.pointing)
      : option(EditState_pointing.t) => {
    let j = List.length(prefix);
    let tiles = ListUtil.of_frame((prefix, suffix));
    let exit = () =>
      // TODO first try moving to next child
      I.move_into_frame(d, P.associate(tiles), frame);
    switch (d) {
    | Left when j == 0 => exit()
    | Right when j == List.length(tiles) => exit()
    | _ =>
      let n = d == Left ? j - 1 : j;
      let (tile, tile_frame) = ListUtil.split_frame(n, tiles);
      switch (move_into_tile(d, tile, tile_frame, Bi(frame))) {
      | Some(_) as entered => entered
      | None =>
        let j = d == Left ? j - 1 : j + 1;
        let (prefix, suffix) = ListUtil.mk_frame(j, tiles);
        Some(I.mk_pointing(((prefix, suffix), frame)));
      };
    };
  };

  exception Impossible;
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
        (prefix, suffix) as pointing: Subject.pointing(T.t),
        frame: F.bidelimited,
      )
      : option(EditState.t) =>
    switch (a) {
    | Mark =>
      let selecting = (
        (Direction.Left, []),
        (
          List.map(Selection.tile, prefix),
          List.map(Selection.tile, suffix),
        ),
      );
      Some(I.mk_edit_state((Selecting(selecting), frame)));
    | Move(d) =>
      Option.map(EditState.of_pointing, move_pointing(d, (pointing, frame)))
    | Delete(Left) =>
      let subject = ListUtil.of_frame(pointing);
      // TODO handle empty prefix
      let (tile, (prefix, suffix)) =
        ListUtil.split_frame(List.length(prefix) - 1, subject);
      // break up nth tile into alternating sequence of tesserae + open children
      let (hd, tl) = AltList.rev(P.disassemble_tile(tile));
      switch (tl) {
      | [] =>
        // if consists of a single tessera:
        //   remove, fix empty holes, reassemble into pointing mode
        let (prefix, suffix) =
          (prefix, suffix)
          |> TupleUtil.map2(affix => [List.map(Selection.tile, affix)])
          |> P.fix_empty_holes
          |> TupleUtil.map2(Selection.get_whole);
        Some(I.mk_edit_state((Pointing((prefix, suffix)), frame)));
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
          |> List.flatten;
        let prefix = inner_prefix @ List.map(Either.l, prefix);
        let suffix = List.map(Either.l, suffix);
        let focused_selection = [Selection.Tessera(hd)];
        Some(
          I.mk_edit_state((
            Restructuring((
              (focused_selection, ([], [])),
              (prefix, suffix),
            )),
            frame,
          )),
        );
      };
    | Delete(Right) =>
      let subject = ListUtil.of_frame(pointing);
      // TODO handle empty suffix
      let (tile, (prefix, suffix)) =
        ListUtil.split_frame(List.length(prefix), subject);
      // break up nth tile into alternating sequence of tesserae + open children
      let (hd, tl) = P.disassemble_tile(tile);
      switch (tl) {
      | [] =>
        // if consists of a single tessera:
        //   remove, fix empty holes, reassemble into pointing mode
        let (prefix, suffix) =
          (prefix, suffix)
          |> TupleUtil.map2(affix => [List.map(Selection.tile, affix)])
          |> P.fix_empty_holes
          |> TupleUtil.map2(Selection.get_whole);
        Some(I.mk_edit_state((Pointing((prefix, suffix)), frame)));
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
        let focused_selection = [Selection.Tessera(hd)];
        Some(
          I.mk_edit_state((
            Restructuring((
              (focused_selection, ([], [])),
              (prefix, suffix),
            )),
            frame,
          )),
        );
      };

    | Construct(shape) =>
      let (tessera, (ts_before, ts_after)) = tesserae_of_shape(shape);
      if (Unsorted.Tessera.has_child(tessera)) {
        let (ts_before, ts_after) =
          TupleUtil.map2(
            List.map(Selection.tessera),
            (ts_before, ts_after),
          );
        let (prefix, suffix) =
          TupleUtil.map2(List.map(Selection.tile), (prefix, suffix));
        let (prefix, suffix) =
          P.fix_empty_holes((
            [ts_before, prefix],
            [[Selection.Tessera(tessera)], ts_after, suffix],
          ));
        let (subject, frame) =
          parse_pointing((prefix, suffix), frame)
          |> OptUtil.get_or_raise(Impossible);
        perform(Move(Right), (Pointing(subject), frame));
      } else {
        switch (ts_before, ts_after) {
        | ([], []) =>
          let (prefix, suffix) =
            TupleUtil.map2(List.map(Selection.tile), (prefix, suffix));
          let (prefix, suffix) =
            P.fix_empty_holes((
              [[Selection.Tessera(tessera)], prefix],
              [suffix],
            ));
          let (subject, frame) =
            parse_pointing((prefix, suffix), frame)
            |> OptUtil.get_or_raise(Impossible);
          Some(I.mk_edit_state((Pointing(subject), frame)));
        | _ =>
          let (prefix, suffix) =
            TupleUtil.map2(List.map(Either.l), (prefix, suffix));
          let (ss_before, ss_after) =
            TupleUtil.map2(
              List.map(tessera => [Selection.Tessera(tessera)]),
              (ts_before, ts_after),
            );
          let selection = [Selection.Tessera(tessera)];
          Some(
            I.mk_edit_state((
              Restructuring((
                (selection, (ss_before, ss_after)),
                (prefix, suffix),
              )),
              frame,
            )),
          );
        };
      };
    }
  and perform_selecting =
      (
        a: t,
        ((side, selection), (prefix, suffix)) as selecting:
          Subject.selecting(T.t),
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
        (selection, ([], [])),
        TupleUtil.map2(upgrade, (prefix, suffix)),
      );
      Some(I.mk_edit_state((Restructuring(restructuring), frame)));

    | Move(d) =>
      let adjust_subject = (~prefix=prefix, ~suffix=suffix, selection) => {
        let mk_edit_state = ((prefix, suffix), frame) =>
          Some(
            I.mk_edit_state((
              Selecting(((side, selection), (prefix, suffix))),
              frame,
            )),
          );
        switch (parse_pointing((prefix, suffix), frame)) {
        | None => mk_edit_state((prefix, suffix), frame)
        | Some(((prefix, suffix), frame)) =>
          let (prefix, suffix) =
            TupleUtil.map2(List.map(Selection.tile), (prefix, suffix));
          mk_edit_state((prefix, suffix), frame);
        };
      };
      if (d != side) {
        if (selection == []) {
          perform_selecting(
            Move(d),
            ((d, selection), (prefix, suffix)),
            frame,
          );
        } else {
          switch (side) {
          | Left =>
            let (first, trailing) = ListUtil.split_first(selection);
            switch (first) {
            | Tessera(t) =>
              let prefix =
                P.assemble_tiles_in_selection(
                  ~direction=Left,
                  [Selection.Tessera(t), ...prefix],
                );
              adjust_subject(~prefix, trailing);
            | Tile(tile) =>
              let (hd, tl) = Parser_unsorted.disassemble_tile(tile);
              switch (tl) {
              | [] =>
                let* tile = P.sort(tile);
                adjust_subject(~prefix=[Tile(tile), ...prefix], trailing);
              | [_, ..._] =>
                let prefix = Selection.[Tessera(hd), ...prefix];
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
            };
          | Right =>
            let (leading, last) = ListUtil.split_last(selection);
            switch (last) {
            | Tessera(t) =>
              let suffix =
                P.assemble_tiles_in_selection(
                  ~direction=Right,
                  [Tessera(t), ...suffix],
                );
              adjust_subject(~suffix, leading);
            | Tile(tile) =>
              let (hd, tl) =
                AltList.rev(Parser_unsorted.disassemble_tile(tile));
              switch (tl) {
              | [] =>
                let* tile = P.sort(tile);
                adjust_subject(~suffix=[Tile(tile), ...suffix], leading);
              | [_, ..._] =>
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
        };
      } else {
        switch (side) {
        | Left =>
          switch (prefix) {
          | [] =>
            Option.map(
              EditState.of_selecting,
              I.select_into_frame((selecting, frame)),
            )
          | [Tessera(t), ...trailing] =>
            let selection =
              Parser_unsorted.assemble_tiles_in_selection(
                ~direction=Right,
                [Selection.Tessera(t), ...selection],
              );
            adjust_subject(~prefix=trailing, selection);
          | [Tile(tile), ...trailing] =>
            let (hd, tl) = AltList.rev(P.disassemble_tile(tile));
            switch (tl) {
            | [] =>
              // maintain maximum structure in selection
              let tile = P.unsort(tile);
              adjust_subject(~prefix=trailing, [Tile(tile), ...selection]);
            | [_, ..._] =>
              let prefix =
                (
                  tl
                  |> List.map(((open_child, tessera)) =>
                       List.rev_map(Selection.tile, P.dissociate(open_child))
                       @ [Selection.Tessera(tessera)]
                     )
                  |> List.flatten
                )
                @ trailing;
              let selection = Selection.[Tessera(hd), ...selection];
              adjust_subject(~prefix, selection);
            };
          }
        | Right =>
          switch (suffix) {
          | [] =>
            Option.map(
              EditState.of_selecting,
              I.select_into_frame((selecting, frame)),
            )
          | [Tessera(t), ...trailing] =>
            let selection =
              Parser_unsorted.assemble_tiles_in_selection(
                ~direction=Right,
                selection @ [Tessera(t)],
              );
            adjust_subject(~suffix=trailing, selection);
          | [Tile(tile), ...trailing] =>
            let (hd, tl) = P.disassemble_tile(tile);
            switch (tl) {
            | [] =>
              // maintain maximum structure in selection
              let tile = P.unsort(tile);
              adjust_subject(~suffix=trailing, selection @ [Tile(tile)]);
            | [_, ..._] =>
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
            };
          }
        };
      };

    | Delete(_) =>
      // upgrade tesserae in prefix/suffix into selections
      let (prefix, suffix) =
        TupleUtil.map2(
          Selection.to_list(Either.l, t => Either.R([Selection.Tessera(t)])),
          (prefix, suffix),
        );
      // upgrade current selection into zlist of selections
      //  and put in restructuring mode
      //  TODO side depending on delete direction
      let subject =
        Subject.Restructuring(((selection, ([], [])), (prefix, suffix)));
      Some(I.mk_edit_state((subject, frame)));

    | Construct(shape) =>
      // TODO unify with hole fixing logic in Parser
      // TODO REVERSE
      let fix_prefix = (prefix: list(T.t), t) => {
        let convex_left = Unsorted.Tessera.is_convex(Left, t);
        switch (ListUtil.split_last_opt(prefix)) {
        | None => convex_left ? prefix : [Op(Tm.mk_op_hole())]
        | Some((leading, Op(op))) when Tm.is_op_hole(op) && convex_left => leading
        | Some((leading, Bin(bin))) when Tm.is_bin_hole(bin) && !convex_left => leading
        | Some((_, last)) =>
          if (Tile.is_convex(Right, last) != convex_left) {
            prefix;
          } else if (convex_left) {
            prefix @ [Tile.Bin(Tm.mk_bin_hole())];
          } else {
            prefix @ [Tile.Op(Tm.mk_op_hole())];
          }
        };
      };
      let fix_suffix = (t, suffix: list(T.t)) => {
        let convex_right = Unsorted.Tessera.is_convex(Right, t);
        switch (suffix) {
        | [] => convex_right ? suffix : [Op(Tm.mk_op_hole())]
        | [Op(op), ...trailing] when convex_right && Tm.is_op_hole(op) => trailing
        | [Bin(bin), ...trailing] when !convex_right && Tm.is_bin_hole(bin) => trailing
        | [first, ..._] =>
          if (Tile.is_convex(Left, first) != convex_right) {
            suffix;
          } else if (convex_right) {
            suffix @ [Tile.Bin(Tm.mk_bin_hole())];
          } else {
            suffix @ [Tile.Op(Tm.mk_op_hole())];
          }
        };
      };
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
      let* _ = Selection.is_whole(selection);
      let (prefix_tiles, suffix_tiles) =
        TupleUtil.map2(Selection.get_whole, (prefix, suffix));
      let (t, (ts_before, ts_after)) = tesserae_of_shape(shape);
      switch (ts_before) {
      | [_, ..._] => None
      | [] =>
        switch (ts_after) {
        | [hd, ...tl] =>
          let frame_prefix = (t, []);
          let frame_suffix = (
            hd,
            List.map(t => (Term.Op(Tm.mk_op_hole()), t), tl),
          );
          let prefix_tiles = fix_prefix(prefix_tiles, t);
          let suffix_tiles = {
            let last_t =
              switch (ListUtil.split_last_opt(tl)) {
              | None => hd
              | Some((_, last)) => last
              };
            fix_suffix(last_t, suffix_tiles);
          };
          let+ frame =
            P.assemble_open_frame(
              (frame_prefix, frame_suffix),
              (prefix_tiles, suffix_tiles),
              frame,
            );
          I.mk_edit_state((Selecting(selecting), Open(frame)));
        | [] =>
          let+ tile = P.assemble_tile((t, []));
          let (prefix, suffix) =
            ([[Selection.Tile(tile)], prefix], [suffix])
            |> P.fix_empty_holes
            |> TupleUtil.map2(Selection.get_whole);
          let subject = Subject.Pointing((prefix, suffix));
          I.mk_edit_state((subject, frame));
        }
      };
    }
  and perform_restructuring =
      (
        a: t,
        ((selection, (ss_before, ss_after)), (prefix, suffix)):
          Subject.restructuring(T.t),
        frame: F.bidelimited,
      )
      : option(EditState.t) =>
    switch (a) {
    | Mark =>
      let flatten = elems =>
        elems
        |> List.map(
             fun
             | Either.L(tile) => Some([Selection.Tile(tile)])
             | R(selection) =>
               selection
               |> List.map(
                    fun
                    | Selection.Tile(tile) =>
                      Option.map(Selection.tile, P.sort(tile))
                    | Tessera(t) => Some(Tessera(t)),
                  )
               |> OptUtil.sequence,
           )
        |> OptUtil.sequence
        |> Option.map(List.flatten);
      // TODO add side to focused selection in restructuring
      let suffix = [Either.R(selection), ...suffix];
      switch (List.rev(ss_before), ss_after) {
      | ([], []) =>
        let+ prefix = flatten(prefix)
        and+ suffix = flatten(suffix);
        // TODO fix empty holes
        let (subject, frame) =
          parse_pointing((prefix, suffix), frame)
          |> OptUtil.get_or_raise(Impossible);
        I.mk_edit_state((Pointing(subject), frame));
      | ([s, ...ss_before], _) =>
        let restructuring = (
          (s, ([], ss_before @ ss_after)),
          (prefix, suffix),
        );
        Some(I.mk_edit_state((Restructuring(restructuring), frame)));
      | ([], [s, ...ss_after]) =>
        let restructuring = ((s, ([], ss_after)), (prefix, suffix));
        Some(I.mk_edit_state((Restructuring(restructuring), frame)));
      };

    | Move(d) =>
      let picked_up_all_selections = (prefix, suffix) => {
        let+ whole_prefix = OptUtil.sequence(List.map(Either.get_L, prefix))
        and+ whole_suffix = OptUtil.sequence(List.map(Either.get_L, suffix));
        (whole_prefix, whole_suffix);
      };
      let move_via_pointing = (prefix, suffix) => {
        let+ moved = move_pointing(d, ((prefix, suffix), frame));
        EditState.of_restructuring(
          EditState_restructuring.of_pointing(
            selection,
            (ss_before, ss_after),
            moved,
          ),
        );
      };
      let move_through_tile = (prefix, tile, suffix) => {
        switch (picked_up_all_selections(prefix, suffix)) {
        | None =>
          let tile = Either.L(tile);
          let (prefix, suffix) =
            switch (d) {
            | Left => (prefix, [tile, ...suffix])
            | Right => ([tile, ...prefix], suffix)
            };
          let restructuring = (
            (selection, (ss_before, ss_after)),
            (prefix, suffix),
          );
          Some(I.mk_edit_state((Restructuring(restructuring), frame)));
        | Some((prefix, suffix)) =>
          let (prefix, suffix) =
            switch (d) {
            | Left => ([tile, ...prefix], suffix)
            | Right => (prefix, [tile, ...suffix])
            };
          move_via_pointing(prefix, suffix);
        };
      };
      let pick_up_selection = (~prefix=prefix, ~suffix=suffix, selection) => {
        let selections = {
          let ss = ListUtil.of_frame((ss_before, ss_after));
          switch (d) {
          | Left => ([], ss)
          | Right => (List.rev(ss), [])
          };
        };
        Some(
          I.mk_edit_state((
            Restructuring(((selection, selections), (prefix, suffix))),
            frame,
          )),
        );
      };
      switch (d) {
      | Left =>
        switch (prefix) {
        | [] =>
          let* (prefix, suffix) = picked_up_all_selections(prefix, suffix);
          move_via_pointing(prefix, suffix);
        | [L(tile), ...trailing] =>
          move_through_tile(trailing, tile, suffix)
        | [R(selection), ...trailing] =>
          pick_up_selection(~prefix=trailing, selection)
        }
      | Right =>
        switch (suffix) {
        | [] =>
          let* (prefix, suffix) = picked_up_all_selections(prefix, suffix);
          move_via_pointing(prefix, suffix);
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
      Some(I.mk_edit_state((Subject.Pointing((prefix, suffix)), frame)));

    | Construct(_) => None
    };
};
