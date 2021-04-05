open Util;

module Input = {
  let mk_pointing = p => EditState_pointing.Pat(p);
  let mk_edit_state = z => EditState.Pat(z);

  let can_enter =
    Term_pat.(
      Tile.get(
        fun
        | OpHole
        | Var(_) => false
        | Paren(_) => true,
        () => raise(Void_pre),
        fun
        | Ann(_) => true,
        fun
        | BinHole
        | Prod => false,
      )
    );

  let move_into_root =
      (d: Direction.t, subject: Term_pat.t, frame: Frame_pat.t) => {
    let mk_pointing = tiles =>
      ListUtil.mk_frame(d == Left ? List.length(tiles) : 0, tiles);
    subject
    |> Term.get(
         fun
         | Term_pat.OpHole
         | Var(_) => None
         | Paren(body) => {
             let subject = mk_pointing(Parser_pat.dissociate(body));
             let frame = Frame_pat.Open(Paren_body(frame));
             Some(EditState_pointing.Pat((subject, frame)));
           },
         fun
         | ((), _) => raise(Term_pat.Void_pre),
         fun
         | (subj, Term_pat.Ann(ann)) => {
             let subject = mk_pointing(Parser_typ.dissociate(ann));
             let frame = Frame_typ.Closed(Ann_ann(subj, frame));
             Some(EditState_pointing.Typ((subject, frame)));
           },
         fun
         | (_, BinHole | Prod, _) => None,
       );
  };

  let move_into_frame =
      (d: Direction.t, subject: Term_pat.t, frame: Frame_pat.bidelimited) => {
    open Frame_pat;
    let escaped_tile = (prefix, tile, suffix) =>
      switch (d) {
      | Left => (prefix, [tile, ...suffix])
      | Right => ([tile, ...prefix], suffix)
      };
    switch (frame) {
    | Root => None
    | Open(Paren_body(frame)) =>
      let tile = Tile.Op(Term_pat.Paren(subject));
      let ((prefix, suffix), frame) = Parser_pat.dissociate_frame(frame);
      let subject = escaped_tile(prefix, tile, suffix);
      Some(EditState_pointing.Pat((subject, frame)));
    | Closed(Lam_pat(frame, body)) =>
      let lam_tile = Tile.Pre(Term_exp.Lam(subject));
      let body_tiles = Parser_exp.dissociate(body);
      let ((prefix, suffix), frame) = Parser_exp.dissociate_frame(frame);
      let subject = escaped_tile(prefix, lam_tile, body_tiles @ suffix);
      Some(EditState_pointing.Exp((subject, frame)));
    | Closed(Let_pat(frame, def, body)) =>
      switch (d) {
      | Left =>
        let let_tile = Tile.Pre(Term_exp.Let(subject, def));
        let body_tiles = Parser_exp.dissociate(body);
        let ((prefix, suffix), frame) = Parser_exp.dissociate_frame(frame);
        let subject = escaped_tile(prefix, let_tile, body_tiles @ suffix);
        Some(EditState_pointing.Exp((subject, frame)));
      | Right =>
        let frame = Frame_exp.Open(Let_def(subject, frame, body));
        let subject = ([], Parser_exp.dissociate(def));
        Some(EditState_pointing.Exp((subject, frame)));
      }
    };
  };

  let select_into_frame = ((selecting, frame): Zipper_pat.selecting) => {
    let ((side, selection), (prefix, suffix)) = selecting;
    switch (frame) {
    | Root => None
    | Closed(closed) =>
      // merge selection elements into single selection
      let (prefix, suffix) =
        (prefix, suffix)
        |> TupleUtil.map2(Selection.map_tile(Parser_pat.unsort));
      let selection = prefix @ selection @ suffix;
      // call assemble_tiles_in_selection
      let selection =
        Parser_unsorted.assemble_tiles_in_selection(
          ~direction=Right,
          selection,
        );
      // call get_whole on assembled selection
      let tiles = Selection.get_whole(selection);
      // convert frame with tiles into tessera
      switch (closed) {
      | Lam_pat(frame, body) =>
        let selected_t = Unsorted.Tessera.Lam(tiles);
        let body_tiles = Parser_exp.dissociate(body);
        let ((prefix, suffix), frame) = Parser_exp.dissociate_frame(frame);
        let (prefix, body_tiles, suffix) =
          TupleUtil.map3(
            List.map(Selection.tile),
            (prefix, body_tiles, suffix),
          );
        let selecting = (
          (side, [Selection.Tessera(selected_t)]),
          (prefix, body_tiles @ suffix),
        );
        Some(EditState_selecting.Exp((selecting, frame)));
      | Let_pat(frame, def, body) =>
        let selected_t = Unsorted.Tessera.Let_eq(tiles);
        let def_tiles = Parser_exp.dissociate(def);
        let body_tiles = Parser_exp.dissociate(body);
        let ((prefix, suffix), frame) = Parser_exp.dissociate_frame(frame);
        let (prefix, def_tiles, body_tiles, suffix) =
          [prefix, def_tiles, body_tiles, suffix]
          |> List.map(List.map(Selection.tile))
          |> ListUtil.take_4;
        let selecting = (
          (side, [Selection.Tessera(selected_t)]),
          (
            prefix,
            def_tiles @ [Selection.Tessera(Let_in)] @ body_tiles @ suffix,
          ),
        );
        Some(EditState_selecting.Exp((selecting, frame)));
      };
    | Open(open_) =>
      let ((ts_before, ts_after), (outer_prefix, outer_suffix), frame) =
        Parser_pat.disassemble_open_frame(open_);
      let (outer_prefix, outer_suffix) =
        TupleUtil.map2(
          List.map(Selection.tile),
          (outer_prefix, outer_suffix),
        );
      switch (side) {
      | Left =>
        // assume prefix empty
        let (tessera, ts_before) = ts_before;
        let ts_before =
          ts_before
          |> AltList.even_to_list(
               open_child =>
                 List.map(Selection.tile, Parser_pat.dissociate(open_child)),
               tessera => [Tessera(tessera)],
             )
          |> List.rev
          |> List.flatten;
        let ts_after =
          ts_after
          |> AltList.odd_to_list(
               tessera => [Selection.Tessera(tessera)],
               open_child =>
                 List.map(Selection.tile, Parser_pat.dissociate(open_child)),
             )
          |> List.flatten;
        let selection = (
          Direction.Left,
          [Selection.Tessera(tessera), ...selection],
        );
        let selecting = (
          selection,
          (ts_before @ outer_prefix, suffix @ ts_after @ outer_suffix),
        );
        Some(EditState_selecting.Pat((selecting, frame)));
      | Right =>
        // assume suffix empty
        let (tessera, ts_after) = ts_after;
        let ts_after =
          ts_after
          |> AltList.even_to_list(
               open_child =>
                 List.map(Selection.tile, Parser_pat.dissociate(open_child)),
               tessera => [Tessera(tessera)],
             )
          |> List.flatten;
        let ts_before =
          ts_before
          |> AltList.odd_to_list(
               tessera => [Selection.Tessera(tessera)],
               open_child =>
                 List.map(Selection.tile, Parser_pat.dissociate(open_child)),
             )
          |> List.rev
          |> List.flatten;
        let selection = (Direction.Right, selection @ [Tessera(tessera)]);
        let selecting = (
          selection,
          (prefix @ ts_before @ outer_prefix, ts_after @ outer_suffix),
        );
        Some(EditState_selecting.Pat((selecting, frame)));
      };
    };
  };
};

include Action_make.Make(
          Term_pat,
          Tile_pat,
          Frame_pat,
          Zipper_pat,
          Parser_pat,
          Input,
        );
