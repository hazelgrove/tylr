open Util;

module Input = {
  let mk_pointing = p => EditState_pointing.Typ(p);
  let mk_edit_state = z => EditState.Typ(z);

  let can_enter =
    Term_typ.(
      Tile.get(
        fun
        | OpHole
        | Num
        | Bool => false
        | Paren(_) => true,
        () => raise(Void_pre),
        () => raise(Void_post),
        fun
        | Arrow
        | BinHole => false,
      )
    );

  let move_into_root =
      (d: Direction.t, subject: Term_typ.t, frame: Frame_typ.t) => {
    let mk_pointing = tiles =>
      ListUtil.mk_frame(d == Left ? List.length(tiles) : 0, tiles);
    subject
    |> Term.get(
         fun
         | Term_typ.OpHole
         | Num
         | Bool => None
         | Paren(body) => {
             let subject = mk_pointing(Parser_typ.dissociate(body));
             let frame = Frame_typ.Open(Paren_body(frame));
             Some(EditState_pointing.Typ((subject, frame)));
           },
         fun
         | ((), _) => raise(Term_typ.Void_pre),
         fun
         | (_, ()) => raise(Term_typ.Void_post),
         fun
         | (_, Term_typ.BinHole | Arrow, _) => None,
       );
  };

  let move_into_frame =
      (d: Direction.t, subject: Term_typ.t, frame: Frame_typ.bidelimited) => {
    open Frame_typ;
    let escaped_tile = (prefix, tile, suffix) =>
      switch (d) {
      | Left => (prefix, [tile, ...suffix])
      | Right => ([tile, ...prefix], suffix)
      };
    switch (frame) {
    | Root => None
    | Open(Paren_body(frame)) =>
      let tile = Tile.Op(Term_typ.Paren(subject));
      let ((prefix, suffix), frame) = Parser_typ.dissociate_frame(frame);
      let subject = escaped_tile(prefix, tile, suffix);
      Some(EditState_pointing.Typ((subject, frame)));
    | Closed(Ann_ann(p, frame)) =>
      let p_tiles = Parser_pat.dissociate(p);
      let ann_tile = Tile.Post(Term_pat.Ann(subject));
      let ((prefix, suffix), frame) = Parser_pat.dissociate_frame(frame);
      let subject = escaped_tile(prefix @ p_tiles, ann_tile, suffix);
      Some(EditState_pointing.Pat((subject, frame)));
    };
  };

  let select_into_frame = ((selecting, frame): Zipper_typ.selecting) => {
    let ((side, selection), (prefix, suffix)) = selecting;
    switch (frame) {
    | Root => None
    | Closed(closed) =>
      // merge selection elements into single selection
      let (prefix, suffix) =
        (prefix, suffix)
        |> TupleUtil.map2(Selection.map_tile(Parser_typ.unsort));
      let selection =
        ListUtil.of_frame(~subject=selection, (prefix, suffix));
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
      | Ann_ann(subj, frame) =>
        let selected_t = Unsorted.Tessera.Ann(tiles);
        let subj_tiles = Parser_pat.dissociate(subj);
        let ((prefix, suffix), frame) = Parser_pat.dissociate_frame(frame);
        let (prefix, subj_tiles, suffix) =
          TupleUtil.map3(
            List.map(Selection.tile),
            (prefix, subj_tiles, suffix),
          );
        let selecting = (
          (side, [Selection.Tessera(selected_t)]),
          (subj_tiles @ prefix, suffix),
        );
        Some(EditState_selecting.Pat((selecting, frame)));
      };
    | Open(open_) =>
      let ((ts_before, ts_after), (outer_prefix, outer_suffix), frame) =
        Parser_typ.disassemble_open_frame(open_);
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
                 List.map(Selection.tile, Parser_typ.dissociate(open_child)),
               tessera => [Tessera(tessera)],
             )
          |> List.rev
          |> List.flatten;
        let ts_after =
          ts_after
          |> AltList.odd_to_list(
               tessera => [Selection.Tessera(tessera)],
               open_child =>
                 List.map(Selection.tile, Parser_typ.dissociate(open_child)),
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
        Some(EditState_selecting.Typ((selecting, frame)));
      | Right =>
        // assume suffix empty
        let (tessera, ts_after) = ts_after;
        let ts_after =
          ts_after
          |> AltList.even_to_list(
               open_child =>
                 List.map(Selection.tile, Parser_typ.dissociate(open_child)),
               tessera => [Tessera(tessera)],
             )
          |> List.flatten;
        let ts_before =
          ts_before
          |> AltList.odd_to_list(
               tessera => [Selection.Tessera(tessera)],
               open_child =>
                 List.map(Selection.tile, Parser_typ.dissociate(open_child)),
             )
          |> List.rev
          |> List.flatten;
        let selection = (Direction.Right, selection @ [Tessera(tessera)]);
        let selecting = (
          selection,
          (prefix @ ts_before @ outer_prefix, ts_after @ outer_suffix),
        );
        Some(EditState_selecting.Typ((selecting, frame)));
      };
    };
  };
};

include Action_make.Make(
          Term_typ,
          Tile_typ,
          Frame_typ,
          Zipper_typ,
          Parser_typ,
          Input,
        );
