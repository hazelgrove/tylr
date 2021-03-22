open Util;

module Input = {
  let mk_pointing = p => EditState_pointing.Exp(p);
  let mk_edit_state = z => EditState.Exp(z);

  let move_into_root =
      (d: Direction.t, subject: Term_exp.t, frame: Frame_exp.t) => {
    let mk_pointing = tiles =>
      ListUtil.mk_frame(d == Left ? List.length(tiles) : 0, tiles);
    subject
    |> Term.get(
         fun
         | Term_exp.OpHole
         | Num(_)
         | Var(_) => None
         | Paren(body) => {
             let subject = mk_pointing(Parser_exp.dissociate(body));
             let frame = Frame_exp.Open(Paren_body(frame));
             Some(EditState_pointing.Exp((subject, frame)));
           },
         fun
         | (Term_exp.Lam(p), body) => {
             let subject = mk_pointing(Parser_pat.dissociate(p));
             let frame = Frame_pat.Closed(Lam_pat(frame, body));
             Some(EditState_pointing.Pat((subject, frame)));
           }
         | (Let(p, def), body) =>
           switch (d) {
           | Left =>
             let subject = mk_pointing(Parser_exp.dissociate(def));
             let frame = Frame_exp.Open(Let_def(p, frame, body));
             Some(EditState_pointing.Exp((subject, frame)));
           | Right =>
             let subject = mk_pointing(Parser_pat.dissociate(p));
             let frame = Frame_pat.Closed(Let_pat(frame, def, body));
             Some(EditState_pointing.Pat((subject, frame)));
           },
         fun
         | (_, Term_exp.Ap(_)) => failwith("ap todo"),
         fun
         | (_, Plus | BinHole, _) => None,
       );
  };

  let move_into_frame =
      (d: Direction.t, subject: Term_exp.t, frame: Frame_exp.bidelimited) => {
    open Frame_exp;
    let escaped_tile = (prefix, tile, suffix) =>
      switch (d) {
      | Left => (prefix, [tile, ...suffix])
      | Right => ([tile, ...prefix], suffix)
      };
    switch (frame) {
    | Root => None
    | Closed () => raise(Frame_exp.Void_closed)
    | Open(Paren_body(frame)) =>
      let tile = Tile.Op(Term_exp.Paren(subject));
      let ((prefix, suffix), frame) = Parser_exp.dissociate_frame(frame);
      let subject = escaped_tile(prefix, tile, suffix);
      Some(EditState_pointing.Exp((subject, frame)));
    | Open(Let_def(p, frame, body)) =>
      switch (d) {
      | Left =>
        let frame = Frame_pat.Closed(Let_pat(frame, subject, body));
        let subject = (List.rev(Parser_pat.dissociate(p)), []);
        Some(EditState_pointing.Pat((subject, frame)));
      | Right =>
        let let_tile = Tile.Pre(Term_exp.Let(p, subject));
        let body_tiles = Parser_exp.dissociate(body);
        let ((prefix, suffix), frame) = Parser_exp.dissociate_frame(frame);
        let subject = escaped_tile(prefix, let_tile, body_tiles @ suffix);
        Some(EditState_pointing.Exp((subject, frame)));
      }
    | Open(Ap_arg(_)) => failwith("ap todo")
    };
  };

  let select_into_frame = ((selecting, frame): Zipper_exp.selecting) => {
    let ((side, selection), (prefix, suffix)) = selecting;
    switch (frame) {
    | Root => None
    | Closed () => raise(Frame_exp.Void_closed)
    | Open(open_) =>
      // TODO abstract open logic into Action_make
      let ((ts_before, ts_after), (outer_prefix, outer_suffix), frame) =
        Parser_exp.disassemble_open_frame(open_);
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
                 List.map(Selection.tile, Parser_exp.dissociate(open_child)),
               tessera => [Tessera(tessera)],
             )
          |> List.rev
          |> List.flatten;
        let ts_after =
          ts_after
          |> AltList.odd_to_list(
               tessera => [Selection.Tessera(tessera)],
               open_child =>
                 List.map(Selection.tile, Parser_exp.dissociate(open_child)),
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
        Some(EditState_selecting.Exp((selecting, frame)));
      | Right =>
        // assume suffix empty
        let (tessera, ts_after) = ts_after;
        let ts_after =
          ts_after
          |> AltList.even_to_list(
               open_child =>
                 List.map(Selection.tile, Parser_exp.dissociate(open_child)),
               tessera => [Tessera(tessera)],
             )
          |> List.flatten;
        let ts_before =
          ts_before
          |> AltList.odd_to_list(
               tessera => [Selection.Tessera(tessera)],
               open_child =>
                 List.map(Selection.tile, Parser_exp.dissociate(open_child)),
             )
          |> List.rev
          |> List.flatten;
        let selection = (Direction.Right, selection @ [Tessera(tessera)]);
        let selecting = (
          selection,
          (prefix @ ts_before @ outer_prefix, ts_after @ outer_suffix),
        );
        Some(EditState_selecting.Exp((selecting, frame)));
      };
    };
  };
};

include Action_make.Make(
          Term_exp,
          Tile_exp,
          Frame_exp,
          Zipper_exp,
          Parser_exp,
          Input,
        );
