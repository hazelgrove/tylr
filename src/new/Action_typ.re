open Util;
open OptUtil.Syntax;

module Input = {
  let mk_pointing = p => EditState.Typ_p(p);
  let mk_edit_state = z => EditState.Typ(z);
  let append_frame = ((subj, frm), frame) => {
    let+ frame = Frame_typ.bidelimited_append_typ(frm, frame);
    (subj, frame);
  };

  let move_into_root =
      (d: Direction.t, subject: Term_typ.t, frame: Frame_typ.t) => {
    let mk_pointing = tiles =>
      switch (d) {
      | Left => (tiles, (), [])
      | Right => ([], (), tiles)
      };
    subject
    |> Term.get(
         fun
         | Term_typ.OpHole
         | Num
         | Bool => None
         | Paren(body) => {
             let subject = mk_pointing(Parser_typ.dissociate(body));
             let frame = Frame_typ.Paren_body(frame);
             Some(EditState.Typ_p((subject, frame)));
           },
         (((), _)) => raise(Term_typ.Void_pre),
         ((_, ())) => raise(Term_typ.Void_post),
         fun
         | (_, Term_typ.BinHole | Arrow, _) => None,
       );
  };

  let move_into_frame =
      (d: Direction.t, subject: Term_typ.t, frame: Frame_typ.bidelimited) => {
    open Frame_typ;
    let escaped_tile = (prefix, tile, suffix) =>
      switch (d) {
      | Left => (prefix, (), [tile, ...suffix])
      | Right => (prefix @ [tile], (), suffix)
      };
    switch (frame) {
    | Root => None
    | Paren_body(frame) =>
      let tile = Tile.Op(Term_typ.Paren(subject));
      let ((prefix, suffix), frame) = Parser_typ.dissociate_frame(frame);
      let subject = escaped_tile(prefix, tile, suffix);
      Some(EditState.Typ_p((subject, frame)));
    | Ann_ann(p, frame) =>
      let p_tiles = Parser_pat.dissociate(p);
      let ann_tile = Tile.Post(Term_pat.Ann(subject));
      let ((prefix, suffix), frame) = Parser_pat.dissociate_frame(frame);
      let subject = escaped_tile(prefix @ p_tiles, ann_tile, suffix);
      Some(EditState.Pat_p((subject, frame)));
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
