open Util;
open OptUtil.Syntax;

module Input = {
  let mk_pointing = p => EditState.Exp_p(p);
  let mk_edit_state = z => EditState.Exp(z);
  let append_frame = ((subj, frm), frame) => {
    let+ frame = Frame_exp.bidelimited_append_exp(frm, frame);
    (subj, frame);
  };

  let move_into_root =
      (d: Direction.t, subject: Term_exp.t, frame: Frame_exp.t) => {
    let mk_pointing = tiles =>
      switch (d) {
      | Left => (tiles, (), [])
      | Right => ([], (), tiles)
      };
    subject
    |> Term.get(
         fun
         | Term_exp.OpHole
         | Num(_)
         | Var(_) => None
         | Paren(body) => {
             let subject = mk_pointing(Parser_exp.dissociate(body));
             let frame = Frame_exp.Paren_body(frame);
             Some(EditState.Exp_p((subject, frame)));
           },
         fun
         | (Term_exp.Lam(p), body) => {
             let subject = mk_pointing(Parser_pat.dissociate(p));
             let frame = Frame_pat.Lam_pat(frame, body);
             Some(EditState.Pat_p((subject, frame)));
           }
         | (Let(p, def), body) =>
           switch (d) {
           | Left =>
             let subject = mk_pointing(Parser_exp.dissociate(def));
             let frame = Frame_exp.Let_def(p, frame, body);
             Some(EditState.Exp_p((subject, frame)));
           | Right =>
             let subject = mk_pointing(Parser_pat.dissociate(p));
             let frame = Frame_pat.Let_pat(frame, def, body);
             Some(EditState.Pat_p((subject, frame)));
           },
         fun
         | (_, Term_exp.Ap(_)) => failwith("todo"),
         fun
         | (_, Plus | BinHole, _) => None,
       );
  };

  let move_into_frame =
      (d: Direction.t, subject: Term_exp.t, frame: Frame_exp.bidelimited) => {
    open Frame_exp; // open Term_exp;

    // TODO rename to something like escaped_tile
    let mk_pointing = (prefix, tile, suffix) =>
      switch (d) {
      | Left => (prefix, (), [tile, ...suffix])
      | Right => (prefix @ [tile], (), suffix)
      };
    switch (frame) {
    | Root => None
    | Paren_body(frame) =>
      let tile = Tile.Op(Term_exp.Paren(subject));
      let ((prefix, suffix), frame) = Parser_exp.dissociate_frame(frame);
      let subject = mk_pointing(prefix, tile, suffix);
      Some(EditState.Exp_p((subject, frame)));
    | Let_def(_p, _frame, _body) =>
      /*
       switch (d) {
       | Right =>
         let tile = Tile.Pre(Term_exp.Let(p, subject));
         let ((prefix, suffix), frame) = Parser_exp.dissociate_frame(frame);
         let subject = mk_pointing(prefix, tile, suffix);
         Some(EditState.Exp_p((subject, frame)));
       }
       */
      failwith("todo")
    | Ap_arg(_) => failwith("ap todo")
    };
  };

  let assemble_open_bidelimited_frame = (~associate as _, _, _e) =>
    failwith("todo");
};

include Action_make.Make(
          Term_exp,
          Tile_exp,
          Frame_exp,
          Zipper_exp,
          Parser_exp,
          Input,
        );
