open Util;
open OptUtil.Syntax;

module Input = {
  let mk_zipper = z => Zipper.Exp(z);
  let append_frame = Zipper.append_frame_exp;

  let move_into_root =
      (d: Direction.t, subject: Term_exp.t, frame: Frame_exp.t) => {
    let mk_pointing = tiles =>
      switch (d) {
      | Left => Subject.Pointing(ZList.mk(~prefix=tiles, ~z=(), ()))
      | Right => Pointing(ZList.mk(~z=(), ~suffix=tiles, ()))
      };
    subject
    |> Term.get(
         fun
         | Term_exp.OpHole
         | Num(_)
         | Var(_) => None
         | Paren(body) => {
             let subject = mk_pointing(Parser_exp.dissociate(body));
             let frame = Some(Frame_exp.Paren_body(frame));
             Some(Zipper.Exp((subject, frame)));
           },
         fun
         | (Term_exp.Lam(p), body) => {
             let subject = mk_pointing(Parser_pat.dissociate(p));
             let frame = Some(Frame_pat.Lam_pat(frame, body));
             Some(Zipper.Pat((subject, frame)));
           }
         | (Let(p, def), body) =>
           switch (d) {
           | Left =>
             let subject = mk_pointing(Parser_exp.dissociate(def));
             let frame = Some(Frame_exp.Let_def(p, frame, body));
             Some(Zipper.Exp((subject, frame)));
           | Right =>
             let subject = mk_pointing(Parser_pat.dissociate(p));
             let frame = Some(Frame_pat.Let_pat(frame, def, body));
             Some(Zipper.Pat((subject, frame)));
           },
         fun
         | (_, Term_exp.Ap(_)) => failwith("todo"),
         fun
         | (_, Plus | BinHole, _) => None,
       );
  };

  let move_into_frame =
      (d: Direction.t, subject: Term_exp.t, frame: Frame_exp.bidelimited) => {
    open Term_exp;
    open Frame_exp;
    let mk_pointing = (prefix, tile, suffix) =>
      switch (d) {
      | Left =>
        Subject.Pointing(
          ZList.mk(~prefix, ~z=(), ~suffix=[tile, ...suffix], ()),
        )
      | Right =>
        Pointing(ZList.mk(~prefix=prefix @ [tile], ~z=(), ~suffix, ()))
      };
    switch (frame) {
    | Paren_body(frame) =>
      let tile = Tile.Op(Term_exp.Paren(subject));
      let ((prefix, suffix), frame) = Parser_exp.dissociate_frame(frame);
      let subject = mk_pointing(prefix, tile, suffix);
      Zipper.Exp((subject, frame));
    | Let_def(p, frame, body) =>
      let tile = Tile.Pre(Term_exp.Let(p, subject));
      let ((prefix, suffix), frame) = Parser_exp.dissociate_frame(frame);
      let subject = mk_pointing(prefix, tile, suffix);
      Zipper.Exp((subject, frame));
    | Ap_arg(_) => failwith("ap todo")
    };
  };
};

include Action_make(Term_exp, Tile_exp, Frame_exp, Zipper_exp, Input);
