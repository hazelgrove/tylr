let mk_pointing = (pointing: EditState_pointing.t) => {
  let rec go = (~caret=CaretPosition.Before(0), pointing) =>
    switch (pointing) {
    | Typ(_)
    | Pat(_) => failwith("todo")
    | Exp(((prefix, (), []), frame)) =>
      let subject = Parser_exp.associate(prefix);
      switch (frame) {
      | Root =>
        let (leading, last) = ListUtil.split_last(prefix);
        go(~caret=After, Exp(((leading, (), [last]), frame)));
      | Open(Paren_body(frame)) =>
        let ((prefix, suffix), frame) = Parser_exp.dissociate_frame(frame);
        go(
          ~caret=Before(1),
          Exp(((prefix, (), [Op(Paren(subject)), ...suffix]), frame)),
        );
      | Open(Let_def(p, frame, body)) =>
        let inner_suffix = Parser_exp.dissociate(body);
        let ((prefix, suffix), frame) = Parser_exp.dissociate_frame(frame);
        go(
          ~caret=Before(2),
          Exp((
            (
              prefix,
              (),
              [Tile.Pre(Term_exp.Let(p, subject)), ...inner_suffix] @ suffix,
            ),
            frame,
          )),
        );
      | Open(Ap_arg(_)) => failwith("ap todo")
      | Closed () => raise(Frame_exp.Void_closed)
      };
    | Exp(((prefix, (), [_, ..._] as suffix), frame)) =>
      let (prefix, term, suffix) = {
        let n = List.length(prefix);
        let tiles = prefix @ suffix;
        let skel = Skel.skel_at(n, Parser_exp.mk_skel(tiles));
        Parser_exp.term_of_skel(skel, tiles);
      };
      let frame = Parser_exp.associate_frame((prefix, suffix), frame);
      let l_frame = Layout_exp.mk_frame(frame);
      let info_term = TypeInfo_exp.of_t'(l_frame);
      let l_term =
        Layout_exp.mk_term(~has_caret=Some(caret), info_term, term);
      switch (l_frame.mode) {
      | Syn(l_frame) =>
        let ty = Statics_exp.syn(info_term, term);
        l_frame(ty, l_term);
      | Ana(_, l_frame) => l_frame(l_term)
      | Fn_pos(l_frame) =>
        let (ty_in, ty_out) =
          Option.get(Type.matches_arrow(Statics_exp.syn(info_term, term)));
        l_frame(ty_in, ty_out, l_term);
      };
    };
  go(pointing);
};

let mk = (edit_state: EditState.t) =>
  switch (edit_state) {
  | Typ((Pointing(pointing), frame)) => mk_pointing(Typ((pointing, frame)))
  | Pat((Pointing(pointing), frame)) => mk_pointing(Pat((pointing, frame)))
  | Exp((Pointing(pointing), frame)) => mk_pointing(Exp((pointing, frame)))
  | Typ((Selecting(_) | Restructuring(_), _))
  | Pat((Selecting(_) | Restructuring(_), _))
  | Exp((Selecting(_) | Restructuring(_), _)) => failwith("todo")
  // | Exp((Selecting(selecting), frame)) =>
  };
