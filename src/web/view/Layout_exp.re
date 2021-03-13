open Layout;

let rec mk_frame =
        (~show_err_holes: bool, frame: Frame_exp.t)
        : TypeInfo_exp.t(Layout.frame) => {
  let rec go = (frame: Frame_exp.t) =>
    switch (frame) {
    | Uni(uni) => mk_unidelimited(uni)
    | Bi(bi) => mk_bidelimited(bi)
    }
  and mk_unidelimited = (uni: Frame_exp.unidelimited) =>
    switch (uni) {
    | Pre_r(Lam(p), frame) =>
      let info = mk_frame(frame);
      let info_p = TypeInfo_exp.lam_pat(info);
      let l_p = Layout_pat.mk(info_p, p);
      let (ty_p, ctx_body) = Statics_pat.syn(info_p, p);
      let mode_body: TypeInfo_exp.mode(_) = {
        let l_lam = l_body => sep(mk_Lam(l_p), l_body);
        switch (info.mode) {
        | Syn(l_frame) =>
          Syn((ty_body, l_body) => l_frame(ty_body, l_lam(l_body)))
        | Fn_pos(l_frame) =>
          Syn((ty_body, l_body) => l_frame(ty_p, ty_body, l_lam(l_body)))
        | Ana(ty, l_frame) =>
          switch (Type.matches_arrow(ty)) {
          | None =>
            Syn(
              (_ty_body, l_body) => l_frame(Annot(ErrHole, l_lam(l_body))),
            )
          | Some((_, ty_out)) => Ana(ty_out, l_frame)
          }
        };
      };
      {ctx: ctx_body, mode: mode_body};
    | Pre_r(Let(p, def), frame) =>
      let info = mk_frame(frame);
      let info_p = TypeInfo_pat.{ctx: info.ctx, mode: Syn()};
      let l_p = Layout_pat.mk_term(info_p, p);
      let l_def = {
        let (p_ty, _) = Statics_pat.syn(info_p, p);
        let info_def = TypeInfo_exp.{ctx: info.ctx, mode: Ana(p_ty, ())};
        mk_term(info_def, def);
      };
      let ctx_body = Statics_exp.extend_ctx_let_body(info.ctx, p, def);
      let mode_body =
        info.mode
        |> TypeInfo_exp.map_mode(
             (l_frame, l_subject) =>
               l_frame(sep(mk_Let(l_p, l_def), l_subject)),
             {ctx: ctx_body, mode: mode_body},
           );
      ();
    | Post_r(_, Ap(_)) => failwith("ap todo")
    | Bin_l(frame, Plus, r) =>
      let info = mk_frame(frame);
      let l_r = mk_term({ctx: info.ctx, mode: Ana(Num, ())}, r);
      let l_plus = l_l => sep(l_l, mk_Plus(), l_r);
      let mode_l =
        switch (info.mode) {
        | Syn(l_frame) => Ana(Num, l_l => l_frame(Num, l_plus(l_l)))
        | Ana(ty, l_frame) =>
          Ana(
            Num,
            l_l =>
              err_hole(
                show_err_holes && !Type.consistent(ty, Num),
                l_frame(l_plus(l_l)),
              ),
          )
        | Fn_pos(l_frame) =>
          Ana(
            Num,
            l_l =>
              err_hole(show_err_holes, l_frame(Hole, Hole, l_plus(l_l))),
          )
        };
      {...info, mode: mode_l};
    | Bin_r(l, Plus, frame) =>
      let info = mk_frame(frame);
      let l_l = mk_term({ctx: info.ctx, mode: Ana(Num, ())}, l);
      let l_plus = l_r => sep(l_l, mk_Plus(), l_r);
      let mode_r =
        switch (info.mode) {
        | Syn(l_frame) => Ana(Num, l_r => l_frame(Num, l_plus(l_r)))
        | Ana(ty, l_frame) =>
          Ana(
            Num,
            l_r =>
              err_hole(
                show_err_holes && !Type.consistent(ty, Num),
                l_frame(l_plus(l_r)),
              ),
          )
        | Fn_pos(l_frame) =>
          Ana(
            Num,
            l_r =>
              err_hole(show_err_holes, l_frame(Hole, Hole, l_plus(l_r))),
          )
        };
      {...info, mode: mode_r};
    | Bin_l(_, BinHole, _)
    | Bin_r(_, BinHole, _) => failwith("binhole todo")
    };
  go(frame);
};

let rec mk_term =
        (
          ~has_caret: option(CaretPosition.t)=?,
          info: TypeInfo_exp.t,
          e: Term_exp.t,
        )
        : Layout.t =>
  e
  |> Term.get(
       fun
       | OpHole => Text(Unicode.nbsp)
       | Var(x) => Text(x)
       | Num(n) => Text(string_of_int(n))
       | Paren(body) => mk_Paren(mk_term(info, body)),
       fun
       | (Lam(p), body) => {
           let has_err = TypeInfo_exp.lam_has_err(info);
           let l_p = Layout_pat.mk_term(TypeInfo_exp.lam_pat(info), p);
           let l_body = mk_term(TypeInfo_exp.lam_body(info, p), body);
           err_hole(has_err, sep(mk_Lam(l_p), l_body));
         }
       | (Let(p, def), body) => {
           let l_p = Layout_pat.mk_term(TypeInfo_exp.let_pat(info));
           let l_def = mk_term(TypeInfo_exp.let_def(info, p), def);
           let l_body = mk_term(TypeInfo_exp.let_body(info, p, def), body);
           sep(mk_Let(l_p, l_def), l_body);
         },
       fun
       | (_, Ap(_)) => failwith("ap todo"),
       fun
       | (l, Plus, r) => {
           let has_err = TypeInfo_exp.plus_has_err(info);
           let l_l = mk_term(TypeInfo_exp.plus_l(info), l);
           let l_r = mk_term(TypeInfo_exp.plus_r(info), r);
           err_hole(has_err, seps([l_l, mk_Plus(), l_r]));
         }
       | (l, BinHole, r) => {
           let l_l = mk_term(TypeInfo_exp.binhole_l(info), l);
           let l_r = mk_term(TypeInfo_exp.binhole_r(info), r);
           seps([l_l, mk_BinHole(), l_r]);
         },
     );
