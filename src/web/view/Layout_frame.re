open Core;
open Layout;

module Exp = {
  let rec mk =
          (~show_err_holes: bool, frame: Frame_exp.t)
          : TypeInfo_exp.t'(Layout.frame) =>
    switch (frame) {
    | Uni(uni) => mk_uni(~show_err_holes, uni)
    | Bi(bi) => mk_bi(~show_err_holes, bi)
    }
  and mk_uni = (~show_err_holes, uni: Frame_exp.unidelimited) =>
    switch (uni) {
    | Pre_r(Lam(p), frame) =>
      let info_lam = mk(~show_err_holes, frame);
      TypeInfo_exp.lam_body'(
        (l_frame, l_body) => {
          let info_p = TypeInfo_exp.(lam_pat(of_t'(info_lam)));
          let l_p = Layout_term.Pat.mk(info_p, p);
          let l_lam = cat(grouts_l([fst(mk_Lam(l_p))]), l_body);
          let l_lam =
            TypeInfo_exp.lam_has_err(info_lam)
              ? l_lam : Annot(ErrHole(true), l_lam);
          l_frame(l_lam);
        },
        p,
        info_lam,
      );
    | Pre_r(Let(p, def), frame) =>
      let info = mk(~show_err_holes, frame);
      let info_p = TypeInfo_pat.{ctx: info.ctx, mode: syn};
      let l_p = Layout_term.Pat.mk(info_p, p);
      let l_def = {
        let (p_ty, _) = TypeInfo_pat.synthesize(info_p, p);
        let info_def =
          TypeInfo_exp.{ctx: info.ctx, mode: Ana(p_ty, _ => ())};
        Layout_term.Exp.mk(info_def, def);
      };
      let ctx_body = TypeInfo_exp.extend_ctx_let_body(p, def, info.ctx);
      let mode_body =
        info.mode
        |> TypeInfo_exp.map_mode((l_frame, l_body) =>
             l_frame(cat(grouts_l([fst(mk_Let(l_p, l_def))]), l_body))
           );
      {ctx: ctx_body, mode: mode_body};
    | Post_l(_, Ap(_)) => failwith("ap todo")

    | Bin_l(frame, Plus, r) =>
      let info = mk(~show_err_holes, frame);
      let has_err = TypeInfo_exp.plus_has_err(info);
      info
      |> TypeInfo_exp.plus_l'((l_frame, l_l) => {
           let l_r =
             Layout_term.Exp.mk(
               {ctx: info.ctx, mode: Ana(Num, _ => ())},
               r,
             );
           let l_plus =
             err_hole(
               show_err_holes && has_err,
               true,
               cats([l_l, fst(mk_Plus()), l_r]),
             );
           l_frame(l_plus);
         });
    | Bin_r(l, Plus, frame) =>
      let info = mk(~show_err_holes, frame);
      let has_err = TypeInfo_exp.plus_has_err(info);
      info
      |> TypeInfo_exp.plus_r'((l_frame, l_r) => {
           let l_l =
             Layout_term.Exp.mk(
               {ctx: info.ctx, mode: Ana(Num, _ => ())},
               l,
             );
           let l_plus =
             err_hole(
               show_err_holes && has_err,
               true,
               cats([l_l, fst(mk_Plus()), l_r]),
             );
           l_frame(l_plus);
         });

    | Bin_l(frame, BinHole, r) =>
      let info = mk(~show_err_holes, frame);
      info
      |> TypeInfo_exp.binhole_l'((l_frame, l_l) => {
           let l_r =
             Layout_term.Exp.mk({ctx: info.ctx, mode: TypeInfo_exp.syn}, r);
           l_frame(cats([l_l, empty_hole(fst(mk_BinHole())), l_r]));
         });
    | Bin_r(l, BinHole, frame) =>
      let info = mk(~show_err_holes, frame);
      info
      |> TypeInfo_exp.binhole_r'((l_frame, l_r) => {
           let l_l =
             Layout_term.Exp.mk({ctx: info.ctx, mode: TypeInfo_exp.syn}, l);
           l_frame(cats([l_l, empty_hole(fst(mk_BinHole())), l_r]));
         });
    }
  and mk_bi = (~show_err_holes, bi: Frame_exp.bidelimited) =>
    switch (bi) {
    | Root => TypeInfo_exp.root'((_, l) => l)
    | Closed () => raise(Frame_exp.Void_closed)
    | Open(Paren_body(frame)) =>
      let info = mk(~show_err_holes, frame);
      {
        ...info,
        mode:
          TypeInfo_exp.map_mode(
            (l_frame, l_body) => l_frame(grouts([fst(mk_Paren(l_body))])),
            info.mode,
          ),
      };
    | Open(Let_def(p, frame, body)) =>
      let info: TypeInfo_exp.t'(Layout.frame) = mk(~show_err_holes, frame);
      let l_p = Layout_term.Pat.mk(TypeInfo_exp.let_pat(info), p);
      TypeInfo_exp.let_def'(
        (info_body, l_frame, l_def) => {
          let l_body = Layout_term.Exp.mk(info_body, body);
          l_frame(cat(grouts_l([fst(mk_Let(l_p, l_def))]), l_body));
        },
        p,
        body,
        info,
      );
    | Open(Ap_arg(_)) => failwith("ap todo")
    };
};

module Pat = {
  let rec mk =
          (~show_err_holes: bool, frame: Frame_pat.t)
          : TypeInfo_pat.t'(Layout.frame) =>
    switch (frame) {
    | Uni(uni) => mk_uni(~show_err_holes, uni)
    | Bi(bi) => mk_bi(~show_err_holes, bi)
    }
  and mk_uni = (~show_err_holes, uni: Frame_pat.unidelimited) =>
    switch (uni) {
    | Pre_r((), _) => raise(Term_pat.Void_pre)
    | Post_l(frame, Ann(ann)) =>
      let info = mk(~show_err_holes, frame);
      let l_ann = Layout_term.Typ.mk(ann);
      TypeInfo_pat.ann_subj'(
        (l_frame, l_subj) =>
          l_frame(cat(l_subj, grouts_r([fst(mk_Ann(l_ann))]))),
        ann,
        info,
      );
    | Bin_l(frame, BinHole, r) =>
      let info = mk(~show_err_holes, frame);
      TypeInfo_pat.binhole_l'(
        (ctx_l, l_frame, l_l) => {
          let l_r =
            Layout_term.Pat.mk({ctx: ctx_l, mode: TypeInfo_pat.syn}, r);
          l_frame(cats([l_l, empty_hole(fst(mk_BinHole())), l_r]));
        },
        r,
        info,
      );
    | Bin_r(l, BinHole, frame) =>
      let info = mk(~show_err_holes, frame);
      let (_, ctx_l) =
        TypeInfo_pat.synthesize({ctx: info.ctx, mode: TypeInfo_pat.syn}, l);
      TypeInfo_pat.binhole_r'(
        (l_frame, l_r) => {
          let l_l =
            Layout_term.Pat.mk({ctx: ctx_l, mode: TypeInfo_pat.syn}, l);
          l_frame(cats([l_l, empty_hole(fst(mk_BinHole())), l_r]));
        },
        l,
        info,
      );
    }
  and mk_bi = (~show_err_holes as _, bi: Frame_pat.bidelimited) =>
    switch (bi) {
    | Root => TypeInfo_pat.root'((_, _, l) => l)
    // | Closed(Let_pat(frame, def, body)) =>
    | _ => failwith("todo")
    };
};
