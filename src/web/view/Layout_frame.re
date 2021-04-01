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
        |> TypeInfo_exp.map_mode'((l_frame, l_body) =>
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
           l_frame(
             cats([l_l, empty_hole(~sort=Exp, fst(mk_BinHole())), l_r]),
           );
         });
    | Bin_r(l, BinHole, frame) =>
      let info = mk(~show_err_holes, frame);
      info
      |> TypeInfo_exp.binhole_r'((l_frame, l_r) => {
           let l_l =
             Layout_term.Exp.mk({ctx: info.ctx, mode: TypeInfo_exp.syn}, l);
           l_frame(
             cats([l_l, empty_hole(~sort=Exp, fst(mk_BinHole())), l_r]),
           );
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
          TypeInfo_exp.map_mode'(
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
          l_frame(
            cats([l_l, empty_hole(~sort=Pat, fst(mk_BinHole())), l_r]),
          );
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
          l_frame(
            cats([l_l, empty_hole(~sort=Pat, fst(mk_BinHole())), l_r]),
          );
        },
        l,
        info,
      );
    }
  and mk_bi = (~show_err_holes, bi: Frame_pat.bidelimited) =>
    switch (bi) {
    | Root => TypeInfo_pat.root'((_, _, l) => l)
    | Open(Paren_body(frame)) =>
      let info = mk(~show_err_holes, frame);
      {
        ...info,
        mode:
          TypeInfo_pat.map_mode'(
            (l_frame, l_body) => l_frame(grouts([fst(mk_Paren(l_body))])),
            info.mode,
          ),
      };
    | Closed(Let_pat(frame, def, body)) =>
      let info = Exp.mk(~show_err_holes, frame);
      TypeInfo_exp.let_pat'(
        (ty_p, ctx_body, l_frame, l_p) => {
          let l_def =
            Layout_term.Exp.mk(
              {ctx: info.ctx, mode: TypeInfo_exp.ana(ty_p)},
              def,
            );
          let l_body =
            Layout_term.Exp.mk(
              {...TypeInfo_exp.of_t'(info), ctx: ctx_body},
              body,
            );
          l_frame(cat(grouts_l([fst(mk_Let(l_p, l_def))]), l_body));
        },
        def,
        body,
        info,
      );
    | Closed(Lam_pat(frame, body)) =>
      let info = Exp.mk(~show_err_holes, frame);
      TypeInfo_exp.lam_pat'(
        (ctx_body, l_frame, l_p) => {
          let l_body =
            Layout_term.Exp.mk(
              {ctx: ctx_body, mode: TypeInfo_exp.lam_body_mode(info.mode)},
              body,
            );
          let l_lam = cat(grouts_l([fst(mk_Lam(l_p))]), l_body);
          let l_lam =
            TypeInfo_exp.lam_has_err(info)
              ? l_lam : Annot(ErrHole(true), l_lam);
          l_frame(l_lam);
        },
        body,
        info,
      );
    };
};

module Typ = {
  let rec mk = (~show_err_holes, frame: Frame_typ.t): (Type.t => Layout.frame) =>
    switch (frame) {
    | Uni(uni) => mk_uni(~show_err_holes, uni)
    | Bi(bi) => mk_bi(~show_err_holes, bi)
    }
  and mk_uni = (~show_err_holes, uni: Frame_typ.unidelimited) =>
    switch (uni) {
    | Pre_r((), _) => raise(Term_typ.Void_pre)
    | Post_l(_, ()) => raise(Term_typ.Void_post)
    | Bin_l(frame, bin, r) =>
      let l_frame = mk(~show_err_holes, frame);
      let l_bin =
        switch (bin) {
        | Arrow => fst(mk_Arrow())
        | BinHole => empty_hole(~sort=Typ, fst(mk_BinHole()))
        };
      let l_r = Layout_term.Typ.mk(r);
      let ty = ty_l =>
        switch (bin) {
        | Arrow => Type.Arrow(ty_l, Term_typ.to_type(r))
        | BinHole => Hole
        };
      ((ty_l, l_l) => l_frame(ty(ty_l), cats([l_l, l_bin, l_r])));
    | Bin_r(l, bin, frame) =>
      let l_frame = mk(~show_err_holes, frame);
      let l_l = Layout_term.Typ.mk(l);
      let l_bin =
        switch (bin) {
        | Arrow => fst(mk_Arrow())
        | BinHole => empty_hole(~sort=Typ, fst(mk_BinHole()))
        };
      let ty = ty_r =>
        switch (bin) {
        | Arrow => Type.Arrow(Term_typ.to_type(l), ty_r)
        | BinHole => Hole
        };
      ((ty_r, l_r) => l_frame(ty(ty_r), cats([l_l, l_bin, l_r])));
    }
  and mk_bi = (~show_err_holes, bi: Frame_typ.bidelimited) =>
    switch (bi) {
    | Root => ((_, l) => l)
    | Open(Paren_body(frame)) =>
      let l_frame = mk(~show_err_holes, frame);
      (
        (ty_body, l_body) =>
          l_frame(ty_body, grouts([fst(mk_Paren(l_body))]))
      );
    | Closed(Ann_ann(subj, frame)) =>
      let info = Pat.mk(~show_err_holes, frame);
      TypeInfo_pat.ann_ann'(
        (ty_ann, l_frame: Layout.frame, l_ann: t) => {
          let info_subj = TypeInfo_pat.{ctx: info.ctx, mode: ana(ty_ann)};
          let l_subj = Layout_term.Pat.mk(info_subj, subj);
          err_hole(
            TypeInfo_pat.ann_has_err(ty_ann, info),
            true,
            l_frame(cat(l_subj, grouts_r([fst(mk_Ann(l_ann))]))),
          );
        },
        subj,
        info,
      );
    };
};
