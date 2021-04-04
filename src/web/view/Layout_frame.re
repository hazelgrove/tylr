open Core;
open Layout;

module Exp = {
  let rec mk =
          (~show_err_holes: bool, frame: Frame_exp.t)
          : (TypeInfo_exp.t, Type.t => Layout.frame) =>
    switch (frame) {
    | Uni(uni) => mk_uni(~show_err_holes, uni)
    | Bi(bi) => mk_bi(~show_err_holes, bi)
    }
  and mk_uni = (~show_err_holes, uni: Frame_exp.unidelimited) => {
    let err_hole = (has_err, l) =>
      err_hole(has_err && show_err_holes, true, l);
    switch (uni) {
    | Pre_r(Lam(p), frame) =>
      let (info_lam, l_frame) = mk(~show_err_holes, frame);
      let info_p = TypeInfo_exp.lam_pat(info_lam);
      let info_body = TypeInfo_exp.lam_body(p, info_lam);
      let l_frame = (ty_body, l_body) => {
        let l_lam = {
          let l_p = Layout_term.Pat.mk(info_p, p);
          err_hole(
            TypeInfo_exp.lam_has_err(info_lam),
            cat(grouts_l([fst(mk_Lam(l_p))]), l_body),
          );
        };
        let ty_lam = {
          let (ty_p, _) = TypeInfo_pat.synthesize(info_p, p);
          TypeInfo_exp.subsume(info_lam, Arrow(ty_p, ty_body));
        };
        l_frame(ty_lam, l_lam);
      };
      (info_body, l_frame);
    | Pre_r(Let(p, def), frame) =>
      let (info_let, l_frame) = mk(~show_err_holes, frame);
      let info_p = TypeInfo_exp.let_pat(def, info_let);
      let info_def = {
        let (ty_p, _) = TypeInfo_pat.synthesize(info_p, p);
        TypeInfo_exp.{ctx: info_let.ctx, mode: Ana(ty_p)};
      };
      let info_body =
        TypeInfo_exp.{
          mode: info_let.mode,
          ctx: TypeInfo_exp.extend_ctx_let_body(p, def, info_let.ctx),
        };
      let l_frame = (ty_body, l_body) => {
        let l_let = {
          let l_p = Layout_term.Pat.mk(info_p, p);
          let l_def = Layout_term.Exp.mk(info_def, def);
          cat(grouts_l([fst(mk_Let(l_p, l_def))]), l_body);
        };
        l_frame(ty_body, l_let);
      };
      (info_body, l_frame);

    | Post_l(_, Ap(_)) => failwith("ap todo")

    | Bin_l(frame, Plus, r) =>
      let (info_plus, l_frame) = mk(~show_err_holes, frame);
      let info_r = TypeInfo_exp.plus_r(info_plus);
      let l_frame = (_ty_l, l_l) => {
        let l_plus = {
          let l_r = Layout_term.Exp.mk(info_r, r);
          err_hole(
            TypeInfo_exp.plus_has_err(info_plus),
            cats([l_l, fst(mk_Plus()), l_r]),
          );
        };
        let ty_plus = TypeInfo_exp.subsume(info_plus, Num);
        l_frame(ty_plus, l_plus);
      };
      (info_plus, l_frame);
    | Bin_r(l, Plus, frame) =>
      let (info_plus, l_frame) = mk(~show_err_holes, frame);
      let info_l = TypeInfo_exp.plus_l(info_plus);
      let l_frame = (_ty_r, l_r) => {
        let l_plus = {
          let l_l = Layout_term.Exp.mk(info_l, l);
          err_hole(
            TypeInfo_exp.plus_has_err(info_plus),
            cats([l_l, fst(mk_Plus()), l_r]),
          );
        };
        let ty_plus = TypeInfo_exp.subsume(info_plus, Num);
        l_frame(ty_plus, l_plus);
      };
      (info_plus, l_frame);

    | Bin_l(frame, BinHole, r) =>
      let (info_binhole, l_frame) = mk(~show_err_holes, frame);
      let info_r = TypeInfo_exp.binhole_r(info_binhole);
      let l_frame = (_ty_l, l_l) => {
        let l_binhole = {
          let l_r = Layout_term.Exp.mk(info_r, r);
          cats([l_l, empty_hole(~sort=Exp, fst(mk_BinHole())), l_r]);
        };
        l_frame(Hole, l_binhole);
      };
      (info_binhole, l_frame);
    | Bin_r(l, BinHole, frame) =>
      let (info_binhole, l_frame) = mk(~show_err_holes, frame);
      let info_l = TypeInfo_exp.binhole_l(info_binhole);
      let l_frame = (_ty_r, l_r) => {
        let l_binhole = {
          let l_l = Layout_term.Exp.mk(info_l, l);
          cats([l_l, empty_hole(~sort=Exp, fst(mk_BinHole())), l_r]);
        };
        l_frame(Hole, l_binhole);
      };
      (info_binhole, l_frame);

    | Bin_l(_, Cond(_), _)
    | Bin_r(_, Cond(_), _) => failwith("cond todo")
    };
  }
  and mk_bi = (~show_err_holes, bi: Frame_exp.bidelimited) =>
    switch (bi) {
    | Root => (TypeInfo_exp.root, ((_, l) => l))
    | Closed () => raise(Frame_exp.Void_closed)
    | Open(Paren_body(frame)) =>
      let (info_paren, l_frame) = mk(~show_err_holes, frame);
      let l_frame = (ty_body, l_body) =>
        l_frame(ty_body, grouts([fst(mk_Paren(l_body))]));
      (info_paren, l_frame);
    | Open(Let_def(p, frame, body)) =>
      let (info_let, l_frame) = mk(~show_err_holes, frame);
      let info_def = TypeInfo_exp.let_def(p, info_let);
      let l_frame = (ty_def, l_def) => {
        // TODO internalize in TypeInfo_exp
        let info_body = {
          let (_, ctx_body) = TypeInfo_pat.synthesize(Ana(ty_def), p);
          TypeInfo_exp.{ctx: ctx_body, mode: info_let.mode};
        };
        let ty_body = TypeInfo_exp.synthesize(info_body, body);
        let l_let = {
          // TODO inconsistent with Let_pat elsewhere
          // but is equivalent here
          let l_p = Layout_term.Pat.mk(Syn, p);
          let l_body = Layout_term.Exp.mk(info_body, body);
          cat(grouts_l([fst(mk_Let(l_p, l_def))]), l_body);
        };
        l_frame(ty_body, l_let);
      };
      (info_def, l_frame);

    | Open(Cond_then(_)) => failwith("cond todo")
    | Open(Ap_arg(_)) => failwith("ap todo")
    };
};

module Pat = {
  let rec mk =
          (~show_err_holes: bool, frame: Frame_pat.t)
          : (TypeInfo_pat.t, (Type.t, Ctx.t) => Layout.frame) =>
    switch (frame) {
    | Uni(uni) => mk_uni(~show_err_holes, uni)
    | Bi(bi) => mk_bi(~show_err_holes, bi)
    }
  and mk_uni = (~show_err_holes, uni: Frame_pat.unidelimited) =>
    switch (uni) {
    | Pre_r((), _) => raise(Term_pat.Void_pre)

    | Post_l(frame, Ann(ann)) =>
      let ty_ann = Term_typ.to_type(ann);
      let (_, l_frame) = mk(~show_err_holes, frame);
      let l_frame = (_ty_subj, ctx_subj, l_subj) => {
        let l_ann = {
          let l_ann = Layout_term.Typ.mk(ann);
          cat(l_subj, grouts_r([fst(mk_Ann(l_ann))]));
        };
        l_frame(ty_ann, ctx_subj, l_ann);
      };
      (Ana(ty_ann), l_frame);

    | Bin_l(frame, BinHole, r) =>
      let (_, l_frame) = mk(~show_err_holes, frame);
      let info_r = TypeInfo_pat.Syn;
      let (_ty_r, ctx_r) = TypeInfo_pat.synthesize(info_r, r);
      let l_frame = (_ty_l, ctx_l, l_l) => {
        let l_r = Layout_term.Pat.mk(info_r, r);
        let l_binhole =
          cats([l_l, empty_hole(~sort=Pat, fst(mk_BinHole())), l_r]);
        l_frame(Hole, Ctx.union(ctx_l, ctx_r), l_binhole);
      };
      (Syn, l_frame);
    | Bin_r(l, BinHole, frame) =>
      let (_, l_frame) = mk(~show_err_holes, frame);
      let info_l = TypeInfo_pat.Syn;
      let (_ty_l, ctx_l) = TypeInfo_pat.synthesize(info_l, l);
      let l_frame = (_ty_r, ctx_r, l_r) => {
        let l_l = Layout_term.Pat.mk(info_l, l);
        let l_binhole =
          cats([l_l, empty_hole(~sort=Pat, fst(mk_BinHole())), l_r]);
        l_frame(Hole, Ctx.union(ctx_l, ctx_r), l_binhole);
      };
      (Syn, l_frame);
    }
  and mk_bi = (~show_err_holes, bi: Frame_pat.bidelimited) => {
    let err_hole = (has_err, l) =>
      err_hole(has_err && show_err_holes, true, l);
    switch (bi) {
    | Root => (TypeInfo_pat.Syn, ((_, _, l) => l))

    | Open(Paren_body(frame)) =>
      let (info, l_frame) = mk(~show_err_holes, frame);
      let l_frame = (ty_body, ctx_body, l_body) => {
        let l_paren = grouts([fst(mk_Paren(l_body))]);
        l_frame(ty_body, ctx_body, l_paren);
      };
      (info, l_frame);

    | Closed(Let_pat(frame, def, body)) =>
      let (info_let, l_frame) = Exp.mk(~show_err_holes, frame);
      let l_frame = (ty_p, ctx_body, l_p) => {
        let info_def = TypeInfo_exp.{ctx: info_let.ctx, mode: Ana(ty_p)};
        let info_body = TypeInfo_exp.{ctx: ctx_body, mode: info_let.mode};
        let ty_body = TypeInfo_exp.synthesize(info_body, body);
        let l_let = {
          let l_def = Layout_term.Exp.mk(info_def, def);
          let l_body = Layout_term.Exp.mk(info_body, body);
          cat(grouts_l([fst(mk_Let(l_p, l_def))]), l_body);
        };
        l_frame(ty_body, l_let);
      };
      (TypeInfo_exp.let_pat(def, info_let), l_frame);

    | Closed(Lam_pat(frame, body)) =>
      let (info_lam, l_frame) = Exp.mk(~show_err_holes, frame);
      let l_frame = (ty_p, ctx_body, l_p) => {
        let info_body =
          TypeInfo_exp.{ctx: ctx_body, mode: lam_body_mode(info_lam.mode)};
        let ty_body = TypeInfo_exp.synthesize(info_body, body);
        let l_lam = {
          let l_body = Layout_term.Exp.mk(info_body, body);
          err_hole(
            TypeInfo_exp.lam_has_err(info_lam),
            cat(grouts_l([fst(mk_Lam(l_p))]), l_body),
          );
        };
        l_frame(Arrow(ty_p, ty_body), l_lam);
      };
      (TypeInfo_exp.lam_pat(info_lam), l_frame);
    };
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
  and mk_bi = (~show_err_holes, bi: Frame_typ.bidelimited) => {
    let err_hole = (has_err, l) =>
      err_hole(has_err && show_err_holes, true, l);
    switch (bi) {
    | Root => ((_, l) => l)
    | Open(Paren_body(frame)) =>
      let l_frame = mk(~show_err_holes, frame);
      (
        (ty_body, l_body) =>
          l_frame(ty_body, grouts([fst(mk_Paren(l_body))]))
      );
    | Closed(Ann_ann(subj, frame)) =>
      let (info_ann, l_frame) = Pat.mk(~show_err_holes, frame);
      (
        (ty_ann, l_ann) => {
          let info_subj = TypeInfo_pat.Ana(ty_ann);
          let (_, ctx) = TypeInfo_pat.synthesize(info_subj, subj);
          let l_subj = Layout_term.Pat.mk(info_subj, subj);
          let l_ann = cat(l_subj, grouts_r([fst(mk_Ann(l_ann))]));
          err_hole(
            TypeInfo_pat.ann_has_err(ty_ann, info_ann),
            l_frame(ty_ann, ctx, l_ann),
          );
        }
      );
    };
  };
};
