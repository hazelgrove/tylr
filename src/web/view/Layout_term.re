open Core;
open Layout;

module Typ = {
  let decorate_term =
    decorate_term(
      ~sort=Typ,
      ~is_op_hole=Term_typ.is_op_hole,
      ~is_bin_hole=Term_typ.is_bin_hole,
    );

  let rec mk = (~has_caret: option(CaretPosition.t)=?, ty: Term_typ.t) => {
    open Term_typ;
    let has_caret = Option.map(pos => (Pointing(Typ), pos), has_caret);
    ty
    |> decorate_term(
         ~has_caret,
         fun
         | OpHole => mk_OpHole(~has_caret?, ())
         | Num => mk_text(~has_caret?, "num")
         | Bool => mk_text(~has_caret?, "bool")
         | Paren(body) => mk_Paren(~has_caret?, mk(body)),
         (((), _)) => raise(Void_pre),
         ((_, ())) => raise(Void_post),
         fun
         | (l, BinHole, r) => (mk(l), mk_BinHole(~has_caret?, ()), mk(r))
         | (l, Arrow, r) => (mk(l), mk_Arrow(~has_caret?, ()), mk(r)),
       );
  };
};

module Pat = {
  let decorate_term =
    decorate_term(
      ~sort=Pat,
      ~is_op_hole=Term_pat.is_op_hole,
      ~is_bin_hole=Term_pat.is_bin_hole,
    );

  let rec mk =
          (
            ~has_caret: option(CaretPosition.t)=?,
            info: TypeInfo_pat.t,
            p: Term_pat.t,
          ) => {
    open Term_pat;
    let has_caret =
      Option.map(
        pos => {
          let (syn_ty, _) =
            TypeInfo_pat.(synthesize({ctx: info.ctx, mode: syn}, p));
          (Pointing(Pat(info, syn_ty)), pos);
        },
        has_caret,
      );
    let l =
      p
      |> decorate_term(
           ~has_caret,
           fun
           | OpHole => mk_OpHole(~has_caret?, ())
           | Var(x) => mk_text(~has_caret?, x)
           | Paren(body) => mk_Paren(~has_caret?, mk(info, body)),
           (((), _)) => raise(Void_pre),
           fun
           | (subj, Ann(ann)) => {
               let l_subj = mk(TypeInfo_pat.ann_subj(ann, info), subj);
               let l_ann = Typ.mk(ann);
               (l_subj, mk_Ann(~has_caret?, l_ann));
             },
           fun
           | (l, BinHole, r) => {
               let l_l = mk(TypeInfo_pat.binhole_l(info), l);
               let l_r = mk(TypeInfo_pat.binhole_r(l, info), r);
               (l_l, mk_BinHole(~has_caret?, ()), l_r);
             },
         );
    TypeInfo_pat.has_err(info, p)
      ? Annot(ErrHole(Option.is_some(has_caret)), l) : l;
  };
};

module Exp = {
  let decorate_term =
    decorate_term(
      ~sort=Exp,
      ~is_op_hole=Term_exp.is_op_hole,
      ~is_bin_hole=Term_exp.is_bin_hole,
    );

  let rec mk =
          (
            ~has_caret: option(CaretPosition.t)=?,
            info: TypeInfo_exp.t'(_),
            e: Term_exp.t,
          )
          : Layout.t => {
    open Term_exp;
    let has_caret =
      Option.map(
        pos => {
          let syn_ty =
            TypeInfo_exp.(synthesize({ctx: info.ctx, mode: syn}, e));
          (Pointing(Exp(info, syn_ty)), pos);
        },
        has_caret,
      );
    let l =
      e
      |> decorate_term(
           ~has_caret,
           fun
           | OpHole => mk_OpHole(~has_caret?, ())
           | Var(x) => mk_text(~has_caret?, x)
           | Num(n) => mk_text(~has_caret?, string_of_int(n))
           | Paren(body) => mk_Paren(~has_caret?, mk(info, body)),
           fun
           | (Lam(p), body) => {
               let l_p = Pat.mk(TypeInfo_exp.lam_pat(info), p);
               let l_body = mk(TypeInfo_exp.lam_body(p, info), body);
               (mk_Lam(~has_caret?, l_p), l_body);
             }
           | (Let(p, def), body) => {
               let l_p = Pat.mk(TypeInfo_exp.let_pat(info), p);
               let l_def = mk(TypeInfo_exp.let_def(p, info), def);
               let l_body = mk(TypeInfo_exp.let_body(p, def, info), body);
               (mk_Let(~has_caret?, l_p, l_def), l_body);
             },
           fun
           | (fn, Ap(arg)) => {
               let _l_fn = mk(TypeInfo_exp.ap_fn(info), fn);
               let _l_arg = mk(TypeInfo_exp.ap_arg(fn, info), arg);
               failwith("ap todo");
             },
           fun
           | (l, Plus, r) => {
               let l_l = mk(TypeInfo_exp.plus_l(info), l);
               let l_r = mk(TypeInfo_exp.plus_r(info), r);
               (l_l, mk_Plus(~has_caret?, ()), l_r);
             }
           | (l, BinHole, r) => {
               let l_l = mk(TypeInfo_exp.binhole_l(info), l);
               let l_r = mk(TypeInfo_exp.binhole_r(info), r);
               (l_l, mk_BinHole(~has_caret?, ()), l_r);
             },
         );
    TypeInfo_exp.has_err(info, e)
      ? Annot(ErrHole(Option.is_some(has_caret)), l) : l;
  };
};
