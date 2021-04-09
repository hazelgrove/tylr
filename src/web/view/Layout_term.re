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
         | (l, bin, r) => {
             let l_bin =
               switch (bin) {
               | BinHole => mk_BinHole(~has_caret?, ())
               | Arrow => mk_Arrow(~has_caret?, ())
               | Prod => mk_Prod(~has_caret?, ())
               };
             (mk(l), l_bin, mk(r));
           },
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
          let (syn_ty, _) = {
            let info =
              TypeInfo_pat.has_err(info, p) ? TypeInfo_pat.Syn : info;
            TypeInfo_pat.synthesize(info, p);
          };
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
               let l_subj = mk(Ana(Term_typ.to_type(ann)), subj);
               let l_ann = Typ.mk(ann);
               (l_subj, mk_Ann(~has_caret?, l_ann));
             },
           fun
           | (l, bin, r) => {
               let (info_l, info_r, l_bin) =
                 switch (bin) {
                 | BinHole => (
                     TypeInfo_pat.Syn,
                     TypeInfo_pat.Syn,
                     mk_BinHole(~has_caret?, ()),
                   )
                 | Prod => (
                     TypeInfo_pat.prod_l(info),
                     TypeInfo_pat.prod_r(info),
                     mk_Prod(~has_caret?, ()),
                   )
                 };
               (mk(info_l, l), l_bin, mk(info_r, r));
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
            info: TypeInfo_exp.t,
            e: Term_exp.t,
          )
          : Layout.t => {
    open Term_exp;
    let has_caret =
      Option.map(
        pos => {
          let syn_ty = {
            let info =
              TypeInfo_exp.has_err(info, e)
                ? TypeInfo_exp.{ctx: info.ctx, mode: Syn} : info;
            TypeInfo_exp.(synthesize(info, e));
          };
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
               let l_p = Pat.mk(TypeInfo_exp.let_pat(def, info), p);
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
           | (l, BinHole, r) => {
               let l_l = mk(TypeInfo_exp.binhole_l(info), l);
               let l_r = mk(TypeInfo_exp.binhole_r(info), r);
               (l_l, mk_BinHole(~has_caret?, ()), l_r);
             }
           | (l, Plus, r) => {
               let l_l = mk(TypeInfo_exp.plus_l(info), l);
               let l_r = mk(TypeInfo_exp.plus_r(info), r);
               (l_l, mk_Plus(~has_caret?, ()), l_r);
             }
           | (l, Prod, r) => {
               let l_l = mk(TypeInfo_exp.prod_l(info), l);
               let l_r = mk(TypeInfo_exp.prod_r(info), r);
               (l_l, mk_Prod(~has_caret?, ()), l_r);
             }
           | (guard, Cond(then_), else_) => {
               let l_guard = mk({...info, mode: Ana(Bool)}, guard);
               let l_then = mk(info, then_);
               let l_else = mk(info, else_);
               (l_guard, mk_Cond(~has_caret?, l_then), l_else);
             },
         );
    TypeInfo_exp.has_err(info, e)
      ? Annot(ErrHole(Option.is_some(has_caret)), l) : l;
  };
};
