// open Sexplib.Std;

[@deriving sexp]
type mode =
  | Syn
  | Ana(Type.t);
[@deriving sexp]
type t = {
  ctx: Ctx.t,
  mode,
};

let root = {ctx: Ctx.empty, mode: Syn};

type out('a) = Type.t => 'a;

let num_has_err = (info_num: t) =>
  switch (info_num.mode) {
  | Syn => false
  | Ana(ty) => !Type.consistent(ty, Num)
  };

let var_has_err = (x, info_var: t) =>
  switch (Ctx.find_opt(x, info_var.ctx)) {
  | None => true
  | Some(ty) =>
    switch (info_var.mode) {
    | Syn => false
    | Ana(ty') => !Type.consistent(ty, ty')
    }
  };

let lam_has_err = (info_lam: t) =>
  switch (info_lam.mode) {
  | Ana(ty) => Option.is_none(Type.matches_arrow(ty))
  | Syn => false
  };
let lam_pat = (info_lam: t): TypeInfo_pat.t => {
  switch (info_lam.mode) {
  | Syn => TypeInfo_pat.Syn
  | Ana(ty) =>
    switch (Type.matches_arrow(ty)) {
    | None => TypeInfo_pat.Syn
    | Some((ty_in, _)) => TypeInfo_pat.Ana(ty_in)
    }
  };
};

let lam_body_mode = (mode_lam: mode): mode =>
  switch (mode_lam) {
  | Syn => Syn
  | Ana(ty) =>
    switch (Type.matches_arrow(ty)) {
    | None => Syn
    | Some((_, ty_out)) => Ana(ty_out)
    }
  };
let lam_body = (p: Term_pat.t, info_lam: t): t => {
  let info_p = lam_pat(info_lam);
  let (_, ctx_body) = TypeInfo_pat.synthesize(info_p, p);
  let mode_body = lam_body_mode(info_lam.mode);
  {ctx: ctx_body, mode: mode_body};
};

let let_def = (p: Term_pat.t, info_let: t): t => {
  let (ty_p, _) = TypeInfo_pat.synthesize(TypeInfo_pat.Syn, p);
  {ctx: info_let.ctx, mode: Ana(ty_p)};
};

let plus_has_err = (info_plus: t) =>
  switch (info_plus.mode) {
  | Syn => false
  | Ana(ty) => !Type.consistent(ty, Num)
  };
let plus_l = (info_plus: t): t => {...info_plus, mode: Ana(Num)};
let plus_r = plus_l;

let prod_has_err = (info_prod: t) =>
  switch (info_prod.mode) {
  | Ana(ty) when Option.is_none(Type.matches_prod(ty)) => true
  | _ => false
  };
let prod_l = (info_prod: t) => {
  let mode =
    switch (info_prod.mode) {
    | Syn => Syn
    | Ana(ty) =>
      switch (Type.matches_prod(ty)) {
      | None => Syn
      | Some((ty_l, _)) => Ana(ty_l)
      }
    };
  {ctx: info_prod.ctx, mode};
};
let prod_r = (info_prod: t) => {
  let mode =
    switch (info_prod.mode) {
    | Syn => Syn
    | Ana(ty) =>
      switch (Type.matches_prod(ty)) {
      | None => Syn
      | Some((_, ty_r)) => Ana(ty_r)
      }
    };
  {ctx: info_prod.ctx, mode};
};

let binhole_l = (info_binhole: t): t => {...info_binhole, mode: Syn};
let binhole_r = binhole_l;

let cond_guard = (info_cond: t): t => {ctx: info_cond.ctx, mode: Ana(Bool)};

let subsume = (info, ty) =>
  switch (info.mode) {
  | Syn => ty
  | Ana(ty') => Type.consistent(ty, ty') ? ty : Hole
  };

// synthesize(info, e) where info.mode == Syn  ==>  (same as normal syn)
// synthesize(info, e) where info.mode == Ana(ty, _)  ==>  ty' consistent with ty (ty' same as normal syn after ana_fix_holes)
// synthesize(info, e) where info.mode == Fn_pos(_)  ==>  ty' s.t. Option.is_some(Type.matches_arrow(ty')) (ty' same as normal syn after syn_fix_holes + matched_arrow fix)
let rec synthesize = (info: t, e: Term_exp.t): Type.t => {
  let subsume = subsume(info);
  Term_exp.(
    e
    |> Term.get(
         fun
         | OpHole => Type.Hole
         | Num(_) => subsume(Num)
         | Var(x) =>
           switch (Ctx.find_opt(x, info.ctx)) {
           | None => Hole
           | Some(ty) => subsume(ty)
           }
         | Paren(body) => synthesize(info, body),
         fun
         | (Lam(p), body) => {
             let (ty_in, _) = TypeInfo_pat.synthesize(lam_pat(info), p);
             let ty_out = synthesize(lam_body(p, info), body);
             subsume(Arrow(ty_in, ty_out));
           }
         | (Let(p, def), body) => synthesize(let_body(p, def, info), body),
         fun
         | (_, Ap(_)) => failwith("ap todo"),
         fun
         | (_, BinHole, _) => Hole
         | (_, Plus, _) => subsume(Num)
         | (l, Prod, r) => {
             let ty_l = synthesize(prod_l(info), l);
             let ty_r = synthesize(prod_r(info), r);
             subsume(Prod(ty_l, ty_r));
           }
         | (_cond, Cond(then_), else_) => {
             let ty_then = synthesize(info, then_);
             let ty_else = synthesize(info, else_);
             switch (Type.join(ty_then, ty_else)) {
             | None => Hole
             | Some(ty_joined) => subsume(ty_joined)
             };
           },
       )
  );
}
and let_body = (p: Term_pat.t, def: Term_exp.t, info_let: t): t => {
  let ctx_body = extend_ctx_let_body(p, def, info_let.ctx);
  {...info_let, ctx: ctx_body};
}
and extend_ctx_let_body = (p: Term_pat.t, def: Term_exp.t, ctx: Ctx.t) => {
  let (ty_p, _) = TypeInfo_pat.synthesize(Syn, p);
  let ty_def = synthesize({ctx, mode: Ana(ty_p)}, def);
  let (_, ctx) = TypeInfo_pat.synthesize(Ana(ty_def), p);
  ctx;
};

let let_pat = (def: Term_exp.t, info_let: t) =>
  TypeInfo_pat.Let_pat(
    ty_p => synthesize({ctx: info_let.ctx, mode: Ana(ty_p)}, def),
  );

let has_err = (info: t) =>
  Term_exp.(
    Term.get(
      fun
      | OpHole
      | Paren(_) => false
      | Var(x) => var_has_err(x, info)
      | Num(_) => num_has_err(info),
      fun
      | (Lam(_), _) => lam_has_err(info)
      | (Let(_), _) => false,
      fun
      | (_, Ap(_)) => failwith("ap todo"),
      fun
      | (_, BinHole, _) => false
      | (_, Plus, _) => plus_has_err(info)
      | (_, Prod, _) => prod_has_err(info)
      | (_, Cond(then_), else_) => {
          let ty_then = synthesize({ctx: info.ctx, mode: Syn}, then_);
          let ty_else = synthesize({ctx: info.ctx, mode: Syn}, else_);
          !Type.consistent(ty_then, ty_else);
        },
    )
  );

let ap_fn = (info_ap: t) => {
  ctx: info_ap.ctx,
  mode: Ana(Arrow(Hole, Hole)),
};
let ap_arg = (fn, info_ap: t) => {
  let fn_ty = synthesize(ap_fn(info_ap), fn);
  let (ty_in, _) = Option.get(Type.matches_arrow(fn_ty));
  {ctx: info_ap.ctx, mode: Ana(ty_in)};
};
