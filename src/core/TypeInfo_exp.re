open Sexplib.Std;

[@deriving sexp]
type t'('a) = {
  ctx: Ctx.t,
  mode: mode('a),
}
and mode('a) =
  | Syn(Type.t => 'a)
  | Ana(Type.t, 'a)
  | Fn_pos((Type.t, Type.t) => 'a);

let map_mode = (f: 'a => 'b) =>
  fun
  | Syn(g) => Syn(ty => f(g(ty)))
  | Ana(ty, a) => Ana(ty, f(a))
  | Fn_pos(g) => Fn_pos((ty_in, ty_out) => f(g(ty_in, ty_out)));

[@deriving sexp]
type t = t'(unit);
let syn = Syn(_ => ());
let ana = ty => Ana(ty, ());
let fn_pos = Fn_pos((_, _) => ());

let of_t' = info => {...info, mode: map_mode(_ => (), info.mode)};

let num_has_err = (info_num: t) =>
  switch (info_num.mode) {
  | Syn(_) => false
  | Ana(ty, _) => !Type.consistent(ty, Num)
  | Fn_pos(_) => true
  };

let var_has_err = (x, info_var: t) =>
  switch (Ctx.find_opt(x, info_var.ctx)) {
  | None => true
  | Some(ty) =>
    switch (info_var.mode) {
    | Syn(_) => false
    | Ana(ty', _) => !Type.consistent(ty, ty')
    | Fn_pos(_) => Option.is_none(Type.matches_arrow(ty))
    }
  };

let lam_has_err = (info_lam: t) =>
  switch (info_lam.mode) {
  | Ana(ty, _) => Option.is_none(Type.matches_arrow(ty))
  | Syn(_)
  | Fn_pos(_) => false
  };
let lam_pat = (info_lam: t) => {
  let mode =
    switch (info_lam.mode) {
    | Syn(_)
    | Fn_pos(_) => TypeInfo_pat.syn
    | Ana(ty, _) =>
      switch (Type.matches_arrow(ty)) {
      | None => TypeInfo_pat.syn
      | Some((ty_in, _)) => TypeInfo_pat.ana(ty_in)
      }
    };
  TypeInfo_pat.{ctx: info_lam.ctx, mode};
};
let lam_body = (p: Term_pat.t, info_lam: t) => {
  let info_p = lam_pat(info_lam);
  let (_, ctx_body) = TypeInfo_pat.synthesize(info_p, p);
  let mode_body =
    switch (info_lam.mode) {
    | Syn(_)
    | Fn_pos(_) => syn
    | Ana(ty, ()) =>
      switch (Type.matches_arrow(ty)) {
      | None => syn
      | Some((_, ty_out)) => ana(ty_out)
      }
    };
  {ctx: ctx_body, mode: mode_body};
};

let extend_ctx_let_body = (_, _, _) => failwith("todo extend_ctx_let_body");

let let_pat = (info_let: t) =>
  TypeInfo_pat.{ctx: info_let.ctx, mode: TypeInfo_pat.syn};
let let_def = (p: Term_pat.t, info_let: t) => {
  let (ty_p, _) = TypeInfo_pat.synthesize(let_pat(info_let), p);
  {ctx: info_let.ctx, mode: ana(ty_p)};
};
let let_body = (p: Term_pat.t, def: Term_exp.t, info_let: t) => {
  let ctx_body = extend_ctx_let_body(info_let.ctx, p, def);
  {...info_let, ctx: ctx_body};
};

let plus_has_err = (info_plus: t) =>
  switch (info_plus.mode) {
  | Syn(_) => false
  | Ana(ty, _) => !Type.consistent(ty, Num)
  | Fn_pos(_) => true
  };
let plus_l = (info_plus: t) => {...info_plus, mode: ana(Num)};
let plus_r = plus_l;

let binhole_l = (info_binhole: t) => {...info_binhole, mode: syn};
let binhole_r = binhole_l;

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
      | (_, Plus, _) => plus_has_err(info),
    )
  );

// synthesize(info, e) where info.mode == Syn  ==>  (same as normal syn)
// synthesize(info, e) where info.mode == Ana(ty, _)  ==>  ty' consistent with ty (ty' same as normal syn after ana_fix_holes)
// synthesize(info, e) where info.mode == Fn_pos(_)  ==>  ty' s.t. Option.is_some(Type.matches_arrow(ty')) (ty' same as normal syn after syn_fix_holes + matched_arrow fix)
let rec synthesize = (info: t, e: Term_exp.t) => {
  open Term_exp;
  let ty_under_info = ty =>
    switch (info.mode) {
    | Syn(_) => ty
    | Ana(ty', _) => Type.consistent(ty, ty') ? ty : Hole
    | Fn_pos(_) => Option.is_some(Type.matches_arrow(ty)) ? ty : Hole
    };
  e
  |> Term.get(
       fun
       | OpHole => Type.Hole
       | Num(_) => ty_under_info(Num)
       | Var(x) =>
         switch (Ctx.find_opt(x, info.ctx)) {
         | None => Hole
         | Some(ty) => ty_under_info(ty)
         }
       | Paren(body) => synthesize(info, body),
       fun
       | (Lam(p), body) => {
           let (ty_in, _) = TypeInfo_pat.synthesize(lam_pat(info), p);
           let ty_out = synthesize(lam_body(p, info), body);
           ty_under_info(Arrow(ty_in, ty_out));
         }
       | (Let(p, def), body) => synthesize(let_body(p, def, info), body),
       fun
       | (_, Ap(_)) => failwith("ap todo"),
       fun
       | (_, BinHole, _) => Hole
       | (_, Plus, _) => ty_under_info(Num),
     );
};

let ap_fn = (info_ap: t) => {ctx: info_ap.ctx, mode: fn_pos};
let ap_arg = (fn, info_ap: t) => {
  let fn_ty = synthesize(ap_fn(info_ap), fn);
  let (ty_in, _) = Option.get(Type.matches_arrow(fn_ty));
  {ctx: info_ap.ctx, mode: ana(ty_in)};
};
