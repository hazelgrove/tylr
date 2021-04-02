open Sexplib.Std;

[@deriving sexp]
type t'('a) = {
  ctx: Ctx.t,
  mode: mode'('a),
}
and mode'('a) =
  | Syn(Type.t => 'a)
  // maybe todo: split into ana proper and ana_syn
  | Ana(Type.t /* expected ty */, Type.t /* consistent ty */ => 'a)
  | Fn_pos((Type.t /* ty in */, Type.t /* ty out */) => 'a);

let map_mode' = (f: 'a => 'b) =>
  fun
  | Syn(a) => Syn(ty => f(a(ty)))
  | Ana(ty, a) => Ana(ty, ty' => f(a(ty')))
  | Fn_pos(a) => Fn_pos((ty_in, ty_out) => f(a(ty_in, ty_out)));

[@deriving sexp]
type t = t'(unit);
type mode = mode'(unit);
let syn = Syn(_ => ());
let ana = ty => Ana(ty, _ => ());
let fn_pos = Fn_pos((_, _) => ());

let root' = a => {ctx: Ctx.empty, mode: Syn(a)};

let of_t' = info => {...info, mode: map_mode'(_ => (), info.mode)};

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

let lam_has_err = (info_lam: t'(_)) =>
  switch (info_lam.mode) {
  | Ana(ty, _) => Option.is_none(Type.matches_arrow(ty))
  | Syn(_)
  | Fn_pos(_) => false
  };
let lam_pat = (info_lam: t'(_)): TypeInfo_pat.t => {
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

let lam_body_mode = (mode_lam: mode'(_)): mode =>
  switch (mode_lam) {
  | Syn(_)
  | Fn_pos(_) => syn
  | Ana(ty, _) =>
    switch (Type.matches_arrow(ty)) {
    | None => syn
    | Some((_, ty_out)) => ana(ty_out)
    }
  };
let lam_body = (p: Term_pat.t, info_lam: t'(_)): t => {
  let info_p = lam_pat(info_lam);
  let (_, ctx_body) = TypeInfo_pat.synthesize(info_p, p);
  let mode_body = lam_body_mode(info_lam.mode);
  {ctx: ctx_body, mode: mode_body};
};

let let_pat = (info_let: t'(_)): TypeInfo_pat.t =>
  TypeInfo_pat.{ctx: info_let.ctx, mode: syn};
let let_def = (p: Term_pat.t, info_let: t'(_)): t => {
  let (ty_p, _) =
    TypeInfo_pat.synthesize({ctx: info_let.ctx, mode: TypeInfo_pat.syn}, p);
  {ctx: info_let.ctx, mode: ana(ty_p)};
};

let plus_has_err = (info_plus: t'(_)) =>
  switch (info_plus.mode) {
  | Syn(_) => false
  | Ana(ty, _) => !Type.consistent(ty, Num)
  | Fn_pos(_) => true
  };
let plus_l = (info_plus: t'(_)): t => {...info_plus, mode: ana(Num)};
let plus_r = plus_l;

let binhole_l = (info_binhole: t'(_)): t => {...info_binhole, mode: syn};
let binhole_r = binhole_l;

// synthesize(info, e) where info.mode == Syn  ==>  (same as normal syn)
// synthesize(info, e) where info.mode == Ana(ty, _)  ==>  ty' consistent with ty (ty' same as normal syn after ana_fix_holes)
// synthesize(info, e) where info.mode == Fn_pos(_)  ==>  ty' s.t. Option.is_some(Type.matches_arrow(ty')) (ty' same as normal syn after syn_fix_holes + matched_arrow fix)
let rec synthesize = (info: t'(_), e: Term_exp.t): Type.t => {
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
       | (_, Plus, _) => ty_under_info(Num)
       | (_cond, Cond(then_), else_) => {
           let ty_then = synthesize(info, then_);
           let ty_else = synthesize(info, else_);
           switch (Type.join(ty_then, ty_else)) {
           | None => Hole
           | Some(ty_joined) => ty_under_info(ty_joined)
           };
         },
     );
}
and let_body = (p: Term_pat.t, def: Term_exp.t, info_let: t'(_)): t => {
  let ctx_body = extend_ctx_let_body(p, def, info_let.ctx);
  {...info_let, ctx: ctx_body};
}
and extend_ctx_let_body = (p: Term_pat.t, def: Term_exp.t, ctx: Ctx.t) => {
  let (ty_p, _) = TypeInfo_pat.(synthesize({ctx, mode: syn}, p));
  let ty_def = synthesize({ctx, mode: ana(ty_p)}, def);
  let (_, ctx) =
    TypeInfo_pat.synthesize({ctx, mode: TypeInfo_pat.ana(ty_def)}, p);
  ctx;
};

let has_err = (info: t'(_)) =>
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
      | (_, Cond(then_), else_) => {
          let ty_then = synthesize({ctx: info.ctx, mode: syn}, then_);
          let ty_else = synthesize({ctx: info.ctx, mode: syn}, else_);
          !Type.consistent(ty_then, ty_else);
        },
    )
  );

let lam_pat' =
    (f: (Ctx.t, 'a) => 'b, body: Term_exp.t, info_lam: t'('a))
    : TypeInfo_pat.t'('b) => {
  let mode =
    switch (info_lam.mode) {
    | Syn(a) =>
      TypeInfo_pat.Syn(
        (ty_p, ctx) => {
          let ty_body = synthesize({ctx, mode: syn}, body);
          f(ctx, a(Arrow(ty_p, ty_body)));
        },
      )
    | Fn_pos(a) =>
      Syn(
        (ty_p, ctx) => {
          let ty_body = synthesize({ctx, mode: syn}, body);
          f(ctx, a(ty_p, ty_body));
        },
      )
    | Ana(ty, a) =>
      switch (Type.matches_arrow(ty)) {
      | None => Syn((_, ctx) => f(ctx, a(Hole)))
      | Some((ty_in, ty_out)) =>
        Ana(
          ty_in,
          (ty_p, ctx) => {
            let ty_body = synthesize({ctx, mode: ana(ty_out)}, body);
            f(ctx, a(Arrow(ty_p, ty_body)));
          },
        )
      }
    };
  TypeInfo_pat.{ctx: info_lam.ctx, mode};
};
let lam_body' = (f: 'a => 'a, p: Term_pat.t, info_lam: t'('a)): t'('a) => {
  let (ty_p, ctx_body) = TypeInfo_pat.synthesize(lam_pat(info_lam), p);
  let mode_body =
    switch (info_lam.mode) {
    | Syn(a) => Syn(ty_body => f(a(Arrow(ty_p, ty_body))))
    | Fn_pos(a) => Syn(ty_body => f(a(ty_p, ty_body)))
    | Ana(ty, a) =>
      switch (Type.matches_arrow(ty)) {
      | None => Syn(_ => f(a(Hole)))
      | Some((_, ty_out)) =>
        Ana(ty_out, ty_body => f(a(Arrow(ty_p, ty_body))))
      }
    };
  {ctx: ctx_body, mode: mode_body};
};

let ap_fn = (info_ap: t) => {ctx: info_ap.ctx, mode: fn_pos};
let ap_arg = (fn, info_ap: t) => {
  let fn_ty = synthesize(ap_fn(info_ap), fn);
  let (ty_in, _) = Option.get(Type.matches_arrow(fn_ty));
  {ctx: info_ap.ctx, mode: ana(ty_in)};
};

let let_pat' =
    (
      f: (Type.t /* ty pat */, Ctx.t /* ctx body */, 'a) => 'a,
      def,
      body,
      info_let: t'('a),
    )
    : TypeInfo_pat.t'('a) => {
  {
    ctx: info_let.ctx,
    mode:
      Let_pat(
        ty_p => synthesize({ctx: info_let.ctx, mode: ana(ty_p)}, def),
        (ty_p, ctx_body) => {
          let f = f(ty_p, ctx_body);
          switch (info_let.mode) {
          | Syn(a) =>
            let ty_body = synthesize({ctx: ctx_body, mode: syn}, body);
            f(a(ty_body));
          | Ana(ty, a) =>
            let ty_body = synthesize({ctx: ctx_body, mode: ana(ty)}, body);
            f(a(ty_body));
          | Fn_pos(a) =>
            let ty = synthesize({ctx: ctx_body, mode: fn_pos}, body);
            let (ty_in, ty_out) = Option.get(Type.matches_arrow(ty));
            f(a(ty_in, ty_out));
          };
        },
      ),
  };
};
let let_def' =
    (f: (t /* info body */, 'a) => 'a, p, body, info_let: t'('a)): t'('a) => {
  let (ty_p, _) =
    TypeInfo_pat.(synthesize({ctx: info_let.ctx, mode: syn}, p));
  {
    ctx: info_let.ctx,
    mode:
      Ana(
        ty_p,
        ty_def => {
          let f = {
            let (_, ctx_body) =
              TypeInfo_pat.(
                synthesize({ctx: info_let.ctx, mode: ana(ty_def)}, p)
              );
            f(of_t'({ctx: ctx_body, mode: info_let.mode}));
          };
          let (_, ctx_body) =
            TypeInfo_pat.(
              synthesize({ctx: info_let.ctx, mode: ana(ty_def)}, p)
            );
          switch (info_let.mode) {
          | Syn(a) =>
            let ty_body = synthesize({ctx: ctx_body, mode: syn}, body);
            f(a(ty_body));
          | Ana(ty, a) =>
            let ty_body = synthesize({ctx: ctx_body, mode: ana(ty)}, body);
            f(a(ty_body));
          | Fn_pos(a) =>
            let ty = synthesize({ctx: ctx_body, mode: fn_pos}, body);
            let (ty_in, ty_out) = Option.get(Type.matches_arrow(ty));
            f(a(ty_in, ty_out));
          };
        },
      ),
  };
};

let binhole_l' = (f: 'a => 'b, info_binhole: t'('a)): t'('b) => {
  let mode =
    switch (info_binhole.mode) {
    | Syn(a)
    | Ana(_, a) => Syn(_ => f(a(Hole)))
    | Fn_pos(a) => Syn(_ => f(a(Hole, Hole)))
    };
  {mode, ctx: info_binhole.ctx};
};
let binhole_r' = binhole_l';

let plus_l' = (f: 'a => 'b, info_plus: t'('a)): t'('b) => {
  let mode =
    switch (info_plus.mode) {
    | Syn(a) => Ana(Num, _ => f(a(Num)))
    | Ana(ty, a) =>
      let ty = Type.consistent(ty, Num) ? Type.Num : Hole;
      Ana(Num, _ => f(a(ty)));
    | Fn_pos(a) => Ana(Num, _ => f(a(Hole, Hole)))
    };
  {mode, ctx: info_plus.ctx};
};
let plus_r' = plus_l';
