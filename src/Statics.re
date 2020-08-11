module Exp = {
  let rec syn = (ctx: Ctx.t, e: HExp.t): option(HTyp.t) =>
    switch (e) {
    | OperandHole => Some(OperandHole)
    | TypeErr(e) =>
      syn(ctx, e)
      |> Option.map(_ => HTyp.OperandHole)
    | Num(_) => Some(Num)
    | Var(x) =>
      Ctx.find_opt(x, ctx)
    | Paren(body) => syn(ctx, body)
    | Lam(p, body) =>
      switch (Pat.syn(ctx, p)) {
      | None => None
      | Some((ty1, ctx)) =>
        syn(ctx, body)
        |> Option.map(ty2 => (HTyp.Arrow(ty1, ty2)))
      }
    | Plus(e1, e2) =>
      if (ana(ctx, e1, Num) && ana(ctx, e2, Num)) {
        Some(HTyp.Num)
      } else {
        None
      };
    | Ap(fn, arg) =>
      switch (syn(ctx, fn)) {
      | None => None
      | Some(fn_ty) =>
        switch (HTyp.matched_arrow(fn_ty)) {
        | None => None
        | Some((ty1, ty2)) =>
          ana(ctx, arg, ty1) ? Some(ty2) : None
        }
      }
    | OperatorHole(e1, e2) =>
      OptUtil.map2(
        (_, _) => HTyp.OperandHole,
        syn(ctx, e1),
        syn(ctx, e2),
      )
    }
  and ana = (ctx: Ctx.t, e: HExp.t, ty: HTyp.t): bool =>
    switch (e) {
    | OperandHole
    | TypeErr(_)
    | Num(_)
    | Var(_)
    | Plus(_)
    | Ap(_)
    | OperatorHole(_) =>
      switch (syn(ctx, e)) {
      | None => false
      | Some(ty') => HTyp.consistent(ty, ty')
      }
    | Paren(e) => ana(ctx, e, ty)
    | Lam(p, body) =>
      switch (HTyp.matched_arrow(ty)) {
      | None => false
      | Some((ty1, ty2)) =>
        switch (Pat.ana(ctx, p, ty1)) {
        | None => false
        | Some(ctx) => ana(ctx, body, ty2)
        }
      }
    };
};