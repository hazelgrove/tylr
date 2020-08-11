module Exp = {
  let rec syn = (ctx: Ctx.t, e: HExp.t): option(HTyp.t) =>
    switch (e) {
    | OperandHole => Some(OperandHole)
    | NonemptyHole(e) => syn(ctx, e) |> Option.map(_ => HTyp.OperandHole)
    | Num(_) => Some(Num)
    | Var(x) => Ctx.find_opt(x, ctx)
    | Paren(body) => syn(ctx, body)
    | Lam(p, body) =>
      switch (Pat.syn(ctx, p)) {
      | None => None
      | Some((ty1, ctx)) =>
        syn(ctx, body) |> Option.map(ty2 => HTyp.Arrow(ty1, ty2))
      }
    | Plus(e1, e2) =>
      if (ana(ctx, e1, Num) && ana(ctx, e2, Num)) {
        Some(HTyp.Num);
      } else {
        None;
      }
    | Ap(fn, arg) =>
      switch (syn(ctx, fn)) {
      | None => None
      | Some(fn_ty) =>
        switch (HTyp.matched_arrow(fn_ty)) {
        | None => None
        | Some((ty1, ty2)) => ana(ctx, arg, ty1) ? Some(ty2) : None
        }
      }
    | OperatorHole(e1, e2) =>
      OptUtil.map2((_, _) => HTyp.OperandHole, syn(ctx, e1), syn(ctx, e2))
    }
  and ana = (ctx: Ctx.t, e: HExp.t, ty: HTyp.t): bool =>
    switch (e) {
    | OperandHole
    | NonemptyHole(_)
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

  let rec syn_fix_holes = (ctx: Ctx.t, e: HExp.t): (HExp.t, HTyp.t) =>
    switch (e) {
    | OperandHole => (OperandHole, OperandHole)
    | NonemptyHole(e) => syn_fix_holes(ctx, e)
    | Num(_) => (e, Num)
    | Var(x) =>
      switch (Ctx.find_opt(x, ctx)) {
      | None => (NonemptyHole(e), OperandHole)
      | Some(ty) => (e, ty)
      }
    | Paren(body) =>
      let (body, ty) = syn_fix_holes(ctx, body);
      (Paren(body), ty);
    | Lam(p, body) =>
      let (p, ty1, ctx) = Pat.syn_fix_holes(ctx, p);
      let (body, ty2) = syn_fix_holes(ctx, body);
      (Lam(p, body), Arrow(ty1, ty2));
    | Ap(fn, arg) =>
      let (fn, fn_ty) = syn_fix_holes(ctx, fn);
      switch (HTyp.matched_arrow(fn_ty)) {
      | None =>
        let arg = ana_fix_holes(ctx, arg, OperandHole);
        (Ap(NonemptyHole(fn), arg), OperandHole);
      | Some((ty1, ty2)) =>
        let arg = ana_fix_holes(ctx, arg, ty1);
        (Ap(fn, arg), ty2);
      };
    | OperatorHole(e1, e2) =>
      let (e1, _) = syn_fix_holes(ctx, e1);
      let (e2, _) = syn_fix_holes(ctx, e2);
      (OperatorHole(e1, e2), OperandHole);
    }
  and ana_fix_holes = (ctx: Ctx.t, e: HExp.t, ty: HTyp.t): HExp.t =>
    switch (e) {
    | OperandHole
    | NonemptyHole(_)
    | Num(_)
    | Var(_)
    | Plus(_)
    | Ap(_)
    | OperatorHole(_) =>
      let (e, ty') = syn_fix_holes(ctx, e);
      HTyp.consistent(ty, ty') ? e : NonemptyHole(e);
    | Paren(body) =>
      let body = ana_fix_holes(ctx, e, ty);
      Paren(body);
    | Lam(p, body) =>
      switch (HTyp.matched_arrow(ty)) {
      | None =>
        let (e, _) = syn_fix_holes(ctx, e);
        NonemptyHole(e);
      | Some((ty1, ty2)) =>
        let (p, ctx) = Pat.ana_fix_holes(ctx, p, ty1);
        let body = ana_fix_holes(ctx, ty2);
        Lam(p, body);
      }
    };
};
