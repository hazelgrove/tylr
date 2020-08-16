module Pat = {
  let rec syn = (ctx: Ctx.t, p: HPat.t): option((HTyp.t, Ctx.t)) =>
    switch (p) {
    | OperandHole => Some((OperandHole, ctx))
    | Var(x) => Some((OperandHole, Ctx.add(x, HTyp.OperandHole, ctx)))
    | Paren(body) => syn(ctx, body)
    | Ann(InHole, subj, ann) =>
      syn(ctx, Ann(NotInHole, subj, ann))
      |> Option.map(((_, ctx)) => (HTyp.OperandHole, ctx))
    | Ann(NotInHole, subj, ann) =>
      ana(ctx, subj, ann) |> Option.map(ctx => (ann, ctx))
    | OperatorHole(p1, p2) =>
      switch (syn(ctx, p1)) {
      | None => None
      | Some((_, ctx)) =>
        syn(ctx, p2) |> Option.map(((_, ctx)) => (HTyp.OperandHole, ctx))
      }
    }
  and ana = (ctx: Ctx.t, p: HPat.t, ty: HTyp.t): option(Ctx.t) =>
    switch (p) {
    | OperandHole
    | OperatorHole(_)
    | Ann(_) =>
      switch (syn(ctx, p)) {
      | None => None
      | Some((ty', ctx)) => HTyp.consistent(ty, ty') ? Some(ctx) : None
      }
    | Var(x) => Some(Ctx.add(x, ty, ctx))
    | Paren(body) => ana(ctx, body, ty)
    };

  let rec syn_fix_holes = (ctx: Ctx.t, p: HPat.t): (HPat.t, HTyp.t, Ctx.t) =>
    switch (p) {
    | OperandHole => (p, OperandHole, ctx)
    | Var(x) =>
      let ctx = Ctx.add(x, HTyp.OperandHole, ctx);
      (p, OperandHole, ctx);
    | Paren(body) =>
      let (body, ty, ctx) = syn_fix_holes(ctx, body);
      (Paren(body), ty, ctx);
    | Ann(_, subj, ann) =>
      let (subj, ctx) = ana_fix_holes(ctx, subj, ann);
      (Ann(NotInHole, subj, ann), ann, ctx);
    | OperatorHole(p1, p2) =>
      let (p1, _, ctx) = syn_fix_holes(ctx, p1);
      let (p2, _, ctx) = syn_fix_holes(ctx, p2);
      (OperatorHole(p1, p2), OperandHole, ctx);
    }
  and ana_fix_holes = (ctx: Ctx.t, p: HPat.t, ty: HTyp.t): (HPat.t, Ctx.t) =>
    switch (p) {
    | OperandHole
    | OperatorHole(_)
    | Ann(_) =>
      let (p, ty', ctx) = syn_fix_holes(ctx, p);
      let p = HTyp.consistent(ty, ty') ? p : HPat.set_hole_status(InHole, p);
      (p, ctx);
    | Var(x) => (p, Ctx.add(x, ty, ctx))
    | Paren(body) =>
      let (body, ctx) = ana_fix_holes(ctx, body, ty);
      (Paren(body), ctx);
    };
};

module Exp = {
  let rec syn = (ctx: Ctx.t, e: HExp.t): option(HTyp.t) =>
    switch (e) {
    | OperandHole
    | Var(InHole, _)
    | Num(InHole, _) => Some(OperandHole)
    | Var(NotInHole, x) => Ctx.find_opt(x, ctx)
    | Num(NotInHole, _) => Some(Num)
    | Paren(body) => syn(ctx, body)
    | Lam(status, p, body) =>
      switch (status) {
      | InHole =>
        syn(ctx, Lam(NotInHole, p, body))
        |> Option.map(_ => HTyp.OperandHole)
      | NotInHole =>
        switch (Pat.syn(ctx, p)) {
        | None => None
        | Some((ty1, ctx)) =>
          syn(ctx, body) |> Option.map(ty2 => HTyp.Arrow(ty1, ty2))
        }
      }
    | Plus(status, e1, e2) =>
      switch (status) {
      | InHole =>
        syn(ctx, Plus(NotInHole, e1, e2))
        |> Option.map(_ => HTyp.OperandHole)
      | NotInHole =>
        ana(ctx, e1, HTyp.Num) && ana(ctx, e2, HTyp.Num)
          ? Some(HTyp.Num) : None
      }
    | Ap(status, fn, arg) =>
      switch (status) {
      | InHole =>
        syn(ctx, Ap(NotInHole, fn, arg)) |> Option.map(_ => HTyp.OperandHole)
      | NotInHole =>
        switch (syn(ctx, fn)) {
        | None => None
        | Some(fn_ty) =>
          switch (HTyp.matched_arrow(fn_ty)) {
          | None => None
          | Some((ty1, ty2)) => ana(ctx, arg, ty1) ? Some(ty2) : None
          }
        }
      }
    | OperatorHole(e1, e2) =>
      OptUtil.map2((_, _) => HTyp.OperandHole, syn(ctx, e1), syn(ctx, e2))
    }
  and ana = (ctx: Ctx.t, e: HExp.t, ty: HTyp.t): bool =>
    switch (e) {
    | OperandHole
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
    | Lam(InHole, p, body) =>
      Option.is_some(syn(ctx, Lam(NotInHole, p, body)))
    | Lam(NotInHole, p, body) =>
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
    | Num(_, n) => (Num(NotInHole, n), Num)
    | Var(_, x) =>
      switch (Ctx.find_opt(x, ctx)) {
      | None => (Var(InHole, x), OperandHole)
      | Some(ty) => (Var(NotInHole, x), ty)
      }
    | Paren(body) =>
      let (body, ty) = syn_fix_holes(ctx, body);
      (Paren(body), ty);
    | Lam(_, p, body) =>
      let (p, ty1, ctx) = Pat.syn_fix_holes(ctx, p);
      let (body, ty2) = syn_fix_holes(ctx, body);
      (Lam(NotInHole, p, body), Arrow(ty1, ty2));
    | Plus(_, e1, e2) =>
      let e1 = ana_fix_holes(ctx, e1, HTyp.Num);
      let e2 = ana_fix_holes(ctx, e2, HTyp.Num);
      (Plus(NotInHole, e1, e2), Num);
    | Ap(_, fn, arg) =>
      let (fn, fn_ty) = syn_fix_holes(ctx, fn);
      switch (HTyp.matched_arrow(fn_ty)) {
      | None =>
        let fn = HExp.set_hole_status(InHole, fn);
        let arg = ana_fix_holes(ctx, arg, HTyp.OperandHole);
        (Ap(NotInHole, fn, arg), OperandHole);
      | Some((ty1, ty2)) =>
        let arg = ana_fix_holes(ctx, arg, ty1);
        (Ap(NotInHole, fn, arg), ty2);
      };
    | OperatorHole(e1, e2) =>
      let (e1, _) = syn_fix_holes(ctx, e1);
      let (e2, _) = syn_fix_holes(ctx, e2);
      (OperatorHole(e1, e2), OperandHole);
    }
  and ana_fix_holes = (ctx: Ctx.t, e: HExp.t, ty: HTyp.t): HExp.t =>
    switch (e) {
    | OperandHole
    | Num(_)
    | Var(_)
    | Plus(_)
    | Ap(_)
    | OperatorHole(_) =>
      let (e, ty') = syn_fix_holes(ctx, e);
      HTyp.consistent(ty, ty') ? e : HExp.set_hole_status(InHole, e);
    | Paren(body) =>
      let body = ana_fix_holes(ctx, body, ty);
      Paren(body);
    | Lam(InHole, p, body) =>
      let (e, _) = syn_fix_holes(ctx, Lam(NotInHole, p, body));
      HExp.set_hole_status(InHole, e);
    | Lam(NotInHole, p, body) =>
      switch (HTyp.matched_arrow(ty)) {
      | None =>
        let (e, _) = syn_fix_holes(ctx, e);
        HExp.set_hole_status(InHole, e);
      | Some((ty1, ty2)) =>
        let (p, ctx) = Pat.ana_fix_holes(ctx, p, ty1);
        let body = ana_fix_holes(ctx, body, ty2);
        Lam(NotInHole, p, body);
      }
    };
};
