let rec syn = (ctx: Ctx.t, (skel, tiles): UHExp.t): option((HTyp.t, Ctx.t)) => {
  switch (skel) {
  | Operand(n) =>
    switch (Tiles.get_operand(n, tiles)) {
    | Hole => Some((Hole, ctx))
    | Num(_) => Some((Num, ctx))
    | Var(x) => ctx |> Ctx.find_opt(x) |> Option.map(ty => (ty, ctx))
    | Paren(e) => syn(ctx, e)
    }
  | PreOp(n, skel) =>
    switch (Tiles.get_preop(n, tiles)) {
    | If(cond, then_clause) =>
      let else_clause = (skel, tiles);
      if (ana(ctx, cond, HTyp.Bool)) {
        switch (syn(ctx, then_clause), syn(ctx, else_clause)) {
        | (Some((then_ty, _)), Some((else_ty, _))) when then_ty == else_ty =>
          Some((then_ty, ctx))
        | _ => None
        };
      } else {
        None;
      };
    | Let(x, def) =>
      let body = (skel, tiles);
      switch (syn(ctx, def)) {
      | None => None
      | Some((def_ty, _)) => syn(Ctx.add(x, def_ty, ctx), body)
      };
    }
  | PostOp(_) => failwith("empty postop")
  | BinOp(skel1, n, skel2) =>
    switch (Tiles.get_binop(n, tiles)) {
    | Plus
    | Times =>
      if (ana(ctx, (skel1, tiles), Num) && ana(ctx, (skel2, tiles)), Num) {
        Some((Num, ctx));
      } else {
        None;
      }
    | Eq =>
      if (ana(ctx, (skel1, tiles), Num) && ana(ctx, (skel2, tiles)), Num) {
        Some((Bool, ctx));
      } else {
        None;
      }
    }
  };
}
and ana = (ctx: Ctx.t, (skel, tiles): UHExp.t, ty: HTyp.t): bool => {
  switch (skel) {
  | Operand(n) =>
    switch (Tiles.get_operand(n, tiles)) {
    | Hole
    | Num(_)
    | Var(_) =>
      switch (syn(ctx, (skel, tiles))) {
      | Some(ty') when HTyp.consistent(ty, ty') => true
      | _ => false
      }
    | Paren(e) => ana(ctx, e, ty)
    }
  | PreOp(n, skel) =>
    switch (Tiles.get_preop(n, tiles)) {
    | If(cond, then_clause) =>
      let else_clause = (skel, tiles);
      ana(ctx, cond, Bool)
      && ana(ctx, then_clause, ty)
      && ana(ctx, else_clause, ty);
    }
  | PostOp(_) => failwith("empty postop")
  | BinOp(_) =>
    switch (syn(ctx, (skel, tiles))) {
    | Some(ty') when HTyp.consistent(ty, ty') => true
    | _ => false
    }
  };
};
