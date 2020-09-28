module Pat = {
  let rec syn = (ctx: Ctx.t, p: HPat.t): option((Type.t, Ctx.t)) => {
    OptUtil.Syntax.(
      switch (HPat.root(p)) {
      | Operand(operand) =>
        switch (operand) {
        | OperandHole => Some((Type.Hole, ctx))
        | Var(x) => Some((Type.Hole, Ctx.add(x, Type.Hole, ctx)))
        | Paren(body) => syn(ctx, body)
        }
      | PreOp(_) => raise(HPat.Tile.Void_PreOp)
      | PostOp((subj, Ann(status, ann))) =>
        switch (status) {
        | NotInHole =>
          let ty = HTyp.contract(ann);
          let+ ctx = ana(ctx, subj, ty);
          (ty, ctx);
        | InHole =>
          let+ (_, ctx) = syn(ctx, HPat.put_hole_status(NotInHole, p));
          (Type.Hole, ctx);
        }
      | BinOp((l, OperatorHole, r)) =>
        let* (_, ctx) = syn(ctx, l);
        let+ (_, ctx) = syn(ctx, r);
        (Type.Hole, ctx);
      }
    );
  }
  and ana = (ctx: Ctx.t, p: HPat.t, ty: Type.t): option(Ctx.t) => {
    open OptUtil.Syntax;
    let subsume = () => {
      let* (ty', ctx) = syn(ctx, p);
      Type.consistent(ty, ty') ? Some(ctx) : None;
    };
    switch (HPat.root(p)) {
    | Operand(operand) =>
      switch (operand) {
      | OperandHole => subsume()
      | Var(x) => Some(Ctx.add(x, ty, ctx))
      | Paren(body) => ana(ctx, body, ty)
      }
    | PreOp(_) => raise(HPat.Tile.Void_PreOp)
    | PostOp((_, Ann(_)))
    | BinOp((_, OperatorHole, _)) => subsume()
    };
  };

  let rec syn_fix_holes = (ctx: Ctx.t, p: HPat.t): (HPat.t, Type.t, Ctx.t) =>
    switch (HPat.root(p)) {
    | Operand(operand) =>
      switch (operand) {
      | OperandHole => ([Tile.Operand(operand)], Type.Hole, ctx)
      | Var(x) =>
        let ctx = Ctx.add(x, Type.Hole, ctx);
        ([Tile.Operand(operand)], Hole, ctx);
      | Paren(body) =>
        let (body, ty, ctx) = syn_fix_holes(ctx, body);
        ([Tile.Operand(HPat.Tile.Paren(body))], ty, ctx);
      }
    | PreOp(_) => raise(HPat.Tile.Void_PreOp)
    | PostOp((subj, Ann(_, ann))) =>
      let ty = HTyp.contract(ann);
      let (subj, ctx) = ana_fix_holes(ctx, subj, ty);
      (subj @ [Tile.PostOp(HPat.Tile.Ann(NotInHole, ann))], ty, ctx);
    | BinOp((l, OperatorHole, r)) =>
      let (l, _, ctx) = syn_fix_holes(ctx, l);
      let (r, _, ctx) = syn_fix_holes(ctx, r);
      (l @ [Tile.BinOp(OperatorHole), ...r], Type.Hole, ctx);
    }
  and ana_fix_holes = (ctx: Ctx.t, p: HPat.t, ty: Type.t): (HPat.t, Ctx.t) => {
    let subsume = () => {
      let (p, ty', ctx) = syn_fix_holes(ctx, p);
      let p = Type.consistent(ty, ty') ? p : HPat.put_hole_status(InHole, p);
      (p, ctx);
    };
    switch (HPat.root(p)) {
    | Operand(operand) =>
      switch (operand) {
      | OperandHole => subsume()
      | Var(x) => (p, Ctx.add(x, ty, ctx))
      | Paren(body) =>
        let (body, ctx) = ana_fix_holes(ctx, body, ty);
        ([Tile.Operand(HPat.Tile.Paren(body))], ctx);
      }
    | PreOp(_) => raise(HPat.Tile.Void_PreOp)
    | PostOp((_, Ann(_)))
    | BinOp((_, OperatorHole, _)) => subsume()
    };
  };
};

module Exp = {
  let rec syn = (ctx: Ctx.t, e: HExp.t): option(Type.t) => {
    open OptUtil.Syntax;
    let in_hole = () => {
      let+ _ = syn(ctx, HExp.put_hole_status(NotInHole, e));
      Type.Hole;
    };
    switch (HExp.root(e)) {
    | Operand(operand) =>
      switch (operand) {
      | OperandHole
      | Var(InHole, _)
      | Num(InHole, _) => Some(Hole)
      | Var(NotInHole, x) => Ctx.find_opt(x, ctx)
      | Num(NotInHole, _) => Some(Num)
      | Paren(body) => syn(ctx, body)
      }
    | PreOp((Lam(status, p), body)) =>
      switch (status) {
      | InHole => in_hole()
      | NotInHole =>
        let* (ty_in, ctx) = Pat.syn(ctx, p);
        let+ ty_out = syn(ctx, body);
        Type.Arrow(ty_in, ty_out);
      }
    | PostOp((fn, Ap(status, arg))) =>
      switch (status) {
      | InHole => in_hole()
      | NotInHole =>
        let* fn_ty = syn(ctx, fn);
        let* (ty_in, ty_out) = Type.matched_arrow(fn_ty);
        ana(ctx, arg, ty_in) ? Some(ty_out) : None;
      }
    | BinOp((l, Plus(status), r)) =>
      switch (status) {
      | InHole => in_hole()
      | NotInHole =>
        ana(ctx, l, Type.Num) && ana(ctx, r, Type.Num) ? Some(Num) : None
      }
    | BinOp((l, OperatorHole, r)) =>
      let+ _ = syn(ctx, l)
      and+ _ = syn(ctx, r);
      Type.Hole;
    };
  }
  and ana = (ctx: Ctx.t, e: HExp.t, ty: Type.t): bool => {
    let ( let* ) = (o, f) =>
      switch (o) {
      | None => false
      | Some(a) => f(a)
      };
    let subsume = () => {
      let* ty' = syn(ctx, e);
      Type.consistent(ty, ty');
    };
    switch (HExp.root(e)) {
    | Operand(operand) =>
      switch (operand) {
      | OperandHole
      | Num(_)
      | Var(_) => subsume()
      | Paren(body) => ana(ctx, body, ty)
      }
    | PreOp((Lam(InHole, _), _)) =>
      Option.is_some(syn(ctx, HExp.put_hole_status(NotInHole, e)))
    | PreOp((Lam(NotInHole, p), body)) =>
      let* (ty_in, ty_out) = Type.matched_arrow(ty);
      let* ctx = Pat.ana(ctx, p, ty_in);
      ana(ctx, body, ty_out);
    | PostOp((_, Ap(_)))
    | BinOp((_, OperatorHole | Plus(_), _)) => subsume()
    };
  };

  let rec syn_fix_holes = (ctx: Ctx.t, e: HExp.t): (HExp.t, Type.t) =>
    switch (HExp.root(e)) {
    | Operand(operand) =>
      switch (operand) {
      | OperandHole => ([Tile.Operand(operand)], Type.Hole)
      | Num(_, m) => (
          [Tile.Operand(HExp.Tile.Num(NotInHole, m))],
          Type.Num,
        )
      | Var(_, x) =>
        switch (Ctx.find_opt(x, ctx)) {
        | None => ([Tile.Operand(HExp.Tile.Var(InHole, x))], Type.Hole)
        | Some(ty) => ([Tile.Operand(HExp.Tile.Var(NotInHole, x))], ty)
        }
      | Paren(body) =>
        let (body, ty) = syn_fix_holes(ctx, body);
        ([Tile.Operand(HExp.Tile.Paren(body))], ty);
      }
    | PreOp((Lam(_, p), body)) =>
      let (p, ty1, ctx) = Pat.syn_fix_holes(ctx, p);
      let (body, ty2) = syn_fix_holes(ctx, body);
      (
        [Tile.PreOp(HExp.Tile.Lam(NotInHole, p)), ...body],
        Arrow(ty1, ty2),
      );
    | PostOp((fn, Ap(_, arg))) =>
      let (fn, fn_ty) = syn_fix_holes(ctx, fn);
      switch (Type.matched_arrow(fn_ty)) {
      | None =>
        let fn = HExp.put_hole_status(InHole, fn);
        let arg = ana_fix_holes(ctx, arg, Type.Hole);
        (fn @ [Tile.PostOp(HExp.Tile.Ap(NotInHole, arg))], Type.Hole);
      | Some((ty1, ty2)) =>
        let arg = ana_fix_holes(ctx, arg, ty1);
        (e @ [Tile.PostOp(HExp.Tile.Ap(NotInHole, arg))], ty2);
      };
    | BinOp((l, Plus(_), r)) =>
      let l = ana_fix_holes(ctx, l, Type.Num);
      let r = ana_fix_holes(ctx, r, Type.Num);
      (l @ [Tile.BinOp(HExp.Tile.Plus(NotInHole)), ...r], Type.Num);
    | BinOp((l, OperatorHole, r)) =>
      let (l, _) = syn_fix_holes(ctx, l);
      let (r, _) = syn_fix_holes(ctx, r);
      (l @ [Tile.BinOp(OperatorHole), ...r], Type.Hole);
    }
  and ana_fix_holes = (ctx: Ctx.t, e: HExp.t, ty: Type.t): HExp.t => {
    let subsume = () => {
      let (e, ty') = syn_fix_holes(ctx, e);
      Type.consistent(ty, ty') ? e : HExp.put_hole_status(InHole, e);
    };
    switch (HExp.root(e)) {
    | Operand(operand) =>
      switch (operand) {
      | OperandHole
      | Num(_)
      | Var(_) => subsume()
      | Paren(body) =>
        let body = ana_fix_holes(ctx, body, ty);
        [Tile.Operand(HExp.Tile.Paren(body))];
      }
    | PreOp((Lam(_, p), body)) =>
      switch (Type.matched_arrow(ty)) {
      | None =>
        let (p, _, ctx) = Pat.syn_fix_holes(ctx, p);
        let (body, _) = syn_fix_holes(ctx, body);
        [Tile.PreOp(HExp.Tile.Lam(InHole, p)), ...body];
      | Some((ty_in, ty_out)) =>
        let (p, ctx) = Pat.ana_fix_holes(ctx, p, ty_in);
        let body = ana_fix_holes(ctx, body, ty_out);
        [Tile.PreOp(HExp.Tile.Lam(NotInHole, p)), ...body];
      }
    | PostOp((_, Ap(_)))
    | BinOp((_, OperatorHole | Plus(_), _)) => subsume()
    };
  };
};
