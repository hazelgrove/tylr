open Util;

module Pat = {
  open OptUtil.Syntax;
  open HPat.T;

  let rec syn = (ctx: Ctx.t, p: HPat.t): option((PType.t, Ctx.t)) => {
    let in_hole = Some((PType.Unspecified, ctx));
    let op =
      fun
      | OpHole => in_hole
      | Var(x) => Some((Unspecified, Ctx.add(x, Type.Hole, ctx)))
      | Paren(body) => syn(ctx, body);
    let pre = (((), _)) => raise(Void_pre);
    let post =
      fun
      | (_, Ann(InHole, _)) => in_hole
      | (subj, Ann(NotInHole, ann)) => {
          let ty = HTyp.contract(ann);
          let+ ctx = ana(ctx, subj, ty);
          (PType.of_type(ty), ctx);
        };
    let bin = ((l, BinHole, r)) => {
      let* (_, ctx) = syn(ctx, l);
      let+ (_, ctx) = syn(ctx, r);
      (PType.Unspecified, ctx);
    };
    Tile.get(op, pre, post, bin, HPat.root(p));
  }
  and ana = (ctx: Ctx.t, p: HPat.t, ty: Type.t): option(Ctx.t) => {
    let subsume = () => {
      let* (pty, ctx) = syn(ctx, p);
      Type.consistent(PType.to_type(pty), ty) ? Some(ctx) : None;
    };
    let op =
      fun
      | OpHole => subsume()
      | Var(x) => Some(Ctx.add(x, ty, ctx))
      | Paren(body) => ana(ctx, body, ty);
    let pre = (((), _)) => raise(Void_pre);
    let post = ((_, Ann(_))) => subsume();
    let bin = ((_, BinHole, _)) => subsume();
    Tile.get(op, pre, post, bin, HPat.root(p));
  };

  let rec syn_fix_holes = (ctx: Ctx.t, p: HPat.t): (HPat.t, PType.t, Ctx.t) => {
    let op =
      fun
      | (OpHole | Var(_)) as op => ([Tile.Op(op)], PType.Unspecified, ctx)
      | Paren(body) => {
          let (body, pty, ctx) = syn_fix_holes(ctx, body);
          ([Tile.Op(Paren(body))], pty, ctx);
        };
    let pre = (((), _)) => raise(Void_pre);
    let post = ((subj, Ann(_, ann))) => {
      let ty = HTyp.contract(ann);
      let (subj, ctx) = ana_fix_holes(Ctx.empty, subj, ty);
      (subj @ [Tile.Post(Ann(NotInHole, ann))], PType.of_type(ty), ctx);
    };
    let bin = ((l, BinHole, r)) => {
      let (l, _, ctx) = syn_fix_holes(ctx, l);
      let (r, _, ctx) = syn_fix_holes(ctx, r);
      (l @ [Tile.Bin(BinHole), ...r], PType.Unspecified, ctx);
    };
    Tile.get(op, pre, post, bin, HPat.root(p));
  }
  and ana_fix_holes = (ctx: Ctx.t, p: HPat.t, ty: Type.t): (HPat.t, Ctx.t) => {
    let op =
      fun
      | OpHole => (p, ctx)
      | Var(x) => (p, Ctx.add(x, ty, ctx))
      | Paren(body) => {
          let (body, ctx) = ana_fix_holes(ctx, body, ty);
          ([Tile.Op(Paren(body))], ctx);
        };
    let pre = (((), _)) => raise(Void_pre);
    let post = ((subj, Ann(_, ann))) => {
      let ty_ann = HTyp.contract(ann);
      let (subj, ctx) = ana_fix_holes(ctx, subj, ty_ann);
      let status: HoleStatus.t =
        Type.consistent(ty_ann, ty) ? NotInHole : InHole;
      (subj @ [Tile.Post(Ann(status, ann))], ctx);
    };
    let bin = ((l, BinHole, r)) => {
      let (l, ctx) = ana_fix_holes(ctx, l, Hole);
      let (r, ctx) = ana_fix_holes(ctx, r, Hole);
      // TODO review
      (l @ [Tile.Bin(BinHole), ...r], ctx);
    };
    Tile.get(op, pre, post, bin, HPat.root(p));
  };
};

module Exp = {
  open HExp.T;

  let rec syn = (ctx: Ctx.t, e: HExp.t): option(Type.t) => {
    open OptUtil.Syntax;
    let in_hole = () => {
      let+ _ = syn(ctx, HExp.put_hole_status(NotInHole, e));
      Type.Hole;
    };
    let op =
      fun
      | OpHole
      | Var(InHole, _)
      | Num(InHole, _) => Some(Type.Hole)
      | Var(NotInHole, x) => Ctx.find_opt(x, ctx)
      | Num(NotInHole, _) => Some(Num)
      | Paren(body) => syn(ctx, body);
    let pre =
      fun
      | (Lam(status, p), body) =>
        switch (status) {
        | InHole => in_hole()
        | NotInHole =>
          let* (pty, ctx) = Pat.syn(ctx, p);
          let+ ty_body = syn(ctx, body);
          Type.Arrow(PType.to_type(pty), ty_body);
        }
      | (Let(p, def), body) => {
          let* (pty, _) = Pat.syn(ctx, p);
          let* def_ty = syn(ctx, def);
          let joined_ty = PType.join_or_to_type(pty, def_ty);
          let* ctx_body = Pat.ana(ctx, p, joined_ty);
          ana(ctx, def, joined_ty) ? syn(ctx_body, body) : None;
        };
    let post =
      fun
      | (fn, Ap(status, arg)) =>
        switch (status) {
        | InHole => in_hole()
        | NotInHole =>
          let* fn_ty = syn(ctx, fn);
          let* (ty_in, ty_out) = Type.matched_arrow(fn_ty);
          ana(ctx, arg, ty_in) ? Some(ty_out) : None;
        };
    let bin =
      fun
      | (l, Plus(status), r) =>
        switch (status) {
        | InHole => in_hole()
        | NotInHole =>
          ana(ctx, l, Type.Num) && ana(ctx, r, Type.Num) ? Some(Num) : None
        }
      | (l, BinHole, r) => {
          let+ _ = syn(ctx, l)
          and+ _ = syn(ctx, r);
          Type.Hole;
        };
    Tile.get(op, pre, post, bin, HExp.root(e));
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
    let op =
      fun
      | OpHole
      | Num(_)
      | Var(_) => subsume()
      | Paren(body) => ana(ctx, body, ty);
    let pre =
      fun
      | (Lam(InHole, _), _) =>
        Option.is_some(syn(ctx, HExp.put_hole_status(NotInHole, e)))
      | (Lam(NotInHole, p), body) => {
          let* (ty_in, ty_out) = Type.matched_arrow(ty);
          let* ctx = Pat.ana(ctx, p, ty_in);
          ana(ctx, body, ty_out);
        }
      | (Let(p, def), body) => {
          let* (pty, _) = Pat.syn(ctx, p);
          let* def_ty = syn(ctx, def);
          let joined_ty = PType.join_or_to_type(pty, def_ty);
          let* ctx_body = Pat.ana(ctx, p, joined_ty);
          ana(ctx, def, joined_ty) && ana(ctx_body, body, ty);
        };
    let post =
      fun
      | (_, Ap(_)) => subsume();
    let bin =
      fun
      | (_, BinHole | Plus(_), _) => subsume();
    Tile.get(op, pre, post, bin, HExp.root(e));
  };

  let rec syn_fix_holes = (ctx: Ctx.t, e: HExp.t): (HExp.t, Type.t) => {
    let op = op =>
      switch (op) {
      | OpHole => ([Tile.Op(op)], Type.Hole)
      | Num(_, m) => ([Tile.Op(Num(NotInHole, m))], Type.Num)
      | Var(_, x) =>
        switch (Ctx.find_opt(x, ctx)) {
        | None => ([Tile.Op(Var(InHole, x))], Type.Hole)
        | Some(ty) => ([Tile.Op(Var(NotInHole, x))], ty)
        }
      | Paren(body) =>
        let (body, ty) = syn_fix_holes(ctx, body);
        ([Tile.Op(Paren(body))], ty);
      };
    let pre = pre =>
      switch (pre) {
      | (Lam(_, p), body) =>
        let (p, pty, ctx) = Pat.syn_fix_holes(ctx, p);
        let (body, body_ty) = syn_fix_holes(ctx, body);
        (
          [Tile.Pre(Lam(NotInHole, p)), ...body],
          Type.Arrow(PType.to_type(pty), body_ty),
        );
      | (Let(p, def), body) =>
        let (p, pty, _) = Pat.syn_fix_holes(ctx, p);
        let (def, def_ty) = syn_fix_holes(ctx, def);
        let joined_ty = PType.join_or_to_type(pty, def_ty);
        let (p, ctx_body) = Pat.ana_fix_holes(ctx, p, joined_ty);
        let def = ana_fix_holes(ctx, def, joined_ty);
        let (body, ty) = syn_fix_holes(ctx_body, body);
        ([Tile.Pre(Let(p, def)), ...body], ty);
      };
    let post = ((fn, Ap(_, arg))) => {
      let (fn, fn_ty) = syn_fix_holes(ctx, fn);
      switch (Type.matched_arrow(fn_ty)) {
      | None =>
        let fn = HExp.put_hole_status(InHole, fn);
        let arg = ana_fix_holes(ctx, arg, Type.Hole);
        (fn @ [Tile.Post(Ap(NotInHole, arg))], Type.Hole);
      | Some((ty1, ty2)) =>
        let arg = ana_fix_holes(ctx, arg, ty1);
        (e @ [Tile.Post(Ap(NotInHole, arg))], ty2);
      };
    };
    let bin = bin =>
      switch (bin) {
      | (l, Plus(_), r) =>
        let l = ana_fix_holes(ctx, l, Type.Num);
        let r = ana_fix_holes(ctx, r, Type.Num);
        (l @ [Tile.Bin(Plus(NotInHole)), ...r], Type.Num);
      | (l, BinHole, r) =>
        let (l, _) = syn_fix_holes(ctx, l);
        let (r, _) = syn_fix_holes(ctx, r);
        (l @ [Tile.Bin(BinHole), ...r], Type.Hole);
      };
    Tile.get(op, pre, post, bin, HExp.root(e));
  }
  and ana_fix_holes = (ctx: Ctx.t, e: HExp.t, ty: Type.t): HExp.t => {
    let subsume = () => {
      let (e, ty') = syn_fix_holes(ctx, e);
      Type.consistent(ty, ty') ? e : HExp.put_hole_status(InHole, e);
    };
    let op = op =>
      switch (op) {
      | OpHole
      | Num(_)
      | Var(_) => subsume()
      | Paren(body) =>
        let body = ana_fix_holes(ctx, body, ty);
        [Tile.Op(Paren(body))];
      };
    let pre = pre =>
      switch (pre) {
      | (Lam(_, p), body) =>
        switch (Type.matched_arrow(ty)) {
        | None =>
          let (p, ctx) = Pat.ana_fix_holes(ctx, p, Hole);
          let (body, _) = syn_fix_holes(ctx, body);
          [Tile.Pre(Lam(InHole, p)), ...body];
        | Some((ty_in, ty_out)) =>
          let (p, ctx) = Pat.ana_fix_holes(ctx, p, ty_in);
          let body = ana_fix_holes(ctx, body, ty_out);
          [Tile.Pre(Lam(NotInHole, p)), ...body];
        }
      | (Let(_), _) => subsume()
      };
    let post =
      fun
      | (_, Ap(_)) => subsume();
    let bin =
      fun
      | (_, BinHole | Plus(_), _) => subsume();
    Tile.get(op, pre, post, bin, HExp.root(e));
  };
};
