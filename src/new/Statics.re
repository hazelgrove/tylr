open Util;
open OptUtil.Syntax;

module Pat = {
  open HPat;

  // returns None if p doesn't parse
  let rec syn = (ctx: Ctx.t, p: HPat.t): option((Type.t, Ctx.t)) => {
    p
    |> HPat.get(
      fun
      | OpHole => Some((Type.Hole, ctx))
      | Var(x) => Some((Type.Hole, Ctx.add(x, Type.Hole, ctx)))
      | Paren(body) => {
        let* body = HPat.mk(body);
        syn(ctx, body);
      },
      fun
      | ((), _) => raise(HPat.Void_pre),
      fun
      | (subj, Ann(ann)) => {
        let* ann = HTyp.mk(ann);
        let* ty = HTyp.contract(ann);
        let+ ctx = ana(ctx, subj, ty);
        (ty, ctx);
      },
      fun
      | (l, BinHole, r) => {
        let* (_, ctx) = syn(ctx, l);
        let+ (_, ctx) = syn(ctx, r);
        (Type.Hole, ctx);
      }
    );
  }
  and ana = (ctx: Ctx.t, p: HPat.t, ty: Type.t): option(Ctx.t) => {
    let subsume = ()  => {
      let+ (_, ctx) = syn(ctx, p);
      ctx;
    };
    p
    |> HPat.get(
      fun
      | OpHole => subsume()
      | Var(x) => Some(Ctx.add(x, ty, ctx))
      | Paren(body) => {
        let* body = HPat.mk(body);
        ana(ctx, body, ty);
      },
      fun
      | ((), _) => raise(HPat.Void_pre),
      fun
      | (_, Ann(_)) => subsume(),
      fun
      | (_, BinHole, _) => subsume(),
    );
  }
};

module Exp = {
  open HExp;

  let rec syn = (ctx: Ctx.t, e: HExp.t): option(Type.t) => {
    e
    |> HExp.get(
      fun
      | OpHole => Some(Type.Hole)
      | Num(_) => Some(Num)
      | Var(x) => Ctx.find_opt(x, ctx)
      | Paren(body) => {
        let* body = HExp.mk(body);
        syn(ctx, body)
      },
      fun
      | (Lam(p), body) => {
        let* p = HPat.mk(p);
        let* (ty_p, ctx) = Pat.syn(ctx, p);
        let+ ty_body = syn(ctx, body);
        Type.Arrow(ty_p , ty_body);
      }
      | (Let(p, def), body) => {
        let* p = HPat.mk(p);
        let* def = HExp.mk(def);
        let* ty_def = syn(ctx, def);
        let* ctx = Pat.ana(ctx, p, ty_def);
        syn(ctx, body);
      },
      fun
      | (fn, Ap(arg)) => {
        let* ty_fn = syn(ctx, fn);
        let (ty_in, ty_out) =
          switch (Type.matched_arrow(ty_fn)) {
          | None => (Type.Hole, Type.Hole)
          | Some(matched) => matched
          };
        ana()
      },
      fun
      | (l, Plus, r) => {
        let+ _ = syn(ctx, l)
        and+ _ = syn(ctx, r);
        Type.Num;
      }
      | (l, BinHole, r) => {
        let+ _ = syn(ctx, l)
        and+ _ = syn(ctx, r);
        Hole;
      }
    )
  }
  and ana = (ctx: Ctx.t, e: HExp.t, ty: Type.t): bool => {
    e
    |> HExp.get(

    )
  }
}