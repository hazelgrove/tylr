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
      | PostOp(tiles, postop) =>
        switch (postop) {
        | Ann(status, ann) =>
          let subj = tiles;
          switch (status) {
          | NotInHole =>
            let ty = HTyp.contract(ann);
            let+ ctx = ana(ctx, subj, ty);
            (ty, ctx);
          | InHole =>
            let+ (_, ctx) = syn(ctx, HPat.put_hole_status(NotInHole, p));
            (Type.Hole, ctx);
          };
        }
      | BinOp(tiles1, binop, tiles2) =>
        switch (binop) {
        | OperatorHole =>
          let* (_, ctx) = syn(ctx, tiles1);
          let+ (_, ctx) = syn(ctx, tiles2);
          (Type.Hole, ctx);
        }
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
    | PostOp(_, postop) =>
      switch (postop) {
      | Ann(_) => subsume()
      }
    | BinOp(_, binop, _) =>
      switch (binop) {
      | OperatorHole => subsume()
      }
    };
  };

  let rec nth_type_info =
          (n: int, p: HPat.t, info: TypeInfo.t)
          : option((HPat.t, TypeInfo.t)) => {
    OptUtil.Syntax.(
      switch (HPat.root(p)) {
      | Operand(_) =>
        n == 0
          ? Some((p, info))
          : raise(Invalid_argument("Statics_H.Pat.nth_typ_info"))
      | PreOp(_) => raise(HPat.Tile.Void_PreOp)
      | PostOp(p, postop) =>
        n == List.length(p)
          ? Some((p, info))
          : (
            switch (postop) {
            | Ann(_, ty) =>
              let subj_info = {...info, mode: Ana(HTyp.contract(ty))};
              nth_type_info(n, p, subj_info);
            }
          )
      | BinOp(l, binop, r) =>
        let m = List.length(l);
        if (n < m) {
          switch (binop) {
          | OperatorHole => nth_type_info(n, r, {...info, mode: Syn})
          };
        } else if (n > m) {
          let* (_, ctx) = syn(info.ctx, p);
          let info_r = {...info, ctx};
          switch (binop) {
          | OperatorHole =>
            nth_type_info(n - (m + 1), r, {...info_r, mode: Syn})
          };
        } else {
          Some((p, info));
        };
      }
    );
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
    | PostOp(p, postop) =>
      switch (postop) {
      | Ann(_, ann) =>
        let ty = HTyp.contract(ann);
        let (subj, ctx) = ana_fix_holes(ctx, p, ty);
        (subj @ [Tile.PostOp(HPat.Tile.Ann(NotInHole, ann))], ty, ctx);
      }
    | BinOp(p1, binop, p2) =>
      switch (binop) {
      | OperatorHole =>
        let (p1, _, ctx) = syn_fix_holes(ctx, p1);
        let (p2, _, ctx) = syn_fix_holes(ctx, p2);
        (p1 @ [Tile.BinOp(OperatorHole), ...p2], Type.Hole, ctx);
      }
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
    | PostOp(_, postop) =>
      switch (postop) {
      | Ann(_) => subsume()
      }
    | BinOp(_, binop, _) =>
      switch (binop) {
      | OperatorHole => subsume()
      }
    };
  };

  let rec syn_fix_holes_z = (ctx: Ctx.t, zp: ZPat.t): (ZPat.t, Type.t, Ctx.t) =>
    switch (ZPat.root(zp)) {
    | None =>
      let n = ZPat.z_index(zp);
      let (p, ty, ctx) = syn_fix_holes(ctx, ZPat.erase(zp));
      let zp = {
        let (prefix, suffix) = ListUtil.split_n(n, p);
        ZPat.mk(~prefix, ~suffix, ());
      };
      (zp, ty, ctx);
    | Some(root) =>
      switch (root) {
      | OperandZ(ParenZ_body(zbody)) =>
        let (zbody, ty, ctx) = syn_fix_holes_z(ctx, zbody);
        let zp = ZPat.mk(~z=Operand(ParenZ_body(zbody)), ());
        (zp, ty, ctx);
      | PreOpZ_op(_)
      | PreOpZ_arg(_) => raise(HPat.Tile.Void_PreOp)
      | PostOpZ_op(subj, AnnZ_ann(_, zann)) =>
        let ty = HTyp.contract(ZTyp.erase(zann));
        let (subj, ctx) = ana_fix_holes(ctx, subj, ty);
        let zp =
          ZPat.mk(~prefix=subj, ~z=PostOp(AnnZ_ann(NotInHole, zann)), ());
        (zp, ty, ctx);
      | PostOpZ_arg(zsubj, Ann(_, ann)) =>
        let ty = HTyp.contract(ann);
        let (zsubj, ctx) = ana_fix_holes_z(ctx, zsubj, ty);
        let zp =
          ZPat.mk(
            ~prefix=zsubj.prefix,
            ~z=?zsubj.z,
            ~suffix=zsubj.suffix @ [PostOp(Ann(NotInHole, ann))],
            (),
          );
        (zp, ty, ctx);
      | BinOpZ_op(_) => raise(ZPat.ZTile.Void_ZBinOp)
      | BinOpZ_larg(zl, OperatorHole, r) =>
        let (zl, _, ctx) = syn_fix_holes_z(ctx, zl);
        let (r, _, ctx) = syn_fix_holes(ctx, r);
        let zp =
          ZPat.mk(
            ~prefix=zl.prefix,
            ~z=?zl.z,
            ~suffix=zl.suffix @ [BinOp(OperatorHole), ...r],
            (),
          );
        (zp, Type.Hole, ctx);
      | BinOpZ_rarg(l, OperatorHole, zr) =>
        let (l, _, ctx) = syn_fix_holes(ctx, l);
        let (zr, _, ctx) = syn_fix_holes_z(ctx, zr);
        let zp =
          ZPat.mk(
            ~prefix=l @ [BinOp(OperatorHole), ...zr.prefix],
            ~z=?zr.z,
            ~suffix=zr.suffix,
            (),
          );
        (zp, Type.Hole, ctx);
      }
    }
  and ana_fix_holes_z = (ctx: Ctx.t, zp: ZPat.t, ty: Type.t): (ZPat.t, Ctx.t) => {
    let subsume = () => {
      let (zp, ty', ctx) = syn_fix_holes_z(ctx, zp);
      let zp =
        Type.consistent(ty, ty') ? zp : ZPat.put_hole_status(InHole, zp);
      (zp, ctx);
    };
    switch (ZPat.root(zp)) {
    | None =>
      let n = ZPat.z_index(zp);
      let (p, ctx) = ana_fix_holes(ctx, ZPat.erase(zp), ty);
      let zp = {
        let (prefix, suffix) = ListUtil.split_n(n, p);
        ZPat.mk(~prefix, ~suffix, ());
      };
      (zp, ctx);
    | Some(root) =>
      switch (root) {
      | OperandZ(ParenZ_body(zbody)) =>
        let (zbody, ctx) = ana_fix_holes_z(ctx, zbody, ty);
        let zp = ZPat.mk(~z=Operand(ParenZ_body(zbody)), ());
        (zp, ctx);
      | PreOpZ_op(_)
      | PreOpZ_arg(_) => raise(HPat.Tile.Void_PreOp)
      | PostOpZ_op(_, AnnZ_ann(_))
      | PostOpZ_arg(_, Ann(_)) => subsume()
      | BinOpZ_op(_) => raise(ZPat.ZTile.Void_ZBinOp)
      | BinOpZ_larg(_, OperatorHole, _)
      | BinOpZ_rarg(_, OperatorHole, _) => subsume()
      }
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
    | PreOp(Lam(status, p), body) =>
      switch (status) {
      | InHole => in_hole()
      | NotInHole =>
        let* (ty_in, ctx) = Pat.syn(ctx, p);
        let+ ty_out = syn(ctx, body);
        Type.Arrow(ty_in, ty_out);
      }
    | PostOp(fn, Ap(status, arg)) =>
      switch (status) {
      | InHole => in_hole()
      | NotInHole =>
        let* fn_ty = syn(ctx, fn);
        let* (ty_in, ty_out) = Type.matched_arrow(fn_ty);
        ana(ctx, arg, ty_in) ? Some(ty_out) : None;
      }
    | BinOp(l, Plus(status), r) =>
      switch (status) {
      | InHole => in_hole()
      | NotInHole =>
        ana(ctx, l, Type.Num) && ana(ctx, r, Type.Num) ? Some(Num) : None
      }
    | BinOp(l, OperatorHole, r) =>
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
    | PreOp(Lam(InHole, _), _) =>
      Option.is_some(syn(ctx, HExp.put_hole_status(NotInHole, e)))
    | PreOp(Lam(NotInHole, p), body) =>
      let* (ty_in, ty_out) = Type.matched_arrow(ty);
      let* ctx = Pat.ana(ctx, p, ty_in);
      ana(ctx, body, ty_out);
    | PostOp(_, Ap(_))
    | BinOp(_, OperatorHole | Plus(_), _) => subsume()
    };
  };

  let rec nth_type_info =
          (n: int, e: HExp.t, info: TypeInfo.t)
          : option((HExp.t, TypeInfo.t)) => {
    OptUtil.Syntax.(
      switch (HExp.root(e)) {
      | Operand(_) =>
        n == 0
          ? Some((e, info))
          : raise(Invalid_argument("Statics.Exp.nth_typ_info"))
      | PreOp(preop, e) =>
        n == 0
          ? Some((e, info))
          : (
            switch (info.mode, preop) {
            | (Syn, Lam(_, p))
            | (Ana(_), Lam(InHole, p)) =>
              let* (_, ctx) = Pat.syn(info.ctx, p);
              let body_info = TypeInfo.{ctx, mode: Syn};
              nth_type_info(n - 1, e, body_info);
            | (Ana(ty), Lam(NotInHole, p)) =>
              let* (ty_in, ty_out) = Type.matched_arrow(ty);
              let* ctx = Pat.ana(info.ctx, p, ty_in);
              let body_info = TypeInfo.{ctx, mode: Ana(ty_out)};
              nth_type_info(n - 1, e, body_info);
            }
          )
      | PostOp(e, postop) =>
        n == List.length(e)
          ? Some((e, info))
          : (
            switch (postop) {
            | Ap(_) =>
              let fn_info = {...info, mode: Syn};
              nth_type_info(n, e, fn_info);
            }
          )
      | BinOp(l, binop, r) =>
        let m = List.length(l);
        if (n < m) {
          switch (binop) {
          | Plus(_) => nth_type_info(n, l, {...info, mode: Ana(Num)})
          | OperatorHole => nth_type_info(n, r, {...info, mode: Syn})
          };
        } else if (n > m) {
          switch (binop) {
          | Plus(_) =>
            nth_type_info(n - (m + 1), r, {...info, mode: Ana(Num)})
          | OperatorHole =>
            nth_type_info(n - (m + 1), r, {...info, mode: Syn})
          };
        } else {
          Some((e, info));
        };
      }
    );
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
    | PreOp(preop, e) =>
      switch (preop) {
      | Lam(_, p) =>
        let (p, ty1, ctx) = Pat.syn_fix_holes(ctx, p);
        let (body, ty2) = syn_fix_holes(ctx, e);
        (
          [Tile.PreOp(HExp.Tile.Lam(NotInHole, p)), ...body],
          Arrow(ty1, ty2),
        );
      }
    | PostOp(e, postop) =>
      switch (postop) {
      | Ap(_, arg) =>
        let (fn, fn_ty) = syn_fix_holes(ctx, e);
        switch (Type.matched_arrow(fn_ty)) {
        | None =>
          let fn = HExp.put_hole_status(InHole, fn);
          let arg = ana_fix_holes(ctx, arg, Type.Hole);
          (fn @ [Tile.PostOp(HExp.Tile.Ap(NotInHole, arg))], Type.Hole);
        | Some((ty1, ty2)) =>
          let arg = ana_fix_holes(ctx, arg, ty1);
          (e @ [Tile.PostOp(HExp.Tile.Ap(NotInHole, arg))], ty2);
        };
      }
    | BinOp(e1, binop, e2) =>
      switch (binop) {
      | Plus(_) =>
        let e1 = ana_fix_holes(ctx, e1, Type.Num);
        let e2 = ana_fix_holes(ctx, e2, Type.Num);
        (e1 @ [Tile.BinOp(HExp.Tile.Plus(NotInHole)), ...e2], Type.Num);
      | OperatorHole =>
        let (e1, _) = syn_fix_holes(ctx, e1);
        let (e2, _) = syn_fix_holes(ctx, e2);
        (e1 @ [Tile.BinOp(OperatorHole), ...e2], Type.Hole);
      }
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
    | PreOp(preop, e) =>
      switch (preop) {
      | Lam(_, p) =>
        switch (Type.matched_arrow(ty)) {
        | None =>
          let (p, _, ctx) = Pat.syn_fix_holes(ctx, p);
          let (body, _) = syn_fix_holes(ctx, e);
          [Tile.PreOp(HExp.Tile.Lam(InHole, p)), ...body];
        | Some((ty1, ty2)) =>
          let (p, ctx) = Pat.ana_fix_holes(ctx, p, ty1);
          let body = ana_fix_holes(ctx, e, ty2);
          [Tile.PreOp(HExp.Tile.Lam(NotInHole, p)), ...body];
        }
      }
    | PostOp(_, postop) =>
      switch (postop) {
      | Ap(_) => subsume()
      }
    | BinOp(_, binop, _) =>
      switch (binop) {
      | OperatorHole
      | Plus(_) => subsume()
      }
    };
  };

  let rec syn_fix_holes_z = (ctx: Ctx.t, ze: ZExp.t): (ZExp.t, Type.t) =>
    switch (ZExp.root(ze)) {
    | None =>
      let n = ZExp.z_index(ze);
      let (e, ty) = syn_fix_holes(ctx, ZExp.erase(ze));
      let ze = {
        let (prefix, suffix) = ListUtil.split_n(n, e);
        ZExp.mk(~prefix, ~suffix, ());
      };
      (ze, ty);
    | Some(root) =>
      switch (root) {
      | OperandZ(ParenZ_body(zbody)) =>
        let (zbody, ty) = syn_fix_holes_z(ctx, zbody);
        let ze = ZExp.mk(~z=Operand(ParenZ_body(zbody)), ());
        (ze, ty);
      | PreOpZ_op(LamZ_pat(_, zp), body) =>
        let (zp, ty1, ctx) = Pat.syn_fix_holes_z(ctx, zp);
        let (body, ty2) = syn_fix_holes(ctx, body);
        let ze =
          ZExp.mk(~z=PreOp(LamZ_pat(NotInHole, zp)), ~suffix=body, ());
        (ze, Type.Arrow(ty1, ty2));
      | PreOpZ_arg(Lam(_, p), zbody) =>
        let (_, ctx) = Option.get(Pat.syn(ctx, p));
        let (zbody, ty) = syn_fix_holes_z(ctx, zbody);
        let ze =
          ZExp.mk(
            ~prefix=[PreOp(Lam(NotInHole, p)), ...zbody.prefix],
            ~z=?zbody.z,
            ~suffix=zbody.suffix,
            (),
          );
        (ze, ty);
      | PostOpZ_op(fn, ApZ_arg(_, zarg)) =>
        let (ty_in, ty_out) = {
          let fn_ty = Option.get(syn(ctx, fn));
          Option.get(Type.matched_arrow(fn_ty));
        };
        let zarg = ana_fix_holes_z(ctx, zarg, ty_in);
        let ze =
          ZExp.mk(~prefix=fn, ~z=PostOp(ApZ_arg(NotInHole, zarg)), ());
        (ze, ty_out);
      | PostOpZ_arg(zfn, Ap(_, arg)) =>
        let (zfn, (ty_in, ty_out)) = {
          let (zfn, fn_ty) = syn_fix_holes_z(ctx, zfn);
          (zfn, Option.get(Type.matched_arrow(fn_ty)));
        };
        let arg = ana_fix_holes(ctx, arg, ty_in);
        let ze =
          ZExp.mk(
            ~prefix=zfn.prefix,
            ~z=?zfn.z,
            ~suffix=zfn.suffix @ [PostOp(Ap(NotInHole, arg))],
            (),
          );
        (ze, ty_out);
      | BinOpZ_op(_) => raise(ZExp.ZTile.Void_ZBinOp)
      | BinOpZ_larg(zl, Plus(_), r) =>
        let zl = ana_fix_holes_z(ctx, zl, Type.Num);
        let ze =
          ZExp.mk(
            ~prefix=zl.prefix,
            ~z=?zl.z,
            ~suffix=zl.suffix @ [BinOp(Plus(NotInHole)), ...r],
            (),
          );
        (ze, Type.Num);
      | BinOpZ_rarg(l, Plus(_), zr) =>
        let zr = ana_fix_holes_z(ctx, zr, Type.Num);
        let ze =
          ZExp.mk(
            ~prefix=l @ [BinOp(Plus(NotInHole)), ...zr.prefix],
            ~z=?zr.z,
            ~suffix=zr.suffix,
            (),
          );
        (ze, Type.Num);
      | BinOpZ_larg(zl, OperatorHole, r) =>
        let (zl, _) = syn_fix_holes_z(ctx, zl);
        let ze =
          ZExp.mk(
            ~prefix=zl.prefix,
            ~z=?zl.z,
            ~suffix=zl.suffix @ [BinOp(OperatorHole), ...r],
            (),
          );
        (ze, Type.Hole);
      | BinOpZ_rarg(l, OperatorHole, zr) =>
        let (zr, _) = syn_fix_holes_z(ctx, zr);
        let ze =
          ZExp.mk(
            ~prefix=l @ [BinOp(OperatorHole), ...zr.prefix],
            ~z=?zr.z,
            ~suffix=zr.suffix,
            (),
          );
        (ze, Type.Hole);
      }
    }
  and ana_fix_holes_z = (ctx: Ctx.t, ze: ZExp.t, ty: Type.t): ZExp.t => {
    let subsume = () => {
      let (ze, ty') = syn_fix_holes_z(ctx, ze);
      Type.consistent(ty, ty') ? ze : ZExp.put_hole_status(InHole, ze);
    };
    switch (ZExp.root(ze)) {
    | None =>
      let n = ZExp.z_index(ze);
      let e = ana_fix_holes(ctx, ZExp.erase(ze), ty);
      let ze = {
        let (prefix, suffix) = ListUtil.split_n(n, e);
        ZExp.mk(~prefix, ~suffix, ());
      };
      ze;
    | Some(root) =>
      switch (root) {
      | OperandZ(ParenZ_body(zbody)) =>
        let zbody = ana_fix_holes_z(ctx, zbody, ty);
        ZExp.mk(~z=Operand(ParenZ_body(zbody)), ());
      | PreOpZ_op(LamZ_pat(_, zp), body) =>
        switch (Type.matched_arrow(ty)) {
        | None =>
          let (ze, _) = syn_fix_holes_z(ctx, ze);
          ZExp.put_hole_status(InHole, ze);
        | Some((ty_in, ty_out)) =>
          let (zp, ctx) = Pat.ana_fix_holes_z(ctx, zp, ty_in);
          let body = ana_fix_holes(ctx, body, ty_out);
          ZExp.mk(~z=PreOp(LamZ_pat(NotInHole, zp)), ~suffix=body, ());
        }
      | PreOpZ_arg(Lam(_, p), zbody) =>
        switch (Type.matched_arrow(ty)) {
        | None =>
          let (_, ctx) = Option.get(Pat.syn(ctx, p));
          let (zbody, _) = syn_fix_holes_z(ctx, zbody);
          ZExp.mk(
            ~prefix=[PreOp(Lam(InHole, p)), ...zbody.prefix],
            ~z=?zbody.z,
            ~suffix=zbody.suffix,
            (),
          );
        | Some((ty_in, ty_out)) =>
          let ctx = Option.get(Pat.ana(ctx, p, ty_in));
          let zbody = ana_fix_holes_z(ctx, zbody, ty_out);
          ZExp.mk(
            ~prefix=[PreOp(Lam(NotInHole, p)), ...zbody.prefix],
            ~z=?zbody.z,
            ~suffix=zbody.suffix,
            (),
          );
        }
      | PostOpZ_op(_, ApZ_arg(_))
      | PostOpZ_arg(_, Ap(_)) => subsume()
      | BinOpZ_op(_) => raise(ZExp.ZTile.Void_ZBinOp)
      | BinOpZ_larg(_, Plus(_) | OperatorHole, _)
      | BinOpZ_rarg(_, Plus(_) | OperatorHole, _) => subsume()
      }
    };
  };
};
