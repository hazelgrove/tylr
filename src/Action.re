type direction =
  | Left
  | Right;

type tile_shape =
  | Num(int)
  | Var(Var.t)
  | Paren
  | Lam
  | Ap
  | Ann
  | Plus
  | Arrow;

type t =
  | Move(direction)
  // Mark
  | Delete
  | Construct(tile_shape);

module Typ = {
  let tile_of_shape: tile_shape => option(HTyp.Tile.t) =
    fun
    | Num(_)
    | Lam
    | Ap
    | Plus
    | Var(_)
    | Ann => None
    | Paren => Some(Operand(Paren(HTyp.mk_hole())))
    | Arrow => Some(BinOp(Arrow));

  let rec perform = (a: t, {prefix, z, suffix}: ZTyp.t): option(ZTyp.t) => {
    switch (z) {
    | None =>
      switch (a) {
      | Move(Left) => ZTyp.move_left(prefix, suffix)
      | Move(Right) => ZTyp.move_right(prefix, suffix)
      | Delete =>
        switch (prefix) {
        | [] => None
        | [t, ...prefix] =>
          let (tiles, n) =
            HTyp.delete_tile_and_fix_empty_holes(prefix, t, suffix);
          let (new_prefix, new_suffix) = ListUtil.split_n(n, tiles);
          Some({prefix: new_prefix, z: None, suffix: new_suffix});
        }
      | Construct(s) =>
        switch (tile_of_shape(s)) {
        | None => None
        | Some(tile) =>
          let (tiles, n) =
            HTyp.insert_tile_and_fix_empty_holes(prefix, tile, suffix);
          let (new_prefix, new_suffix) = ListUtil.split_n(n, tiles);
          Some({prefix: new_prefix, z: None, suffix: new_suffix});
        }
      }
    | Some(ZOperand(ParenZ_body(zbody))) =>
      perform(a, zbody)
      |> Option.map((zbody: ZTyp.t) =>
           (
             {prefix, z: Some(ZOperand(ParenZ_body(zbody))), suffix}: ZTyp.t
           )
         )
    | Some(ZPreOp(_)) => raise(ZTyp.ZTile.Void_ZPreOp)
    | Some(ZPostOp(_)) => raise(ZTyp.ZTile.Void_ZPostOp)
    | Some(ZBinOp(_)) => raise(ZTyp.ZTile.Void_ZBinOp)
    };
  };
};

module Pat = {
  let move =
    fun
    | Left => ZPat.move_left
    | Right => ZPat.move_right;

  let tile_of_shape: tile_shape => option(HPat.Tile.t) =
    fun
    | Num(_)
    | Lam
    | Ap
    | Plus
    | Arrow => None
    | Var(x) => Some(Operand(Var(x)))
    | Paren => Some(Operand(Paren(HPat.mk_hole())))
    | Ann => Some(PostOp(Ann(NotInHole, HTyp.mk_hole())));

  let syn_fix_and_split = (ctx, n, p) => {
    let (p, ty, ctx) = Statics.Pat.syn_fix_holes(ctx, p);
    (ListUtil.split_n(n, p), ty, ctx);
  };
  let ana_fix_and_split = (ctx, n, p, ty) => {
    let (p, ctx) = Statics.Pat.ana_fix_holes(ctx, p, ty);
    (ListUtil.split_n(n, p), ctx);
  };

  let rec syn_perform =
          (ctx: Ctx.t, a: t, zp: ZPat.t): option((ZPat.t, Type.t, Ctx.t)) =>
    switch (ZPat.root(zp)) {
    | None =>
      let ZList.{prefix, suffix, _} = zp;
      switch (a) {
      | Move(direction) =>
        Option.bind(move(direction, prefix, suffix), zp =>
          Statics.Pat.syn(ctx, ZPat.erase(zp))
          |> Option.map(((ty, ctx)) => (zp, ty, ctx))
        )
      | Delete =>
        ListUtil.split_last_opt(prefix)
        |> Option.map(((prefix, t)) => {
             let (tiles, n) =
               HPat.delete_tile_and_fix_empty_holes(prefix, t, suffix);
             let ((prefix, suffix), ty, ctx) =
               syn_fix_and_split(ctx, n, tiles);
             let new_zp = ZPat.mk(~prefix, ~suffix, ());
             (new_zp, ty, ctx);
           })
      | Construct(s) =>
        tile_of_shape(s)
        |> Option.map(tile => {
             let (tiles, n) =
               HPat.insert_tile_and_fix_empty_holes(prefix, tile, suffix);
             let ((prefix, suffix), ty, ctx) =
               syn_fix_and_split(ctx, n, tiles);
             let zp = ZPat.mk(~prefix, ~suffix, ());
             (zp, ty, ctx);
           })
      };
    | Some(root) =>
      switch (root) {
      | OperandZ(ParenZ_body(zbody)) =>
        syn_perform(ctx, a, zbody)
        |> Option.map(((zbody, ty, ctx)) => {
             let zp = ZPat.mk(~z=ZOperand(ParenZ_body(zbody)), ());
             (zp, ty, ctx);
           })
      | PreOpZ_op(_)
      | PreOpZ_arg(_) => raise(HPat.Tile.Void_PreOp)
      | PostOpZ_op(subj, AnnZ_ann(_, zann)) =>
        Typ.perform(a, zann)
        |> Option.map(zann => {
             let ty = HTyp.contract(ZTyp.erase(zann));
             let (subj, ctx) = Statics.Pat.ana_fix_holes(ctx, subj, ty);
             let zp =
               ZPat.mk(
                 ~prefix=subj,
                 ~z=ZPostOp(AnnZ_ann(NotInHole, zann)),
                 (),
               );
             (zp, ty, ctx);
           })
      | PostOpZ_arg(zsubj, Ann(_, ann)) =>
        let ty = HTyp.contract(ann);
        ana_perform(ctx, a, zsubj, ty)
        |> Option.map(((zsubj: ZPat.t, ctx)) => {
             let zp =
               ZPat.mk(
                 ~prefix=zsubj.prefix,
                 ~z=?zsubj.z,
                 ~suffix=zsubj.suffix @ [PostOp(Ann(NotInHole, ann))],
                 (),
               );
             (zp, ty, ctx);
           });
      | BinOpZ_op(_) => raise(ZPat.ZTile.Void_ZBinOp)
      | BinOpZ_larg(zl, OperatorHole, r) =>
        Option.bind(syn_perform(ctx, a, zl), ((zl, _, ctx)) =>
          Statics.Pat.syn(ctx, r)
          |> Option.map(((_, ctx)) => {
               let zp =
                 ZPat.mk(
                   ~prefix=zl.prefix,
                   ~z=?zl.z,
                   ~suffix=zl.suffix @ [BinOp(OperatorHole), ...r],
                   (),
                 );
               (zp, Type.Hole, ctx);
             })
        )
      | BinOpZ_rarg(l, OperatorHole, zr) =>
        Option.bind(Statics.Pat.syn(ctx, l), ((_, ctx)) =>
          syn_perform(ctx, a, zr)
          |> Option.map(((zr: ZPat.t, _, ctx)) => {
               let zp =
                 ZPat.mk(
                   ~prefix=l @ [BinOp(OperatorHole), ...zr.prefix],
                   ~z=?zr.z,
                   ~suffix=zr.suffix,
                   (),
                 );
               (zp, Type.Hole, ctx);
             })
        )
      }
    }
  and ana_perform =
      (ctx: Ctx.t, a: t, zp: ZPat.t, ty: Type.t): option((ZPat.t, Ctx.t)) => {
    let subsume = () =>
      syn_perform(ctx, a, zp)
      |> Option.map(((zp, ty', ctx)) => {
           let zp =
             Type.consistent(ty, ty')
               ? zp : ZPat.set_hole_status(InHole, zp);
           (zp, ctx);
         });
    switch (ZPat.root(zp)) {
    | None =>
      let ZList.{prefix, suffix, _} = zp;
      switch (a) {
      | Move(direction) =>
        Option.bind(move(direction, prefix, suffix), zp =>
          Statics.Pat.ana(ctx, ZPat.erase(zp), ty)
          |> Option.map(ctx => (zp, ctx))
        )
      | Delete =>
        ListUtil.split_last_opt(prefix)
        |> Option.map(((prefix, t)) => {
             let (tiles, n) =
               HPat.delete_tile_and_fix_empty_holes(prefix, t, suffix);
             let ((prefix, suffix), ctx) =
               ana_fix_and_split(ctx, n, tiles, ty);
             let zp = ZPat.mk(~prefix, ~suffix, ());
             (zp, ctx);
           })
      | Construct(s) =>
        switch (tile_of_shape(s)) {
        | None => None
        | Some(tile) =>
          let (tiles, n) =
            HPat.insert_tile_and_fix_empty_holes(prefix, tile, suffix);
          let ((prefix, suffix), ctx) =
            ana_fix_and_split(ctx, n, tiles, ty);
          let zp = ZPat.mk(~prefix, ~suffix, ());
          Some((zp, ctx));
        }
      };
    | Some(root) =>
      switch (root) {
      | OperandZ(ParenZ_body(zbody)) =>
        ana_perform(ctx, a, zbody, ty)
        |> Option.map(((zbody, ctx)) => {
             let zp = ZPat.mk(~z=ZOperand(ParenZ_body(zbody)), ());
             (zp, ctx);
           })
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
  let move =
    fun
    | Left => ZExp.move_left
    | Right => ZExp.move_right;

  let tile_of_shape: tile_shape => option(HExp.Tile.t) =
    fun
    | Ann
    | Arrow => None
    | Num(n) => Some(Operand(Num(NotInHole, n)))
    | Var(x) => Some(Operand(Var(NotInHole, x)))
    | Paren => Some(Operand(Paren(HExp.mk_hole())))
    | Lam => Some(PreOp(Lam(NotInHole, HPat.mk_hole())))
    | Ap => Some(PostOp(Ap(NotInHole, HExp.mk_hole())))
    | Plus => Some(BinOp(Plus(NotInHole)));

  let syn_fix_and_split = (ctx, n, e) => {
    let (e, ty) = Statics.Exp.syn_fix_holes(ctx, e);
    (ListUtil.split_n(n, e), ty);
  };
  let ana_fix_and_split = (ctx, n, e, ty) => {
    let e = Statics.Exp.ana_fix_holes(ctx, e, ty);
    ListUtil.split_n(n, e);
  };

  let rec syn_perform =
          (ctx: Ctx.t, a: t, ze: ZExp.t): option((ZExp.t, Type.t)) =>
    switch (ZExp.root(ze)) {
    | None =>
      let ZList.{prefix, suffix, _} = ze;
      switch (a) {
      | Move(direction) =>
        Option.bind(move(direction, prefix, suffix), ze =>
          Statics.Exp.syn(ctx, ZExp.erase(ze)) |> Option.map(ty => (ze, ty))
        )
      // z + ( x + y )|
      // --> z + x + y |
      | Delete =>
        switch (prefix) {
        | [] => None
        | [t, ...prefix] =>
          let (tiles, n) =
            HExp.delete_tile_and_fix_empty_holes(prefix, t, suffix);
          let ((prefix, suffix), ty) = syn_fix_and_split(ctx, n, tiles);
          let ze = ZExp.mk(~prefix, ~suffix, ());
          Some((ze, ty));
        }
      | Construct(s) =>
        switch (tile_of_shape(s)) {
        | None => None
        | Some(tile) =>
          let (tiles, n) =
            HExp.insert_tile_and_fix_empty_holes(prefix, tile, suffix);
          let ((prefix, suffix), ty) = syn_fix_and_split(ctx, n, tiles);
          let ze = ZExp.mk(~prefix, ~suffix, ());
          Some((ze, ty));
        }
      };
    | Some(root) =>
      switch (root) {
      | OperandZ(ParenZ_body(zbody)) =>
        syn_perform(ctx, a, zbody)
        |> Option.map(((zbody, ty)) => {
             let ze = ZExp.mk(~z=ZOperand(ParenZ_body(zbody)), ());
             (ze, ty);
           })
      | PreOpZ_op(LamZ_pat(_, zp), body) =>
        Pat.syn_perform(ctx, a, zp)
        |> Option.map(((zp, ty1, ctx)) => {
             let (body, ty2) = Statics.Exp.syn_fix_holes(ctx, body);
             let ze =
               ZExp.mk(
                 ~z=ZPreOp(LamZ_pat(NotInHole, zp)),
                 ~suffix=body,
                 (),
               );
             (ze, Type.Arrow(ty1, ty2));
           })
      | PreOpZ_arg(Lam(_, p), zbody) =>
        Option.bind(Statics.Pat.syn(ctx, p), ((_, ctx)) =>
          syn_perform(ctx, a, zbody)
          |> Option.map(((zbody: ZExp.t, ty)) => {
               let ze =
                 ZExp.mk(
                   ~prefix=[PreOp(Lam(NotInHole, p)), ...zbody.prefix],
                   ~z=?zbody.z,
                   ~suffix=zbody.suffix,
                   (),
                 );
               (ze, ty);
             })
        )
      | PostOpZ_op(fn, ApZ_arg(_, zarg)) =>
        Option.bind(Statics.Exp.syn(ctx, fn), fn_ty =>
          Option.bind(Type.matched_arrow(fn_ty), ((ty1, ty2)) =>
            ana_perform(ctx, a, zarg, ty1)
            |> Option.map(zarg => {
                 let ze =
                   ZExp.mk(
                     ~prefix=fn,
                     ~z=ZPostOp(ApZ_arg(NotInHole, zarg)),
                     (),
                   );
                 (ze, ty2);
               })
          )
        )
      | PostOpZ_arg(zfn, Ap(_, arg)) =>
        Option.bind(syn_perform(ctx, a, zfn), ((zfn, fn_ty)) =>
          Type.matched_arrow(fn_ty)
          |> Option.map(((ty1, ty2)) => {
               let arg = Statics.Exp.ana_fix_holes(ctx, arg, ty1);
               let ze =
                 ZExp.mk(
                   ~prefix=zfn.prefix,
                   ~z=?zfn.z,
                   ~suffix=zfn.suffix @ [PostOp(Ap(NotInHole, arg))],
                   (),
                 );
               (ze, ty2);
             })
        )
      | BinOpZ_op(_) => raise(ZExp.ZTile.Void_ZBinOp)
      | BinOpZ_larg(zl, Plus(_), r) =>
        ana_perform(ctx, a, zl, Type.Num)
        |> Option.map((zl: ZExp.t) => {
             let ze =
               ZExp.mk(
                 ~prefix=zl.prefix,
                 ~z=?zl.z,
                 ~suffix=zl.suffix @ [BinOp(Plus(NotInHole)), ...r],
                 (),
               );
             (ze, Type.Num);
           })
      | BinOpZ_rarg(l, Plus(_), zr) =>
        ana_perform(ctx, a, zr, Type.Num)
        |> Option.map((zr: ZExp.t) => {
             let ze =
               ZExp.mk(
                 ~prefix=l @ [BinOp(Plus(NotInHole)), ...zr.prefix],
                 ~z=?zr.z,
                 ~suffix=zr.suffix,
                 (),
               );
             (ze, Type.Num);
           })
      | BinOpZ_larg(zl, OperatorHole, r) =>
        syn_perform(ctx, a, zl)
        |> Option.map(((zl: ZExp.t, _)) => {
             let ze =
               ZExp.mk(
                 ~prefix=zl.prefix,
                 ~z=?zl.z,
                 ~suffix=zl.suffix @ [BinOp(OperatorHole), ...r],
                 (),
               );
             (ze, Type.Hole);
           })
      | BinOpZ_rarg(l, OperatorHole, zr) =>
        syn_perform(ctx, a, zr)
        |> Option.map(((zr: ZExp.t, _)) => {
             let ze =
               ZExp.mk(
                 ~prefix=l @ [BinOp(OperatorHole), ...zr.prefix],
                 ~z=?zr.z,
                 ~suffix=zr.suffix,
                 (),
               );
             (ze, Type.Hole);
           })
      }
    }
  and ana_perform =
      (ctx: Ctx.t, a: t, ze: ZExp.t, ty: Type.t): option(ZExp.t) => {
    let subsume = () =>
      syn_perform(ctx, a, ze)
      |> Option.map(((ze, ty')) =>
           Type.consistent(ty, ty') ? ze : ZExp.set_hole_status(InHole, ze)
         );
    switch (ZExp.root(ze)) {
    | None =>
      let ZList.{prefix, suffix, _} = ze;
      switch (a) {
      | Move(direction) =>
        Option.bind(move(direction, prefix, suffix), ze =>
          Statics.Exp.ana(ctx, ZExp.erase(ze), ty) ? Some(ze) : None
        )
      | Delete =>
        switch (prefix) {
        | [] => None
        | [t, ...prefix] =>
          let (tiles, n) =
            HExp.delete_tile_and_fix_empty_holes(prefix, t, suffix);
          let (prefix, suffix) = ana_fix_and_split(ctx, n, tiles, ty);
          Some(ZExp.mk(~prefix, ~suffix, ()));
        }
      | Construct(s) =>
        switch (tile_of_shape(s)) {
        | None => None
        | Some(tile) =>
          let (tiles, n) =
            HExp.insert_tile_and_fix_empty_holes(prefix, tile, suffix);
          let (prefix, suffix) = ana_fix_and_split(ctx, n, tiles, ty);
          Some(ZExp.mk(~prefix, ~suffix, ()));
        }
      };
    | Some(root) =>
      switch (root) {
      | OperandZ(ParenZ_body(zbody)) =>
        ana_perform(ctx, a, zbody, ty)
        |> Option.map(zbody => ZExp.mk(~z=ZOperand(ParenZ_body(zbody)), ()))
      | PreOpZ_op(LamZ_pat(_, zp), body) =>
        switch (Type.matched_arrow(ty)) {
        | None =>
          syn_perform(ctx, a, ze)
          |> Option.map(((ze, _)) => ZExp.set_hole_status(InHole, ze))
        | Some((ty1, ty2)) =>
          Pat.ana_perform(ctx, a, zp, ty1)
          |> Option.map(((zp, ctx)) => {
               let body = Statics.Exp.ana_fix_holes(ctx, body, ty2);
               ZExp.mk(
                 ~z=ZPreOp(LamZ_pat(NotInHole, zp)),
                 ~suffix=body,
                 (),
               );
             })
        }
      | PreOpZ_arg(Lam(_, p), zbody) =>
        switch (Type.matched_arrow(ty)) {
        | None =>
          Option.bind(Statics.Pat.syn(ctx, p), ((_, ctx)) =>
            syn_perform(ctx, a, zbody)
            |> Option.map(((zbody: ZExp.t, _)) =>
                 ZExp.mk(
                   ~prefix=[PreOp(Lam(InHole, p)), ...zbody.prefix],
                   ~z=?zbody.z,
                   ~suffix=zbody.suffix,
                   (),
                 )
               )
          )
        | Some((ty1, ty2)) =>
          Option.bind(Statics.Pat.ana(ctx, p, ty1), ctx =>
            ana_perform(ctx, a, zbody, ty2)
            |> Option.map((zbody: ZExp.t) =>
                 ZExp.mk(
                   ~prefix=[PreOp(Lam(NotInHole, p)), ...zbody.prefix],
                   ~z=?zbody.z,
                   ~suffix=zbody.suffix,
                   (),
                 )
               )
          )
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
