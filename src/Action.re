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
    | Paren => Some(Paren(HTyp.OperandHole))
    | Arrow => Some(Arrow);

  let rec perform = (a: t, {prefix, z, suffix}: ZTyp.t): option(ZTyp.t) => {
    let rewrap = z => ZList.{prefix, z, suffix};
    switch (z) {
    | Z =>
      switch (a) {
      | Move(Left) => ZTyp.move_left(prefix, suffix)
      | Move(Right) => ZTyp.move_left(prefix, suffix)
      | Delete =>
        switch (prefix) {
        | [] => None
        | [t, ...prefix] =>
          let (tiles, n) =
            HTyp.delete_tile_and_fix_empty_holes(prefix, t, suffix);
          let (new_prefix, new_suffix) = ListUtil.split_n(n, tiles);
          Some({prefix: new_prefix, z: Z, suffix: new_suffix});
        }
      | Construct(s) =>
        switch (tile_of_shape(s)) {
        | None => None
        | Some(tile) =>
          let (tiles, n) =
            HTyp.insert_tile_and_fix_empty_holes(prefix, tile, suffix);
          let (new_prefix, new_suffix) = ListUtil.split_n(n, tiles);
          Some({prefix: new_prefix, z: Z, suffix: new_suffix});
        }
      }
    | ParenZ_body(zbody) =>
      perform(a, zbody)
      |> Option.map(zbody => rewrap(ZTyp.ParenZ_body(zbody)))
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
    | Var(x) => Some(Var(x))
    | Paren => Some(Paren(HPat.OperandHole))
    | Ann => Some(Ann(NotInHole, HTyp.OperandHole));

  let syn_fix_and_split = (ctx, n, tiles) => {
    let (new_p, ty, ctx) =
      Statics.Pat.syn_fix_holes(ctx, HPat.parse(tiles));
    (ListUtil.split_n(n, HPat.unparse(new_p)), ty, ctx);
  };
  let ana_fix_and_split = (ctx, n, tiles, ty) => {
    let (new_p, ctx) =
      Statics.Pat.ana_fix_holes(ctx, HPat.parse(tiles), ty);
    (ListUtil.split_n(n, HPat.unparse(new_p)), ctx);
  };

  let rec syn_perform =
          (ctx: Ctx.t, a: t, {prefix, z, suffix} as zp: ZPat.t)
          : option((ZPat.t, HTyp.t, Ctx.t)) => {
    switch (z) {
    | None =>
      switch (a) {
      | Move(direction) =>
        move(direction, prefix, suffix)
        |> Option.map(zp => {
             let (_, ty, ctx) =
               Statics.Pat.syn_fix_holes(ctx, HPat.parse(prefix @ suffix));
             (zp, ty, ctx);
           })
      | Delete =>
        ListUtil.split_last_opt(prefix)
        |> Option.map(((prefix, t)) => {
             let (tiles, n) =
               HPat.delete_tile_and_fix_empty_holes(prefix, t, suffix);
             let ((new_prefix, new_suffix), ty, ctx) =
               syn_fix_and_split(ctx, n, tiles);
             let new_zp: ZPat.t = {
               prefix: new_prefix,
               z: Z,
               suffix: new_suffix,
             };
             (new_zp, ty, ctx);
           })
      | Construct(s) =>
        tile_of_shape(s)
        |> Option.map(tile => {
             let (tiles, n) =
               HPat.insert_tile_and_fix_empty_holes(prefix, tile, suffix);
             let ((new_prefix, new_suffix), ty, ctx) =
               syn_fix_and_split(ctx, n, tiles);
             let new_zp: ZPat.t = {
               prefix: new_prefix,
               z: Z,
               suffix: new_suffix,
             };
             (new_zp, ty, ctx);
           })
      }
    | Some(ztile) =>
      let (root_index, root_tile) = HPat.Tile.root(ZPat.erase(zp));
      let z_index = List.length(prefix);
      if (z_index == root_index) {
        switch (ztile) {
        | ParenZ_body(zbody) =>
          syn_perform(ctx, a, zbody)
          |> Option.map(((zbody, ty, ctx)) =>
               (ZTiles.wrap(ZPat.ParenZ_body(zbody)), ty, ctx)
             )
        | AnnZ_ann(_, zann) =>
          Typ.perform(a, zann)
          |> Option.map(zann => {
               let (subj, ctx) =
                 Statics.Pat.ana_fix_holes(ctx, HPat.parse(prefix));
               let new_prefix = HPat.unparse(subj);
               let ann = ZTyp.erase(zann);
               (
                 ZList.{prefix: new_prefix, z: Some(zann), suffix},
                 ann,
                 ctx,
               );
             })
        };
      } else {
        switch (root_tile) {
        | OperandHole
        | Var(_)
        | Paren(_) => assert(false)
        };
      };
    };
  };

  let rec syn_perform =
          (ctx: Ctx.t, a: t, {prefix, z, suffix} as zp: ZPat.t)
          : option((ZPat.t, HTyp.t, Ctx.t)) => {
    let rewrap = z => ZList.{prefix, z, suffix};
    let zroot = List.length(prefix);
    let (zstart, zend) = HPat.Tile.subterm_range(zroot, p);
    switch (z) {
    | Z =>
      switch (a) {
      | Move(direction) =>
        move(direction, prefix, suffix)
        |> Option.map(zp => {
             let (_, ty, ctx) =
               Statics.Pat.syn_fix_holes(- ctx, HPat.parse(prefix @ suffix));
             (zp, ty, ctx);
           })
      | Delete =>
        ListUtil.split_last_opt(prefix)
        |> Option.map(((prefix, t)) => {
             let (tiles, n) =
               HPat.delete_tile_and_fix_empty_holes(prefix, t, suffix);
             let ((new_prefix, new_suffix), ty, ctx) =
               syn_fix_and_split(ctx, n, tiles);
             let new_zp: ZPat.t = {
               prefix: new_prefix,
               z: Z,
               suffix: new_suffix,
             };
             (new_zp, ty, ctx);
           })
      | Construct(s) =>
        tile_of_shape(s)
        |> Option.map(tile => {
             let (tiles, n) =
               HPat.insert_tile_and_fix_empty_holes(prefix, tile, suffix);
             let ((new_prefix, new_suffix), ty, ctx) =
               syn_fix_and_split(ctx, n, tiles);
             let new_zp: ZPat.t = {
               prefix: new_prefix,
               z: Z,
               suffix: new_suffix,
             };
             (new_zp, ty, ctx);
           })
      }
    | ParenZ_body(zbody) =>
      syn_perform(ctx, a, zbody)
      |> Option.map(((zbody, ty, ctx)) =>
           ({prefix, z: ZPat.ParenZ_body(zbody), suffix}, ty, ctx)
         )
    | AnnZ_ann(_, zann) =>
      Typ.perform(a, zann)
      |> Option.map(zann => {
           let ann = ZTyp.erase(zann);
           let new_prefix =
             prefix |> ListUtil.map_sublist(zstart, zend, tiles => {});

           let (subj, ctx) = Statics.Pat.ana_fix_holes(ctx, subj, ann);
           (rewrap(ZPat.AnnZ_ann(NotInHole, subj, zann)), ann, ctx);
         })
    };
  }
  and ana_perform =
      (ctx: Ctx.t, a: t, {prefix, z, suffix}: ZPat.t, ty: HTyp.t)
      : option((ZPat.t, Ctx.t)) =>
    switch (z) {
    | Z =>
      switch (a) {
      | Move(direction) =>
        move(direction, prefix, suffix)
        |> Option.map(zp => {
             let (_, ctx) =
               Statics.Pat.ana_fix_holes(
                 ctx,
                 HPat.parse(List.rev(prefix) @ suffix),
                 ty,
               );
             (zp, ctx);
           })
      | Delete =>
        ListUtil.split_last_opt(prefix)
        |> Option.map(((prefix, t)) => {
             let (tiles, n) =
               HPat.delete_tile_and_fix_empty_holes(prefix, t, suffix);
             let ((new_prefix, new_suffix), ctx) =
               ana_fix_and_split(ctx, n, tiles, ty);
             let new_zp: ZPat.t = {
               prefix: new_prefix,
               z: Z,
               suffix: new_suffix,
             };
             (new_zp, ctx);
           })
      | Construct(s) =>
        switch (tile_of_shape(s)) {
        | None => None
        | Some(tile) =>
          let (tiles, n) =
            HPat.insert_tile_and_fix_empty_holes(prefix, tile, suffix);
          let ((new_prefix, new_suffix), ctx) =
            ana_fix_and_split(ctx, n, tiles, ty);
          Some((Y(new_prefix, new_suffix), ctx));
        }
      }
    | Z(z) =>
      ana_perform_unzipped(ctx, a, z, ty)
      |> Option.map(((z, ctx)) => (ZPat.Z(z), ctx))
    }
  and ana_perform_unzipped =
      (ctx: Ctx.t, a: t, z: ZPat.unzipped, ty: HTyp.t)
      : option((ZPat.unzipped, Ctx.t)) =>
    switch (z) {
    | ParenZ(zbody) =>
      ana_perform(ctx, a, zbody, ty)
      |> Option.map(((zbody, ctx)) => (ZPat.ParenZ(zbody), ctx))
    | AnnZ_subj(_)
    | AnnZ_ann(_)
    | OperatorHoleZ_l(_)
    | OperatorHoleZ_r(_) =>
      syn_perform_unzipped(ctx, a, z)
      |> Option.map(((z, ty', ctx)) => {
           let z =
             HTyp.consistent(ty, ty')
               ? z : ZPat.set_hole_status_unzipped(InHole, z);
           (z, ctx);
         })
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
    | Num(n) => Some(Num(NotInHole, n))
    | Var(x) => Some(Var(NotInHole, x))
    | Paren => Some(Paren(HExp.OperandHole))
    | Lam => Some(Lam(NotInHole, HPat.OperandHole))
    | Ap => Some(Ap(NotInHole, HExp.OperandHole))
    | Plus => Some(Plus(NotInHole));

  let syn_fix_and_split = (ctx, n, tiles) => {
    let (new_e, ty) = Statics.Exp.syn_fix_holes(ctx, HExp.parse(tiles));
    (ListUtil.split_n(n, HExp.unparse(new_e)), ty);
  };
  let ana_fix_and_split = (ctx, n, tiles, ty) => {
    let new_e = Statics.Exp.ana_fix_holes(ctx, HExp.parse(tiles), ty);
    ListUtil.split_n(n, HExp.unparse(new_e));
  };

  let rec syn_perform =
          (ctx: Ctx.t, a: t, ze: ZExp.t): option((ZExp.t, HTyp.t)) =>
    switch (ze) {
    | Y(prefix, suffix) =>
      switch (a) {
      | Move(direction) =>
        move(direction, prefix, suffix)
        |> Option.map(ze => {
             let (_, ty) =
               Statics.Exp.syn_fix_holes(
                 ctx,
                 HExp.parse(List.rev(prefix) @ suffix),
               );
             (ze, ty);
           })
      // z + ( x + y )|
      // --> z + x + y |
      | Delete =>
        switch (prefix) {
        | [] => None
        | [t, ...prefix] =>
          let (tiles, n) =
            HExp.delete_tile_and_fix_empty_holes(prefix, t, suffix);
          let ((new_prefix, new_suffix), ty) =
            syn_fix_and_split(ctx, n, tiles);
          Some((Y(new_prefix, new_suffix), ty));
        }
      | Construct(s) =>
        switch (tile_of_shape(s)) {
        | None => None
        | Some(tile) =>
          let (tiles, n) =
            HExp.insert_tile_and_fix_empty_holes(prefix, tile, suffix);
          let ((new_prefix, new_suffix), ty) =
            syn_fix_and_split(ctx, n, tiles);
          Some((Y(new_prefix, new_suffix), ty));
        }
      }
    | Z(z) =>
      syn_perform_unzipped(ctx, a, z)
      |> Option.map(((z, ty)) => (ZExp.Z(z), ty))
    }
  and syn_perform_unzipped =
      (ctx: Ctx.t, a: t, z: ZExp.unzipped): option((ZExp.unzipped, HTyp.t)) =>
    switch (z) {
    | ParenZ(zbody) =>
      syn_perform(ctx, a, zbody)
      |> Option.map(((zbody, ty)) => (ZExp.ParenZ(zbody), ty))
    | LamZ_pat(_, zp, body) =>
      Option.bind(
        Pat.syn_perform(ctx, a, zp),
        ((zp, ty1, ctx)) => {
          let (body, ty2) = Statics.Exp.syn_fix_holes(ctx, body);
          Some((ZExp.LamZ_pat(NotInHole, zp, body), HTyp.Arrow(ty1, ty2)));
        },
      )
    | LamZ_body(_, p, zbody) =>
      Option.bind(Statics.Pat.syn(ctx, p), ((_, ctx)) =>
        syn_perform_unzipped(ctx, a, zbody)
        |> Option.map(((zbody, ty)) =>
             (ZExp.LamZ_body(NotInHole, p, zbody), ty)
           )
      )
    | ApZ_fn(_, zfn, arg) =>
      Option.bind(syn_perform_unzipped(ctx, a, zfn), ((zfn, fn_ty)) =>
        HTyp.matched_arrow(fn_ty)
        |> Option.map(((ty1, ty2)) => {
             let arg = Statics.Exp.ana_fix_holes(ctx, arg, ty1);
             (ZExp.ApZ_fn(NotInHole, zfn, arg), ty2);
           })
      )
    | ApZ_arg(_, fn, zarg) =>
      switch (Statics.Exp.syn(ctx, fn)) {
      | None => None
      | Some(fn_ty) =>
        switch (HTyp.matched_arrow(fn_ty)) {
        | None => None
        | Some((ty1, ty2)) =>
          ana_perform(ctx, a, zarg, ty1)
          |> Option.map(zarg => (ZExp.ApZ_arg(NotInHole, fn, zarg), ty2))
        }
      }
    | PlusZ_l(_, zl, r) =>
      ana_perform_unzipped(ctx, a, zl, HTyp.Num)
      |> Option.map(zl => (ZExp.PlusZ_l(NotInHole, zl, r), HTyp.Num))
    | PlusZ_r(_, l, zr) =>
      ana_perform_unzipped(ctx, a, zr, HTyp.Num)
      |> Option.map(zr => (ZExp.PlusZ_r(NotInHole, l, zr), HTyp.Num))
    | OperatorHoleZ_l(zl, r) =>
      syn_perform_unzipped(ctx, a, zl)
      |> Option.map(((zl, _)) =>
           (ZExp.OperatorHoleZ_l(zl, r), HTyp.OperandHole)
         )
    | OperatorHoleZ_r(l, zr) =>
      syn_perform_unzipped(ctx, a, zr)
      |> Option.map(((zr, _)) =>
           (ZExp.OperatorHoleZ_r(l, zr), HTyp.OperandHole)
         )
    }
  and ana_perform =
      (ctx: Ctx.t, a: t, ze: ZExp.t, ty: HTyp.t): option(ZExp.t) =>
    switch (ze) {
    | Y(prefix, suffix) =>
      switch (a) {
      | Move(direction) => move(direction, prefix, suffix)
      | Delete =>
        switch (prefix) {
        | [] => None
        | [t, ...prefix] =>
          let (tiles, n) =
            HExp.delete_tile_and_fix_empty_holes(prefix, t, suffix);
          let (new_prefix, new_suffix) =
            ana_fix_and_split(ctx, n, tiles, ty);
          Some(Y(new_prefix, new_suffix));
        }
      | Construct(s) =>
        switch (tile_of_shape(s)) {
        | None => None
        | Some(tile) =>
          let (tiles, n) =
            HExp.insert_tile_and_fix_empty_holes(prefix, tile, suffix);
          let (new_prefix, new_suffix) =
            ana_fix_and_split(ctx, n, tiles, ty);
          Some(Y(new_prefix, new_suffix));
        }
      }
    | Z(z) =>
      ana_perform_unzipped(ctx, a, z, ty) |> Option.map(z => ZExp.Z(z))
    }
  and ana_perform_unzipped =
      (ctx: Ctx.t, a: t, z: ZExp.unzipped, ty: HTyp.t): option(ZExp.unzipped) =>
    switch (z) {
    | ParenZ(zbody) =>
      ana_perform(ctx, a, zbody, ty)
      |> Option.map(zbody => ZExp.ParenZ(zbody))
    | LamZ_pat(_, zp, body) =>
      switch (HTyp.matched_arrow(ty)) {
      | None =>
        syn_perform_unzipped(ctx, a, z)
        |> Option.map(((z, _)) => ZExp.set_hole_status_unzipped(InHole, z))
      | Some((ty1, ty2)) =>
        Pat.ana_perform(ctx, a, zp, ty1)
        |> Option.map(((zp, ctx)) => {
             let body = Statics.Exp.ana_fix_holes(ctx, body, ty2);
             ZExp.LamZ_pat(NotInHole, zp, body);
           })
      }
    | LamZ_body(_, p, zbody) =>
      switch (HTyp.matched_arrow(ty)) {
      | None =>
        Option.bind(Statics.Pat.syn(ctx, p), ((_, ctx)) =>
          syn_perform_unzipped(ctx, a, zbody)
          |> Option.map(((zbody, _)) => ZExp.LamZ_body(InHole, p, zbody))
        )
      | Some((ty1, ty2)) =>
        Option.bind(Statics.Pat.ana(ctx, p, ty1), ctx =>
          ana_perform_unzipped(ctx, a, zbody, ty2)
          |> Option.map(zbody => ZExp.LamZ_body(NotInHole, p, zbody))
        )
      }
    | ApZ_fn(_)
    | ApZ_arg(_)
    | PlusZ_l(_)
    | PlusZ_r(_)
    | OperatorHoleZ_l(_)
    | OperatorHoleZ_r(_) =>
      syn_perform_unzipped(ctx, a, z)
      |> Option.map(((z, ty')) =>
           HTyp.consistent(ty, ty')
             ? z : ZExp.set_hole_status_unzipped(InHole, z)
         )
    };
};
