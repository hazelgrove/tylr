module Exp = {
  type t('z) = {
    ctx: Ctx.t,
    mode: mode('z),
  }
  and mode('z) =
    | Syn({
        fn_pos: bool,
        fix: Type.t => 'z,
      })
    | Ana({
        expected: Type.t,
        fix: unit => 'z,
      });

  let map = (f: 'y => 'z, info: t('y)): t('z) => {
    ctx: info.ctx,
    mode:
      switch (info.mode) {
      | Syn({fn_pos, fix}) => Syn({fn_pos, fix: ty => f(fix(ty))})
      | Ana({expected, fix}) => Ana({expected, fix: () => f(fix())})
      },
  };

  let rec mk = (ze: ZExp.t): option(t(ZExp.t)) => {
    open OptUtil.Syntax;
    let (n, (e, zrest)) = ZPath.Exp.zip(HExp.dummy_hole, ze);
    let* info =
      switch (zrest) {
      | None =>
        Some({
          ctx: Ctx.empty,
          mode: Syn({fn_pos: false, fix: _ => ZExp.mk()}),
        })
      | Some(ztile) =>
        let+ info = mk_ztile(ztile);
        map(ztile => ZExp.mk(~z=ztile, ()), info);
      };

    let rec go = (e: HExp.t, info: t(ZExp.t)): option(t(ZExp.t)) =>
      switch (HExp.root(e)) {
      | Operand(_) =>
        // dummy hole
        Some(info)
      | PreOp((Lam(status, p), body)) =>
        let* body_info =
          switch (info.mode) {
          | Syn({fn_pos: _, fix}) =>
            let+ (ty_in, ctx) = Statics.Pat.syn(info.ctx, p);
            let fix = ty_out => {
              let ze = fix(Arrow(ty_in, ty_out));
              {...ze, prefix: ze.prefix @ [PreOp(Lam(NotInHole, p))]};
            };
            {ctx, mode: Syn({fn_pos: false, fix})};
          | Ana({expected, fix}) =>
            switch (Type.matched_arrow(expected), status) {
            | (None, NotInHole) => None
            | (Some((ty_in, ty_out)), _) =>
              let+ ctx = Statics.Pat.ana(info.ctx, p, ty_in);
              let fix = () => {
                let ze = fix();
                {...ze, prefix: ze.prefix @ [PreOp(Lam(NotInHole, p))]};
              };
              {ctx, mode: Ana({expected: ty_out, fix})};
            | (_, InHole) =>
              let+ (_, ctx) = Statics.Pat.syn(info.ctx, p);
              let fix = _ => {
                let ze = fix();
                {...ze, prefix: ze.prefix @ [PreOp(Lam(InHole, p))]};
              };
              {ctx, mode: Syn({fn_pos: false, fix})};
            }
          };
        go(body, body_info);
      | PostOp((fn, Ap(_, arg))) =>
        let fn_mode =
          Syn({
            fn_pos: true,
            fix: fn_ty => {
              let (ty_in, ty_out) =
                OptUtil.get(
                  () =>
                    failwith("expected client to return matched arrow type"),
                  Type.matched_arrow(fn_ty),
                );
              switch (info.mode) {
              | Syn({fn_pos, fix}) =>
                // TODO check if in fn_pos
                let arg = Statics.Exp.ana_fix_holes(info.ctx, arg, ty_in);
                let status: HoleStatus.t =
                  !fn_pos || Option.is_some(Type.matched_arrow(ty_out))
                    ? NotInHole : InHole;
                let ze = fix(ty_out);
                {
                  ...ze,
                  suffix: [
                    Tile.PostOp(HExp.Tile.Ap(status, arg)),
                    ...ze.suffix,
                  ],
                };
              | Ana({expected, fix}) =>
                let arg = Statics.Exp.ana_fix_holes(info.ctx, arg, ty_in);
                let status: HoleStatus.t =
                  Type.consistent(ty_out, expected) ? NotInHole : InHole;
                let ze = fix();
                {...ze, suffix: [PostOp(Ap(status, arg)), ...ze.suffix]};
              };
            },
          });
        go(fn, {ctx: info.ctx, mode: fn_mode});
      | BinOp((l, Plus(_), r)) =>
        let is_left = n < List.length(l);
        let operand_mode =
          Ana({
            expected: Num,
            fix: () =>
              switch (info.mode) {
              | Syn({fn_pos, fix}) =>
                let (status: HoleStatus.t, ty: Type.t) =
                  fn_pos ? (InHole, Hole) : (NotInHole, Num);
                // TODO shouldn't be necessary to fix in positions like this
                let ze = fix(ty);
                is_left
                  ? {
                    ...ze,
                    suffix:
                      [Tile.BinOp(HExp.Tile.Plus(status)), ...r] @ ze.suffix,
                  }
                  : {...ze, prefix: ze.prefix @ l @ [BinOp(Plus(status))]};
              | Ana({expected, fix}) =>
                let status: HoleStatus.t =
                  Type.consistent(expected, Num) ? NotInHole : InHole;
                let ze = fix();
                is_left
                  ? {
                    ...ze,
                    suffix:
                      [Tile.BinOp(HExp.Tile.Plus(status)), ...r] @ ze.suffix,
                  }
                  : {...ze, prefix: ze.prefix @ l @ [BinOp(Plus(status))]};
              },
          });
        go(is_left ? l : r, {ctx: info.ctx, mode: operand_mode});
      | BinOp((l, OperatorHole, r)) =>
        let is_left = n < List.length(l);
        let operand_mode =
          Syn({
            fn_pos: false,
            fix: _ => {
              let ze =
                switch (info.mode) {
                | Syn({fix, _}) => fix(Type.Hole)
                | Ana({fix, _}) => fix()
                };
              is_left
                ? {
                  ...ze,
                  suffix:
                    [Tile.BinOp(HExp.Tile.OperatorHole), ...r] @ ze.suffix,
                }
                : {
                  ...ze,
                  prefix:
                    ze.prefix @ l @ [Tile.BinOp(HExp.Tile.OperatorHole)],
                };
            },
          });
        go(is_left ? l : r, {ctx: info.ctx, mode: operand_mode});
      };
    go(e, info);
  }
  and mk_ztile = (ztile: ZExp.ztile): option(t(ZExp.ztile)) => {
    open OptUtil.Syntax;
    let (zroot, ze) = {
      let ((tile_step, _), Zipped_exp((e, zrest))) =
        ZPath.Exp.zip_ztile(HExp.dummy_hole, ztile);
      let ZList.{prefix, z, suffix} = HExp.nth_root(tile_step, e);
      (z, ZExp.mk(~prefix, ~z=?zrest, ~suffix, ()));
    };
    let* info = mk(ze);
    switch (zroot) {
    | Operand(_) =>
      switch (Tile.get_operand(ztile)) {
      | ParenZ_body(_) =>
        let info =
          switch (info.mode) {
          | Syn({fn_pos, fix}) =>
            let fix = ty => Tile.Operand(ZExp.ParenZ_body(fix(ty)));
            {ctx: info.ctx, mode: Syn({fn_pos, fix})};
          | Ana({expected, fix}) =>
            let fix = () => Tile.Operand(ZExp.ParenZ_body(fix()));
            {ctx: info.ctx, mode: Ana({expected, fix})};
          };
        Some(info);
      }
    | PreOp(_) => raise(ZExp.Void_ZPreOp)
    | PostOp((l, _)) =>
      switch (Tile.get_postop(ztile)) {
      | ApZ_arg(_) =>
        let* fn_ty = Statics.Exp.syn(info.ctx, l);
        let+ (ty_in, ty_out) = Type.matched_arrow(fn_ty);
        switch (info.mode) {
        | Syn({fn_pos, fix}) =>
          let fix = () => {
            let (status: HoleStatus.t, ty) =
              !fn_pos || Option.is_some(Type.matched_arrow(ty_out))
                ? (NotInHole, ty_out) : (InHole, Hole);
            let ze = {
              let ze = fix(ty);
              {...ze, prefix: ze.prefix @ l};
            };
            Tile.PostOp(ZExp.ApZ_arg(status, ze));
          };
          {ctx: info.ctx, mode: Ana({expected: ty_in, fix})};
        | Ana({expected, fix}) =>
          let fix = () => {
            let status: HoleStatus.t =
              Type.consistent(ty_out, expected) ? NotInHole : InHole;
            let ze = {
              let ze = fix();
              {...ze, prefix: ze.prefix @ l};
            };
            Tile.PostOp(ZExp.ApZ_arg(status, ze));
          };
          {ctx: info.ctx, mode: Ana({expected: ty_in, fix})};
        };
      }
    | BinOp(_) => raise(ZExp.Void_ZBinOp)
    };
  };
};

module Pat = {
  type t('z) = {
    ctx: Ctx.t,
    mode: mode('z),
  }
  and mode('z) =
    | Syn({fix: (Type.t, Ctx.t) => 'z})
    | Ana({
        expected: Type.t,
        fix: Ctx.t => 'z,
      });

  let map = (f: 'y => 'z, info: t('y)): t('z) => {
    ctx: info.ctx,
    mode:
      switch (info.mode) {
      | Syn({fix}) => Syn({fix: (ty, ctx) => f(fix(ty, ctx))})
      | Ana({expected, fix}) => Ana({expected, fix: ctx => f(fix(ctx))})
      },
  };

  let rec mk = (zp: ZPat.t): option(t(ZPat.t)) => {
    open OptUtil.Syntax;
    let (n, (p, zrest)) = ZPath.Pat.zip(HPat.dummy_hole, zp);
    let* info =
      switch (zrest) {
      | None =>
        Some({ctx: Ctx.empty, mode: Syn({fix: (_, _) => ZPat.mk()})})
      | Some(ztile) =>
        let+ info = mk_ztile(ztile);
        map(ztile => ZPat.mk(~z=ztile, ()), info);
      };

    let rec go = (p: HPat.t, info: t(ZPat.t)): option(t(ZPat.t)) =>
      switch (HPat.root(p)) {
      | Operand(_) =>
        // dummy hole
        Some(info)
      | PreOp(_) => raise(HPat.Tile.Void_PreOp)
      | PostOp((subj, Ann(_, ann))) =>
        let ann_ty = HTyp.contract(ann);
        let subj_mode =
          switch (info.mode) {
          | Syn({fix}) =>
            let fix = (ctx: Ctx.t): ZPat.t => {
              let zp = fix(ann_ty, ctx);
              {...zp, suffix: [PostOp(Ann(NotInHole, ann)), ...zp.suffix]};
            };
            Ana({expected: ann_ty, fix});
          | Ana({expected, fix}) =>
            let fix = (ctx: Ctx.t): ZPat.t => {
              let zp = fix(ctx);
              let status: HoleStatus.t =
                Type.consistent(ann_ty, expected) ? NotInHole : InHole;
              {...zp, suffix: [PostOp(Ann(status, ann)), ...zp.suffix]};
            };
            Ana({expected: ann_ty, fix});
          };
        go(subj, {ctx: info.ctx, mode: subj_mode});
      | BinOp((l, OperatorHole, r)) =>
        if (n < List.length(l)) {
          let l_mode =
            Syn({
              fix: (_, ctx) => {
                let (r, _, ctx) = Statics.Pat.syn_fix_holes(ctx, r);
                let ze =
                  switch (info.mode) {
                  | Syn({fix}) => fix(Type.Hole, ctx)
                  | Ana({fix, _}) => fix(ctx)
                  };
                {
                  ...ze,
                  suffix:
                    [Tile.BinOp(HPat.Tile.OperatorHole), ...r] @ ze.suffix,
                };
              },
            });
          go(l, {ctx: info.ctx, mode: l_mode});
        } else {
          let* (_, ctx) = Statics.Pat.syn(info.ctx, l);
          let r_mode =
            Syn({
              fix: (_, ctx) => {
                let ze =
                  switch (info.mode) {
                  | Syn({fix}) => fix(Type.Hole, ctx)
                  | Ana({fix, _}) => fix(ctx)
                  };
                let prefix =
                  ze.prefix @ l @ [Tile.BinOp(HPat.Tile.OperatorHole)];
                {...ze, prefix};
              },
            });
          go(r, {ctx, mode: r_mode});
        }
      };
    go(p, info);
  }
  and mk_ztile = (ztile: ZPat.ztile): option(t(ZPat.ztile)) => {
    open OptUtil.Syntax;
    let ((tile_step, _), zipped) =
      ZPath.Pat.zip_ztile(HPat.dummy_hole, ztile);
    switch (zipped) {
    | Zipped_exp((e, zrest)) =>
      let (zroot, ze) = {
        let ZList.{prefix, z, suffix} = HExp.nth_root(tile_step, e);
        (z, ZExp.mk(~prefix, ~z=?zrest, ~suffix, ()));
      };
      let* info = Exp.mk(ze);
      switch (zroot) {
      | Operand(_) => failwith("no operand that takes a pat and produces exp")
      | PreOp((_, r)) =>
        switch (Tile.get_preop(ztile)) {
        | LamZ_pat(status, _) =>
          switch (info.mode) {
          | Syn({fn_pos: _, fix}) =>
            let p_mode =
              Syn({
                fix: (ty_in, ctx) => {
                  let ze = {
                    let (body, ty_out) = Statics.Exp.syn_fix_holes(ctx, r);
                    let ze = fix(Arrow(ty_in, ty_out));
                    {...ze, suffix: body @ ze.suffix};
                  };
                  Tile.PreOp(ZPat.LamZ_pat(NotInHole, ze));
                },
              });
            Some({ctx: info.ctx, mode: p_mode});
          | Ana({expected, fix}) =>
            let+ p_mode =
              switch (Type.matched_arrow(expected), status) {
              | (None, NotInHole) => None
              | (Some((ty_in, ty_out)), _) =>
                Some(
                  Ana({
                    expected: ty_in,
                    fix: ctx => {
                      let ze = {
                        let body = Statics.Exp.ana_fix_holes(ctx, r, ty_out);
                        let ze = fix();
                        {...ze, suffix: body @ ze.suffix};
                      };
                      Tile.PreOp(ZPat.LamZ_pat(NotInHole, ze));
                    },
                  }),
                )
              | (_, InHole) =>
                Some(
                  Syn({
                    fix: (_, ctx) => {
                      let ze = {
                        let (body, _) = Statics.Exp.syn_fix_holes(ctx, r);
                        let ze = fix();
                        {...ze, suffix: body @ ze.suffix};
                      };
                      Tile.PreOp(ZPat.LamZ_pat(InHole, ze));
                    },
                  }),
                )
              };
            {ctx: info.ctx, mode: p_mode};
          }
        }
      | PostOp(_) => raise(ZPat.Void_ZPostOp)
      | BinOp(_) => raise(ZPat.Void_ZBinOp)
      };
    | Zipped_pat((p, zrest)) =>
      let (zroot, zp) = {
        let ZList.{prefix, z, suffix} = HPat.nth_root(tile_step, p);
        (z, ZPat.mk(~prefix, ~z=?zrest, ~suffix, ()));
      };
      let* info = mk(zp);
      switch (zroot) {
      | Operand(_) =>
        switch (Tile.get_operand(ztile)) {
        | ParenZ_body(_) =>
          let body_mode =
            switch (info.mode) {
            | Syn({fix}) =>
              Syn({
                fix: (ty, ctx) =>
                  Tile.Operand(ZPat.ParenZ_body(fix(ty, ctx))),
              })
            | Ana({expected, fix}) =>
              Ana({
                expected,
                fix: ctx => Tile.Operand(ZPat.ParenZ_body(fix(ctx))),
              })
            };
          Some({ctx: info.ctx, mode: body_mode});
        }
      | PreOp(_) => failwith("no preop that takes a pat produces a pat")
      | PostOp(_) => raise(ZPat.Void_ZPostOp)
      | BinOp(_) => raise(ZPat.Void_ZBinOp)
      };
    };
  };
};
