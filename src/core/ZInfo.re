open Util;
open OptUtil.Syntax;

module Exp = {
  type t('z) = {
    ctx: Ctx.t,
    mode: mode('z),
  }
  and mode('z) =
    | Syn(Type.t => 'z)
    | Ana(Type.t, 'z)
    | Fn_pos((Type.t, Type.t) => 'z)
    | Let_def(Type.t, Type.t => 'z);

  let map_mode = (f: 'y => 'z, mode: mode('y)): mode('z) =>
    switch (mode) {
    | Syn(fix) => Syn(ty => f(fix(ty)))
    | Ana(expected, fixed) => Ana(expected, f(fixed))
    | Fn_pos(fix) => Fn_pos((ty_in, ty_out) => f(fix(ty_in, ty_out)))
    | Let_def(expected, fix) => Let_def(expected, ty => f(fix(ty)))
    };
  let map = (f: 'y => 'z, info: t('y)): t('z) => {
    ctx: info.ctx,
    mode: map_mode(f, info.mode),
  };

  let rec mk = (ze: ZExp.t): option(t(ZExp.t)) => {
    let (n, (e, zrest)) = ZPath.Exp.zip(HExp.dummy_hole, ze);
    let* info =
      switch (zrest) {
      | None => Some({ctx: Ctx.empty, mode: Syn(_ => ZExp.mk())})
      | Some(ztile) =>
        let+ info = mk_ztile(ztile);
        map(ztile => ZExp.mk(~z=ztile, ()), info);
      };

    let rec go = (e: HExp.t, info: t(ZExp.t)): option(t(ZExp.t)) => {
      open HExp;
      // dummy hole
      let op = _ => Some(info);
      let pre = pre =>
        switch (pre) {
        | (Lam(_, p), body) =>
          let syn_pat = () => {
            let+ (pty, ctx) = Statics.Pat.syn(info.ctx, p);
            (PType.to_type(pty), ctx);
          };
          let mk_ze = (status, p, ze) =>
            ZList.{...ze, prefix: ze.prefix @ [Tile.Pre(Lam(status, p))]};
          let* body_info =
            switch (info.mode) {
            | Syn(fix) =>
              let+ (ty_in, ctx) = syn_pat();
              let fix = ty_out =>
                mk_ze(NotInHole, p, fix(Arrow(ty_in, ty_out)));
              {ctx, mode: Syn(fix)};
            | Ana(expected, fixed) =>
              switch (Type.matched_arrow(expected)) {
              | None =>
                let+ (_, ctx) = syn_pat();
                let fix = _ => mk_ze(InHole, p, fixed);
                {ctx, mode: Syn(fix)};
              | Some((ty_in, ty_out)) =>
                let+ ctx = Statics.Pat.ana(info.ctx, p, ty_in);
                let fixed = mk_ze(NotInHole, p, fixed);
                {ctx, mode: Ana(ty_out, fixed)};
              }
            | Fn_pos(fix) =>
              let+ (ty_in, ctx) = syn_pat();
              let fix = ty_out => mk_ze(NotInHole, p, fix(ty_in, ty_out));
              {ctx, mode: Syn(fix)};
            | Let_def(expected, fix) =>
              switch (Type.matched_arrow(expected)) {
              | None =>
                let+ (ty_in, ctx) = syn_pat();
                let fix = ty_out =>
                  mk_ze(NotInHole, p, fix(Arrow(ty_in, ty_out)));
                {ctx, mode: Syn(fix)};
              | Some((ty_in, ty_out)) =>
                let+ ctx = Statics.Pat.ana(info.ctx, p, ty_in);
                let fixed = mk_ze(NotInHole, p, fix(Arrow(ty_in, ty_out)));
                {ctx, mode: Ana(ty_out, fixed)};
              }
            };
          go(body, body_info);
        | (Let(p, def), body) =>
          let mk_ze = ze =>
            ZList.{...ze, prefix: ze.prefix @ [Tile.Pre(Let(p, def))]};
          let mode = map_mode(mk_ze, info.mode);
          // def_ty sufficient to get ctx
          let* def_ty = Statics.Exp.syn(info.ctx, def);
          let* ctx = Statics.Pat.ana(info.ctx, p, def_ty);
          go(body, {ctx, mode});
        };
      let post = post =>
        switch (post) {
        | (fn, Ap(_, arg)) =>
          let fix = (ty_in, ty_out) => {
            let arg = Statics.Exp.ana_fix_holes(info.ctx, arg, ty_in);
            let mk_ze = (status, ze) =>
              ZList.{
                ...ze,
                suffix: [Tile.Post(Ap(status, arg)), ...ze.suffix],
              };
            switch (info.mode) {
            | Syn(fix) => mk_ze(NotInHole, fix(ty_out))
            | Ana(expected, fixed) =>
              let status = HoleStatus.mk(Type.consistent(ty_out, expected));
              mk_ze(status, fixed);
            | Fn_pos(fix) =>
              let (status: HoleStatus.t, (ty_in, ty_out)) =
                switch (Type.matched_arrow(ty_out)) {
                | None => (InHole, Type.(Hole, Hole))
                | Some(tys) => (NotInHole, tys)
                };
              mk_ze(status, fix(ty_in, ty_out));
            | Let_def(expected, fix) =>
              let status = HoleStatus.mk(Type.consistent(ty_out, expected));
              mk_ze(status, fix(ty_out));
            };
          };
          go(fn, {ctx: info.ctx, mode: Fn_pos(fix)});
        };
      let bin = ((l, bin, r)) => {
        let is_left = n < List.length(l);
        switch (bin) {
        | Plus(_) =>
          let mk_ze = (status, ze: ZExp.t): ZExp.t =>
            is_left
              ? {
                ...ze,
                suffix: [Tile.Bin(Plus(status)), ...r] @ ze.suffix,
              }
              : {...ze, prefix: ze.prefix @ l @ [Tile.Bin(Plus(status))]};
          let fixed =
            switch (info.mode) {
            | Syn(fix) =>
              // TODO shouldn't be necessary to fix in positions like this
              mk_ze(NotInHole, fix(Num))
            | Ana(expected, fixed) =>
              let status = HoleStatus.mk(Type.consistent(expected, Num));
              mk_ze(status, fixed);
            | Fn_pos(fix) => mk_ze(InHole, fix(Hole, Hole))
            | Let_def(expected, fix) =>
              let (status: HoleStatus.t, ty: Type.t) =
                Type.consistent(expected, Num)
                  ? (NotInHole, Num) : (InHole, Hole);
              mk_ze(status, fix(ty));
            };
          go(is_left ? l : r, {...info, mode: Ana(Num, fixed)});
        | BinHole =>
          let mk_ze = (ze: ZExp.t): ZExp.t =>
            is_left
              ? {...ze, suffix: [Tile.Bin(BinHole), ...r] @ ze.suffix}
              : {...ze, prefix: ze.prefix @ l @ [Tile.Bin(BinHole)]};
          let fix = _ =>
            switch (info.mode) {
            | Syn(fix) => mk_ze(fix(Hole))
            | Ana(_, fixed) => mk_ze(fixed)
            | Fn_pos(fix) => mk_ze(fix(Hole, Hole))
            | Let_def(_, fix) => mk_ze(fix(Hole))
            };
          go(is_left ? l : r, {...info, mode: Syn(fix)});
        };
      };
      Tile.get(op, pre, post, bin, HExp.root(e));
    };
    go(e, info);
  }
  and mk_ztile = (ztile: ZExp.ztile): option(t(ZExp.ztile)) => {
    let (zroot, ze) = {
      let ((tile_step, _), `Exp(e, zrest)) =
        ZPath.Exp.zip_ztile(HExp.dummy_hole, ztile);
      let ZList.{prefix, z, suffix} = HExp.nth_root(tile_step, e);
      (z, ZExp.mk(~prefix, ~z=?zrest, ~suffix, ()));
    };
    let* info = mk(ze);
    switch (zroot) {
    | Op(_) =>
      switch (Tile.get_operand(ztile)) {
      | ParenZ_body(_) =>
        let wrap = ze => Tile.Op(ZExp.ParenZ_body(ze));
        Some(map(wrap, info));
      }
    | Pre((_, r)) =>
      let mk_ze = (r, ze) => ZList.{...ze, suffix: r @ ze.suffix};
      switch (Tile.get_preop(ztile)) {
      | LetZ_def(p, _) =>
        let+ (pty, _) = Statics.Pat.syn(info.ctx, p);
        let ze = ctx_body =>
          switch (info.mode) {
          | Syn(fix) =>
            let (body, ty) = Statics.Exp.syn_fix_holes(ctx_body, r);
            mk_ze(body, fix(ty));
          | Ana(expected, fixed) =>
            let body = Statics.Exp.ana_fix_holes(ctx_body, r, expected);
            mk_ze(body, fixed);
          | Fn_pos(fix) =>
            let (body, (ty_in, ty_out)) = {
              let (body, ty) = Statics.Exp.syn_fix_holes(ctx_body, r);
              switch (Type.matched_arrow(ty)) {
              | None => (
                  HExp.put_hole_status(InHole, body),
                  Type.(Hole, Hole),
                )
              | Some(tys) => (body, tys)
              };
            };
            mk_ze(body, fix(ty_in, ty_out));
          | Let_def(expected, fix) =>
            let body = Statics.Exp.ana_fix_holes(info.ctx, r, expected);
            let ty =
              Statics.Exp.syn(info.ctx, body)
              |> OptUtil.get(() =>
                   failwith("expected body to synthesize after ana fixing")
                 );
            mk_ze(body, fix(ty));
          };
        let fix = def_ty => {
          let joined_ty = PType.join_or_to_type(pty, def_ty);
          let ctx_body =
            Statics.Pat.ana(info.ctx, p, joined_ty)
            |> OptUtil.get(() =>
                 failwith("expected p to have consistent type with joined_ty")
               );
          let ze = ze(ctx_body);
          Tile.Pre(ZExp.LetZ_def(p, ze));
        };
        {...info, mode: Let_def(PType.to_type(pty), fix)};
      };
    | Post((l, _)) =>
      let mk_ze = (l, ze) => ZList.{...ze, prefix: ze.prefix @ l};
      switch (Tile.get_postop(ztile)) {
      | ApZ_arg(_) =>
        let mk_ztile = (status, ze) =>
          Tile.Post(ZExp.ApZ_arg(status, mk_ze(l, ze)));
        let* fn_ty = Statics.Exp.syn(info.ctx, l);
        let+ (ty_in, ty_out) = Type.matched_arrow(fn_ty);
        let fixed =
          switch (info.mode) {
          | Syn(fix) => mk_ztile(NotInHole, fix(ty_out))
          | Ana(expected, fixed) =>
            let status = HoleStatus.mk(Type.consistent(ty_out, expected));
            mk_ztile(status, fixed);
          | Fn_pos(fix) =>
            switch (Type.matched_arrow(ty_out)) {
            | None => mk_ztile(InHole, fix(Hole, Hole))
            | Some((ty_in, ty_out)) =>
              mk_ztile(NotInHole, fix(ty_in, ty_out))
            }
          | Let_def(expected, fix) =>
            let status = HoleStatus.mk(Type.consistent(ty_out, expected));
            mk_ztile(status, fix(ty_out));
          };
        {...info, mode: Ana(ty_in, fixed)};
      };
    | Bin(_) =>
      let () = Tile.get_binop(ztile);
      raise(ZExp.Void_zbin);
    };
  };

  let fix_holes = ((zipped, unzipped): ZExp.zipper): option(ZExp.zipper) => {
    switch (unzipped) {
    | None =>
      let (zipped, _) = Statics.Exp.syn_fix_holes(Ctx.empty, zipped);
      Some((zipped, None));
    | Some(ztile) =>
      let+ {ctx, mode} = mk_ztile(ztile);
      let (zipped, ztile) =
        switch (mode) {
        | Syn(fix) =>
          let (zipped, ty) = Statics.Exp.syn_fix_holes(ctx, zipped);
          (zipped, fix(ty));
        | Ana(expected, fixed) =>
          let zipped = Statics.Exp.ana_fix_holes(ctx, zipped, expected);
          (zipped, fixed);
        | Fn_pos(fix) =>
          let (zipped, ty) = Statics.Exp.syn_fix_holes(ctx, zipped);
          switch (Type.matched_arrow(ty)) {
          | None =>
            let zipped = HExp.put_hole_status(InHole, zipped);
            (zipped, fix(Hole, Hole));
          | Some((ty_in, ty_out)) => (zipped, fix(ty_in, ty_out))
          };
        | Let_def(_) => failwith("todo")
        };
      (zipped, Some(ztile));
    };
  };
};

module Pat = {
  type t('z) = {
    ctx: Ctx.t,
    mode: mode('z),
  }
  and mode('z) =
    | Syn((PType.t, Ctx.t) => 'z)
    | Ana(Type.t, Ctx.t => 'z)
    | Let_pat(Type.t, (PType.t, Ctx.t) => 'z);

  let map_mode = (f: 'y => 'z, mode: mode('y)): mode('z) =>
    switch (mode) {
    | Syn(fix) => Syn((ty, ctx) => f(fix(ty, ctx)))
    | Ana(expected, fix) => Ana(expected, ctx => f(fix(ctx)))
    | Let_pat(def_ty, fix) =>
      Let_pat(def_ty, (pty, ctx) => f(fix(pty, ctx)))
    };
  let map = (f: 'y => 'z, info: t('y)): t('z) => {
    ctx: info.ctx,
    mode: map_mode(f, info.mode),
  };

  let rec mk = (zp: ZPat.t): option(t(ZPat.t)) => {
    let (n, (p, zrest)) = ZPath.Pat.zip(HPat.dummy_hole, zp);
    let* info =
      switch (zrest) {
      | None => Some({ctx: Ctx.empty, mode: Syn((_, _) => ZPat.mk())})
      | Some(ztile) =>
        let+ info = mk_ztile(ztile);
        map(ztile => ZPat.mk(~z=ztile, ()), info);
      };

    let rec go = (p: HPat.t, info: t(ZPat.t)): option(t(ZPat.t)) => {
      HPat.T.(
        switch (HPat.root(p)) {
        | Op(_) =>
          // dummy hole
          Some(info)
        | Pre(((), _)) => raise(Void_pre)
        | Post((subj, Ann(_, ann))) =>
          let ann_ty = HTyp.contract(ann);
          let mk_zp = (status, zp: ZPat.t): ZPat.t => {
            ...zp,
            suffix: [Post(Ann(status, ann)), ...zp.suffix],
          };
          let subj_mode =
            switch (info.mode) {
            | Syn(fix) =>
              let fix = ctx =>
                mk_zp(NotInHole, fix(PType.of_type(ann_ty), ctx));
              Ana(ann_ty, fix);
            | Ana(expected, fix) =>
              let fix = ctx => {
                let status =
                  HoleStatus.mk(Type.consistent(ann_ty, expected));
                mk_zp(status, fix(ctx));
              };
              Ana(ann_ty, fix);
            | Let_pat(_, fix) =>
              let fix = ctx =>
                mk_zp(NotInHole, fix(PType.of_type(ann_ty), ctx));
              Ana(ann_ty, fix);
            };
          go(subj, {ctx: info.ctx, mode: subj_mode});
        | Bin((l, BinHole, r)) =>
          let is_left = n < List.length(l);
          let tile = Tile.Bin(BinHole);
          let mk_zp = ctx =>
            switch (info.mode) {
            | Syn(fix)
            | Let_pat(_, fix) => fix(Unspecified, ctx)
            | Ana(_, fix) => fix(ctx)
            };
          if (is_left) {
            let fix = (_, ctx): ZPat.t => {
              let (r, _, ctx) = Statics.Pat.syn_fix_holes(ctx, r);
              let zp = mk_zp(ctx);
              {...zp, suffix: [tile, ...r] @ zp.suffix};
            };
            go(l, {...info, mode: Syn(fix)});
          } else {
            let fix = (_, ctx): ZPat.t => {
              let zp = mk_zp(ctx);
              {...zp, prefix: zp.prefix @ l @ [tile]};
            };
            let* (_, ctx) = Statics.Pat.syn(info.ctx, l);
            go(r, {ctx, mode: Syn(fix)});
          };
        }
      );
    };
    go(p, info);
  }
  and mk_ztile = (ztile: ZPat.ztile): option(t(ZPat.ztile)) => {
    let ((tile_step, _), zip_result) =
      ZPath.Pat.zip_ztile(HPat.dummy_hole, ztile);
    switch (zip_result) {
    | `Exp(e, zrest) =>
      let (zroot, ze) = {
        let ZList.{prefix, z, suffix} = HExp.nth_root(tile_step, e);
        (z, ZExp.mk(~prefix, ~z=?zrest, ~suffix, ()));
      };
      let* info = Exp.mk(ze);
      switch (zroot) {
      | Op(_) => failwith("no op that takes a pat and produces exp")
      | Pre((_, r)) =>
        let mk_ze = (r, ze: ZExp.t): ZExp.t => {
          ...ze,
          suffix: r @ ze.suffix,
        };
        switch (Tile.get_preop(ztile)) {
        | LamZ_pat(_) =>
          let mk_ztile = (status, body, ze) => {
            let ze = mk_ze(body, ze);
            Tile.Pre(ZPat.LamZ_pat(status, ze));
          };
          let mode: mode(ZPat.ztile) =
            switch (info.mode) {
            | Syn(fix) =>
              let fix = (ty_in, ctx) => {
                let (body, ty_out) = Statics.Exp.syn_fix_holes(ctx, r);
                mk_ztile(
                  NotInHole,
                  body,
                  fix(Arrow(PType.to_type(ty_in), ty_out)),
                );
              };
              Syn(fix);
            | Ana(expected, fixed) =>
              switch (Type.matched_arrow(expected)) {
              | None =>
                let fix = (_, ctx) => {
                  let (body, _) = Statics.Exp.syn_fix_holes(ctx, r);
                  mk_ztile(InHole, body, fixed);
                };
                Syn(fix);
              | Some((ty_in, ty_out)) =>
                let fix = ctx => {
                  let body = Statics.Exp.ana_fix_holes(ctx, r, ty_out);
                  mk_ztile(NotInHole, body, fixed);
                };
                Ana(ty_in, fix);
              }
            | Fn_pos(fix) =>
              let fix = (ty_in, ctx) => {
                let (body, ty_out) = Statics.Exp.syn_fix_holes(ctx, r);
                mk_ztile(
                  NotInHole,
                  body,
                  fix(PType.to_type(ty_in), ty_out),
                );
              };
              Syn(fix);
            | Let_def(expected, fix) =>
              switch (Type.matched_arrow(expected)) {
              | None =>
                let fix = (_, ctx) => {
                  let (body, _) = Statics.Exp.syn_fix_holes(ctx, r);
                  mk_ztile(InHole, body, fix(Hole));
                };
                Syn(fix);
              | Some((ty_in, ty_out)) =>
                let fix = ctx => {
                  let body = Statics.Exp.ana_fix_holes(ctx, r, ty_out);
                  mk_ztile(NotInHole, body, fix(Arrow(ty_in, ty_out)));
                };
                Ana(ty_in, fix);
              }
            };
          Some({ctx: info.ctx, mode});
        | LetZ_pat(_, def) =>
          let mk_ztile = (def, body, ze) => {
            let ze = mk_ze(body, ze);
            Tile.Pre(ZPat.LetZ_pat(ze, def));
          };
          let* def_ty = Statics.Exp.syn(info.ctx, def);
          let fix = (pty, ctx) => {
            let joined = PType.join_or_to_type(pty, def_ty);
            let def = Statics.Exp.ana_fix_holes(ctx, def, joined);
            switch (info.mode) {
            | Syn(fix) =>
              let (body, ty) = Statics.Exp.syn_fix_holes(ctx, r);
              mk_ztile(def, body, fix(ty));
            | Ana(expected, fixed) =>
              let body = Statics.Exp.ana_fix_holes(ctx, r, expected);
              mk_ztile(def, body, fixed);
            | Fn_pos(fix) =>
              let (body, (ty_in, ty_out)) = {
                let (body, ty) = Statics.Exp.syn_fix_holes(ctx, r);
                switch (Type.matched_arrow(ty)) {
                | None => (
                    HExp.put_hole_status(InHole, body),
                    Type.(Hole, Hole),
                  )
                | Some(tys) => (body, tys)
                };
              };
              mk_ztile(def, body, fix(ty_in, ty_out));
            | Let_def(expected, fix) =>
              let body = Statics.Exp.ana_fix_holes(info.ctx, r, expected);
              let ty =
                Statics.Exp.syn(info.ctx, body)
                |> OptUtil.get(() =>
                     failwith("expected body to synthesize after ana fixing")
                   );
              mk_ztile(def, body, fix(ty));
            };
          };
          Some({ctx: info.ctx, mode: Let_pat(def_ty, fix)});
        };
      | Post(_) =>
        let () = Tile.get_postop(ztile);
        raise(ZPat.Void_zpost);
      | Bin(_) =>
        let () = Tile.get_binop(ztile);
        raise(ZPat.Void_zbin);
      };
    | `Pat(p, zrest) =>
      let (zroot, zp) = {
        let ZList.{prefix, z, suffix} = HPat.nth_root(tile_step, p);
        (z, ZPat.mk(~prefix, ~z=?zrest, ~suffix, ()));
      };
      let* info = mk(zp);
      switch (zroot) {
      | Op(_) =>
        switch (Tile.get_operand(ztile)) {
        | ParenZ_body(_) =>
          let wrap = ze => Tile.Op(ZPat.ParenZ_body(ze));
          Some(map(wrap, info));
        }
      | Pre(_) => failwith("no pre that takes a pat produces a pat")
      | Post((_, Ann(_))) => raise(ZPat.Void_zpost)
      | Bin((_, BinHole, _)) => raise(ZPat.Void_zbin)
      };
    };
  };

  let fix_holes = ((zipped, unzipped): ZPat.zipper): option(ZPat.zipper) => {
    switch (unzipped) {
    | None =>
      let (zipped, _, _) = Statics.Pat.syn_fix_holes(Ctx.empty, zipped);
      Some((zipped, None));
    | Some(ztile) =>
      let+ {ctx, mode} = mk_ztile(ztile);
      let (zipped, ztile) =
        switch (mode) {
        | Syn(fix) =>
          let (zipped, ty, ctx) = Statics.Pat.syn_fix_holes(ctx, zipped);
          (zipped, fix(ty, ctx));
        | Ana(expected, fix) =>
          let (zipped, ctx) =
            Statics.Pat.ana_fix_holes(ctx, zipped, expected);
          (zipped, fix(ctx));
        | Let_pat(def_ty, fix) =>
          let (zipped, pty, _) = Statics.Pat.syn_fix_holes(ctx, zipped);
          let joined = PType.join_or_to_type(pty, def_ty);
          let ctx =
            Statics.Pat.ana(ctx, zipped, joined)
            |> OptUtil.get(() =>
                 failwith("expected joined type to be consistent with p")
               );
          (zipped, fix(pty, ctx));
        };
      (zipped, Some(ztile));
    };
  };
};
