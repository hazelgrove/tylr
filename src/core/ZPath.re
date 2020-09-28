type tile_step = int;
type child_step = int;
type caret_step = int;
type two_step = (tile_step, child_step);
type t = (list(two_step), caret_step);

let cons = (two_step, (steps, k)) => ([two_step, ...steps], k);

module rec Typ: {
  type zipped = (HTyp.t, option(ZTyp.ztile));
  type zipped_ztile =
    | Zipped_typ(zipped)
    | Zipped_pat(Pat.zipped);

  let zip: (HTyp.t, ZTyp.t) => (tile_step, zipped);
  let zip_ztile: (HTyp.t, ZTyp.ztile) => (two_step, zipped_ztile);

  type unzipped_tile = (HTyp.t, ZTyp.ztile);
  type unzipped =
    | Unzipped_typ(unzipped_tile);

  let unzip_tile: (child_step, HTyp.Tile.t, ZTyp.t) => unzipped;
  let unzip: (two_step, HTyp.t, option(ZTyp.ztile)) => unzipped;

  let move: (Direction.t, HTyp.t, t) => option(t);
} = {
  type zipped = (HTyp.t, option(ZTyp.ztile));
  type zipped_ztile =
    | Zipped_typ(zipped)
    | Zipped_pat(Pat.zipped);

  let zip = (ty: HTyp.t, zty: ZTyp.t): (tile_step, zipped) => (
    List.length(zty.prefix),
    (zty.prefix @ ty @ zty.suffix, zty.z),
  );
  let zip_ztile =
      (subject: HTyp.t, ztile: ZTyp.ztile): (two_step, zipped_ztile) =>
    switch (ztile) {
    | Operand(ParenZ_body(zty)) =>
      let (tile_step, (ty, zrest)) =
        zip([Tile.Operand(HTyp.Tile.Paren(subject))], zty);
      ((tile_step, 0), Zipped_typ((ty, zrest)));
    | PreOp(_) => raise(ZTyp.Void_ZPreOp)
    | PostOp(AnnZ_ann(status, zp)) =>
      let (tile_step, (p, zrest)) =
        Pat.zip([Tile.PostOp(HPat.Tile.Ann(status, subject))], zp);
      ((tile_step, 0), Zipped_pat((p, zrest)));
    | BinOp(_) => raise(ZTyp.Void_ZBinOp)
    };

  type unzipped_tile = (HTyp.t, ZTyp.ztile);
  type unzipped =
    | Unzipped_typ(unzipped_tile);

  let unzip_tile = (r: child_step, tile: HTyp.Tile.t, zty: ZTyp.t): unzipped =>
    switch (tile) {
    | Operand(Paren(body)) when r == 0 =>
      Unzipped_typ((body, Tile.Operand(ParenZ_body(zty))))
    | _ => raise(Invalid_argument("ZPath.Typ.unzip_tile"))
    };
  let unzip =
      ((l, r): two_step, ty: HTyp.t, zrest: option(ZTyp.ztile)): unzipped => {
    let (tile, ze) = {
      let (prefix, tile, suffix) = ListUtil.split_nth(l, ty);
      (tile, ZTyp.mk(~prefix, ~z=?zrest, ~suffix, ()));
    };
    unzip_tile(r, tile, ze);
  };

  let move = (_: Direction.t, _: HTyp.t, _: t) => failwith("unimplemented");
}
and Pat: {
  type zipped = (HPat.t, option(ZPat.ztile));
  type zipped_ztile =
    | Zipped_pat(zipped)
    | Zipped_exp(Exp.zipped);

  let zip: (HPat.t, ZPat.t) => (tile_step, zipped);
  let zip_ztile: (HPat.t, ZPat.ztile) => (two_step, zipped_ztile);

  type unzipped_tile = (HPat.t, ZPat.ztile);
  type unzipped =
    | Unzipped_pat(unzipped_tile)
    | Unzipped_typ(Typ.unzipped_tile);

  let unzip_tile: (child_step, HPat.Tile.t, ZPat.t) => unzipped;
  let unzip: (two_step, HPat.t, option(ZPat.ztile)) => unzipped;

  let move: (Direction.t, HPat.t, t) => option(t);
} = {
  type zipped = (HPat.t, option(ZPat.ztile));
  type zipped_ztile =
    | Zipped_pat(zipped)
    | Zipped_exp(Exp.zipped);

  let zip = (p: HPat.t, zp: ZPat.t): (tile_step, zipped) => (
    List.length(zp.prefix),
    (zp.prefix @ p @ zp.suffix, zp.z),
  );
  let zip_ztile = (p: HPat.t, ztile: ZPat.ztile): (two_step, zipped_ztile) =>
    switch (ztile) {
    | Operand(ParenZ_body(zp)) =>
      let (tile_step, (p, zrest)) =
        zip([Tile.Operand(HPat.Tile.Paren(p))], zp);
      ((tile_step, 0), Zipped_pat((p, zrest)));
    | PreOp(LamZ_pat(status, ze)) =>
      let (tile_step, (e, zrest)) =
        Exp.zip([Tile.PreOp(HExp.Tile.Lam(status, p))], ze);
      ((tile_step, 0), Zipped_exp((e, zrest)));
    | PostOp(_) => raise(ZPat.Void_ZPostOp)
    | BinOp(_) => raise(ZPat.Void_ZBinOp)
    };

  type unzipped_tile = (HPat.t, ZPat.ztile);
  type unzipped =
    | Unzipped_pat(unzipped_tile)
    | Unzipped_typ(Typ.unzipped_tile);

  let unzip_tile = (r: child_step, tile: HPat.Tile.t, zp: ZPat.t): unzipped =>
    switch (tile) {
    | Operand(Paren(body)) when r == 0 =>
      Unzipped_pat((body, Tile.Operand(ZPat.ParenZ_body(zp))))
    | PostOp(Ann(status, ann)) when r == 0 =>
      Unzipped_typ((ann, Tile.PostOp(ZTyp.AnnZ_ann(status, zp))))
    | _ => raise(Invalid_argument("ZPath.Pat.unzip_tile"))
    };
  let unzip =
      ((l, r): two_step, p: HPat.t, zrest: option(ZPat.ztile)): unzipped => {
    let (tile, zp) = {
      let (prefix, tile, suffix) = ListUtil.split_nth(l, p);
      (tile, ZPat.mk(~prefix, ~z=?zrest, ~suffix, ()));
    };
    unzip_tile(r, tile, zp);
  };

  let move = (_: Direction.t, _: HPat.t, _: t) => failwith("unimplemented");
}
and Exp: {
  type zipped = (HExp.t, option(ZExp.ztile));
  type zipped_ztile =
    | Zipped_exp(zipped);

  let zip: (HExp.t, ZExp.t) => (tile_step, zipped);
  let zip_ztile: (HExp.t, ZExp.ztile) => (two_step, zipped_ztile);

  type unzipped_tile = (HExp.t, ZExp.ztile);
  type unzipped =
    | Unzipped_exp(unzipped_tile)
    | Unzipped_pat(Pat.unzipped_tile);

  let unzip_tile: (child_step, HExp.Tile.t, ZExp.t) => unzipped;
  let unzip: (two_step, HExp.t, option(ZExp.ztile)) => unzipped;

  let move: (Direction.t, HExp.t, t) => option(t);
} = {
  let rec move = (d: Direction.t, e: HExp.t, (steps, k): t): option(t) => {
    open OptUtil.Syntax;
    let if_left = (then_, else_) => d == Left ? then_ : else_;
    switch (steps) {
    | [] =>
      let n = d == Left ? k - 1 : k;
      let+ (_, tile, _) = ListUtil.split_nth_opt(n, e);
      switch (tile) {
      | Operand(OperandHole | Num(_) | Var(_))
      | BinOp(Plus(_) | OperatorHole) => (steps, if_left(k - 1, k + 1))
      | Operand(Paren(e))
      | PostOp(Ap(_, e)) => (
          [(n, 0), ...steps],
          if_left(List.length(e), 0),
        )
      | PreOp(Lam(_, p)) => (
          [(n, 0), ...steps],
          if_left(List.length(p), 0),
        )
      };
    | [(l, r) as two_step, ...steps] =>
      let* tile = List.nth_opt(e, l);
      switch (tile) {
      | Operand(OperandHole | Num(_) | Var(_))
      | BinOp(Plus(_) | OperatorHole) => None
      | Operand(Paren(e))
      | PostOp(Ap(_, e)) =>
        r == 0
          ? Some(
              switch (move(d, e, (steps, k))) {
              | None => ([], if_left(l, l + 1))
              | Some((steps, k)) => ([two_step, ...steps], k)
              },
            )
          : None
      | PreOp(Lam(_, p)) =>
        r == 0
          ? Some(
              switch (Pat.move(d, p, (steps, k))) {
              | None => ([], if_left(l, l + 1))
              | Some((steps, k)) => ([two_step, ...steps], k)
              },
            )
          : None
      };
    };
  };

  type zipped = (HExp.t, option(ZExp.ztile));
  type zipped_ztile =
    | Zipped_exp(zipped);

  let zip = (e: HExp.t, ze: ZExp.t): (tile_step, zipped) => (
    List.length(ze.prefix),
    (ze.prefix @ e @ ze.suffix, ze.z),
  );
  let zip_ztile = (e: HExp.t, ztile: ZExp.ztile): (two_step, zipped_ztile) =>
    switch (ztile) {
    | Operand(ParenZ_body(ze)) =>
      let (tile_step, (e, zrest)) =
        zip([Tile.Operand(HExp.Tile.Paren(e))], ze);
      ((tile_step, 0), Zipped_exp((e, zrest)));
    | PreOp(_) => raise(ZExp.Void_ZPreOp)
    | PostOp(ApZ_arg(status, ze)) =>
      let (tile_step, (e, zrest)) =
        zip([Tile.PostOp(HExp.Tile.Ap(status, e))], ze);
      ((tile_step, 0), Zipped_exp((e, zrest)));
    | BinOp(_) => raise(ZExp.Void_ZBinOp)
    };

  type unzipped_tile = (HExp.t, ZExp.ztile);
  type unzipped =
    | Unzipped_exp(unzipped_tile)
    | Unzipped_pat(Pat.unzipped_tile);

  let unzip_tile = (r: child_step, tile: HExp.Tile.t, ze: ZExp.t): unzipped =>
    switch (tile) {
    | Operand(Paren(body)) when r == 0 =>
      Unzipped_exp((body, Tile.Operand(ParenZ_body(ze))))
    | PreOp(Lam(status, p)) when r == 0 =>
      Unzipped_pat((p, Tile.PreOp(ZPat.LamZ_pat(status, ze))))
    | PostOp(Ap(status, arg)) when r == 0 =>
      Unzipped_exp((arg, Tile.PostOp(ZExp.ApZ_arg(status, ze))))
    | _ => raise(Invalid_argument("ZPath.Exp.unzip_tile"))
    };
  let unzip =
      ((l, r): two_step, e: HExp.t, zrest: option(ZExp.ztile)): unzipped => {
    let (tile, ze) = {
      let (prefix, tile, suffix) = ListUtil.split_nth(l, e);
      (tile, ZExp.mk(~prefix, ~z=?zrest, ~suffix, ()));
    };
    unzip_tile(r, tile, ze);
  };
};
