type tile_step = int;
type child_step = int;
type caret_step = int;
type two_step = (tile_step, child_step);
type t = (list(two_step), caret_step);

let cons = (two_step, (steps, k)) => ([two_step, ...steps], k);

module rec Typ: {
  type zipped =
    | Zipped_typ(ZTyp.zipper)
    | Zipped_pat(ZPat.zipper);

  let zip: (HTyp.t, ZTyp.t) => (tile_step, ZTyp.zipper);
  let zip_ztile: (HTyp.t, ZTyp.ztile) => (two_step, zipped);

  type unzipped =
    | Unzipped_typ(ZTyp.zipper);

  let unzip_tile: (child_step, HTyp.Tile.t, ZTyp.t) => unzipped;
  let unzip: (two_step, ZTyp.zipper) => unzipped;

  /**
   * `move(d, zipper, path)` first attempts to returns the next path
   * from `path` in direction `d` within the focused term of `zipper`.
   * If no such path exists (i.e. the cursor is at one of the ends of
   * the focused term), then it attempts to zip `zipper` and try once
   * more.
   */
  let move: (Direction.t, ZTyp.zipper, t) => option((t, zipped));
} = {
  type zipped =
    | Zipped_typ(ZTyp.zipper)
    | Zipped_pat(ZPat.zipper);

  let zip = (ty: HTyp.t, zty: ZTyp.t): (tile_step, ZTyp.zipper) => (
    List.length(zty.prefix),
    (zty.prefix @ ty @ zty.suffix, zty.z),
  );
  let zip_ztile = (subject: HTyp.t, ztile: ZTyp.ztile): (two_step, zipped) =>
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

  type unzipped =
    | Unzipped_typ(ZTyp.zipper);

  let unzip_tile = (r: child_step, tile: HTyp.Tile.t, zty: ZTyp.t): unzipped =>
    switch (tile) {
    | Operand(Paren(body)) when r == 0 =>
      Unzipped_typ((body, Some(Tile.Operand(ParenZ_body(zty)))))
    | _ => raise(Invalid_argument("ZPath.Typ.unzip_tile"))
    };
  let unzip =
      ((l, r): two_step, (ty: HTyp.t, zrest: option(ZTyp.ztile))): unzipped => {
    let (tile, ze) = {
      let (prefix, tile, suffix) = ListUtil.split_nth(l, ty);
      (tile, ZTyp.mk(~prefix, ~z=?zrest, ~suffix, ()));
    };
    unzip_tile(r, tile, ze);
  };

  let rec move =
          (
            d: Direction.t,
            (ty, zrest) as zipper: ZTyp.zipper,
            (steps, k): t,
          )
          : option((t, zipped)) => {
    let if_left = (then_, else_) => d == Left ? then_ : else_;
    switch (steps) {
    | [] =>
      let next = if_left(k - 1, k);
      switch (List.nth_opt(ty, next), zrest) {
      | (None, None) => None
      | (Some(tile), _) =>
        let path =
          switch (tile) {
          | Operand(OperandHole | Num)
          | BinOp(OperatorHole | Arrow) => (steps, if_left(k - 1, k + 1))
          | Operand(Paren(ty)) => (
              [(next, 0)],
              if_left(List.length(ty), 0),
            )
          | PreOp(_) => raise(HTyp.Tile.Void_PreOp)
          | PostOp(_) => raise(HTyp.Tile.Void_PostOp)
          };
        Some((path, Zipped_typ(zipper)));
      | (_, Some(ztile)) =>
        let ((tile_step, _), zipped) = zip_ztile(ty, ztile);
        let path = ([], if_left(tile_step, tile_step + 1));
        Some((path, zipped));
      };
    | [two_step, ...steps] =>
      open OptUtil.Syntax;
      let Unzipped_typ(unzipped) = unzip(two_step, zipper);
      let+ (path, _) = move(d, unzipped, (steps, k));
      (cons(two_step, path), Zipped_typ(zipper));
    };
  };
}
and Pat: {
  type zipped =
    | Zipped_pat(ZPat.zipper)
    | Zipped_exp(ZExp.zipper);

  let zip: (HPat.t, ZPat.t) => (tile_step, ZPat.zipper);
  let zip_ztile: (HPat.t, ZPat.ztile) => (two_step, zipped);

  type unzipped =
    | Unzipped_pat(ZPat.zipper)
    | Unzipped_typ(ZTyp.zipper);

  let unzip_tile: (child_step, HPat.Tile.t, ZPat.t) => unzipped;
  let unzip: (two_step, ZPat.zipper) => unzipped;

  let move: (Direction.t, ZPat.zipper, t) => option((t, zipped));
} = {
  type zipped =
    | Zipped_pat(ZPat.zipper)
    | Zipped_exp(ZExp.zipper);

  let zip = (p: HPat.t, zp: ZPat.t): (tile_step, ZPat.zipper) => (
    List.length(zp.prefix),
    (zp.prefix @ p @ zp.suffix, zp.z),
  );
  let zip_ztile = (p: HPat.t, ztile: ZPat.ztile): (two_step, zipped) =>
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

  type unzipped =
    | Unzipped_pat(ZPat.zipper)
    | Unzipped_typ(ZTyp.zipper);

  let unzip_tile = (r: child_step, tile: HPat.Tile.t, zp: ZPat.t): unzipped =>
    switch (tile) {
    | Operand(Paren(body)) when r == 0 =>
      Unzipped_pat((body, Some(Tile.Operand(ZPat.ParenZ_body(zp)))))
    | PostOp(Ann(status, ann)) when r == 0 =>
      Unzipped_typ((ann, Some(Tile.PostOp(ZTyp.AnnZ_ann(status, zp)))))
    | _ => raise(Invalid_argument("ZPath.Pat.unzip_tile"))
    };
  let unzip =
      ((l, r): two_step, (p: HPat.t, zrest: option(ZPat.ztile))): unzipped => {
    let (tile, zp) = {
      let (prefix, tile, suffix) = ListUtil.split_nth(l, p);
      (tile, ZPat.mk(~prefix, ~z=?zrest, ~suffix, ()));
    };
    unzip_tile(r, tile, zp);
  };

  let rec move =
          (d: Direction.t, (p, zrest) as zipper: ZPat.zipper, (steps, k): t)
          : option((t, zipped)) => {
    let if_left = (then_, else_) => d == Left ? then_ : else_;
    switch (steps) {
    | [] =>
      let next = if_left(k - 1, k);
      switch (List.nth_opt(p, next), zrest) {
      | (None, None) => None
      | (Some(tile), _) =>
        let path =
          switch (tile) {
          | Operand(OperandHole | Var(_))
          | BinOp(OperatorHole) => (steps, if_left(k - 1, k + 1))
          | Operand(Paren(p)) => (
              [(next, 0)],
              if_left(List.length(p), 0),
            )
          | PostOp(Ann(_, ty)) => (
              [(next, 0)],
              if_left(List.length(ty), 0),
            )
          | PreOp(_) => raise(HPat.Tile.Void_PreOp)
          };
        Some((path, Zipped_pat(zipper)));
      | (_, Some(ztile)) =>
        let ((tile_step, _), zipped) = zip_ztile(p, ztile);
        let path = ([], if_left(tile_step, tile_step + 1));
        Some((path, zipped));
      };
    | [two_step, ...steps] =>
      open OptUtil.Syntax;
      let+ path =
        switch (unzip(two_step, zipper)) {
        | Unzipped_pat(unzipped) =>
          Option.map(fst, move(d, unzipped, (steps, k)))
        | Unzipped_typ(unzipped) =>
          Option.map(fst, Typ.move(d, unzipped, (steps, k)))
        };
      (cons(two_step, path), Zipped_pat(zipper));
    };
  };
}
and Exp: {
  type zipped =
    | Zipped_exp(ZExp.zipper);

  let zip: (HExp.t, ZExp.t) => (tile_step, ZExp.zipper);
  let zip_ztile: (HExp.t, ZExp.ztile) => (two_step, zipped);

  type unzipped =
    | Unzipped_exp(ZExp.zipper)
    | Unzipped_pat(ZPat.zipper);

  let unzip_tile: (child_step, HExp.Tile.t, ZExp.t) => unzipped;
  let unzip: (two_step, ZExp.zipper) => unzipped;

  let move: (Direction.t, ZExp.zipper, t) => option((t, zipped));
} = {
  type zipped =
    | Zipped_exp(ZExp.zipper);

  let zip = (e: HExp.t, ze: ZExp.t): (tile_step, ZExp.zipper) => (
    List.length(ze.prefix),
    (ze.prefix @ e @ ze.suffix, ze.z),
  );
  let zip_ztile = (e: HExp.t, ztile: ZExp.ztile): (two_step, zipped) =>
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

  type unzipped =
    | Unzipped_exp(ZExp.zipper)
    | Unzipped_pat(ZPat.zipper);

  let unzip_tile = (r: child_step, tile: HExp.Tile.t, ze: ZExp.t): unzipped =>
    switch (tile) {
    | Operand(Paren(body)) when r == 0 =>
      Unzipped_exp((body, Some(Tile.Operand(ParenZ_body(ze)))))
    | PreOp(Lam(status, p)) when r == 0 =>
      Unzipped_pat((p, Some(Tile.PreOp(ZPat.LamZ_pat(status, ze)))))
    | PostOp(Ap(status, arg)) when r == 0 =>
      Unzipped_exp((arg, Some(Tile.PostOp(ZExp.ApZ_arg(status, ze)))))
    | _ => raise(Invalid_argument("ZPath.Exp.unzip_tile"))
    };
  let unzip =
      ((l, r): two_step, (e: HExp.t, zrest: option(ZExp.ztile))): unzipped => {
    let (tile, ze) = {
      let (prefix, tile, suffix) = ListUtil.split_nth(l, e);
      (tile, ZExp.mk(~prefix, ~z=?zrest, ~suffix, ()));
    };
    unzip_tile(r, tile, ze);
  };

  let rec move =
          (d: Direction.t, (e, zrest) as zipper: ZExp.zipper, (steps, k): t)
          : option((t, zipped)) => {
    let if_left = (then_, else_) => d == Left ? then_ : else_;
    switch (steps) {
    | [] =>
      let next = if_left(k - 1, k);
      switch (List.nth_opt(e, next), zrest) {
      | (None, None) => None
      | (Some(tile), _) =>
        let path =
          switch (tile) {
          | Operand(OperandHole | Num(_) | Var(_))
          | BinOp(Plus(_) | OperatorHole) => (steps, if_left(k - 1, k + 1))
          | Operand(Paren(e))
          | PostOp(Ap(_, e)) => (
              [(next, 0), ...steps],
              if_left(List.length(e), 0),
            )
          | PreOp(Lam(_, p)) => (
              [(next, 0), ...steps],
              if_left(List.length(p), 0),
            )
          };
        Some((path, Zipped_exp(zipper)));
      | (_, Some(ztile)) =>
        let ((tile_step, _), zipped) = zip_ztile(e, ztile);
        let path = ([], if_left(tile_step, tile_step + 1));
        Some((path, zipped));
      };
    | [two_step, ...steps] =>
      open OptUtil.Syntax;
      let+ path =
        switch (unzip(two_step, zipper)) {
        | Unzipped_exp(unzipped) =>
          Option.map(fst, move(d, unzipped, (steps, k)))
        | Unzipped_pat(unzipped) =>
          Option.map(fst, Pat.move(d, unzipped, (steps, k)))
        };
      (cons(two_step, path), Zipped_exp(zipper));
    };
  };
};
