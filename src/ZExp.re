// 1 : + : (2 + 3) | + : 4
// 1 + ( 2 : + : 3| ) + 4
type t =
  | Y(list(HExp.Tile.t), list(HExp.Tile.t))
  | Z(unzipped)
and unzipped =
  | ParenZ(t)
  | LamZ_pat(HoleStatus.t, ZPat.t, HExp.t)
  | LamZ_body(HoleStatus.t, HPat.t, unzipped)
  | PlusZ_l(HoleStatus.t, unzipped, HExp.t)
  | PlusZ_r(HoleStatus.t, HExp.t, unzipped)
  | ApZ_fn(HoleStatus.t, unzipped, HExp.t)
  | ApZ_arg(HoleStatus.t, HExp.t, t)
  | OperatorHoleZ_l(unzipped, HExp.t)
  | OperatorHoleZ_r(HExp.t, unzipped);

let rec set_hole_status = (status, ze) =>
  switch (ze) {
  | Y(prefix, suffix) =>
    let n = List.length(prefix);
    let (prefix, suffix) =
      List.rev(prefix)
      @ suffix
      |> HExp.parse
      |> HExp.set_hole_status(status)
      |> HExp.Tile.unparse
      |> ListUtil.split_n(n);
    Y(prefix, suffix);
  | Z(z) => Z(set_hole_status_unzipped(status, z))
  }
and set_hole_status_unzipped = (status, z) =>
  switch (z) {
  | ParenZ(zbody) => ParenZ(set_hole_status(status, zbody))
  | LamZ_pat(_, zp, body) => LamZ_pat(status, zp, body)
  | LamZ_body(_, p, zbody) => LamZ_body(status, p, zbody)
  | PlusZ_l(_, zl, r) => PlusZ_l(status, zl, r)
  | PlusZ_r(_, l, zr) => PlusZ_r(status, l, zr)
  | ApZ_fn(_, zfn, arg) => ApZ_fn(status, zfn, arg)
  | ApZ_arg(_, fn, zarg) => ApZ_arg(status, fn, zarg)
  | OperatorHoleZ_l(_)
  | OperatorHoleZ_r(_) => z
  };

let rec erase: t => HExp.t =
  fun
  | Y(prefix, suffix) => HExp.parse(List.rev(prefix) @ suffix)
  | Z(z) => erase_unzipped(z)
and erase_unzipped: unzipped => HExp.t =
  fun
  | ParenZ(zbody) => Paren(erase(zbody))
  | LamZ_pat(status, zp, body) => Lam(status, ZPat.erase(zp), body)
  | LamZ_body(status, p, zbody) => Lam(status, p, erase_unzipped(zbody))
  | ApZ_fn(status, zfn, arg) => Ap(status, erase_unzipped(zfn), arg)
  | ApZ_arg(status, fn, zarg) => Ap(status, fn, erase(zarg))
  | PlusZ_l(status, zl, r) => Plus(status, erase_unzipped(zl), r)
  | PlusZ_r(status, l, zr) => Plus(status, l, erase_unzipped(zr))
  | OperatorHoleZ_l(zl, r) => OperatorHole(erase_unzipped(zl), r)
  | OperatorHoleZ_r(l, zr) => OperatorHole(l, erase_unzipped(zr));

let place_before = (e: HExp.t): t => Y([], HExp.Tile.unparse(e));
let place_after = (e: HExp.t): t => Y(HExp.Tile.unparse(e), []);

let move_left =
    (prefix: list(HExp.Tile.t), suffix: list(HExp.Tile.t)): option(t) => {
  let rec go = (n: int, e: HExp.t) =>
    switch (e) {
    | _ when n <= 0 => None
    | OperandHole
    | Num(_)
    | Var(_) => Some(place_before(e))
    | Paren(body) =>
      let zbody = place_after(body);
      Some(Z(ParenZ(zbody)));
    | Lam(status, p, body) =>
      if (n == 1) {
        let zp = ZPat.place_after(p);
        Some(Z(LamZ_pat(status, zp, body)));
      } else {
        go(n - 1, body)
        |> Option.map(
             fun
             | Y(pre, suf) => Y(pre @ [Lam(status, p)], suf)
             | Z(zbody) => Z(LamZ_body(status, p, zbody)),
           );
      }
    | Ap(status, fn, arg) =>
      let fn_tiles = HExp.Tile.unparse(fn);
      let m = List.length(fn_tiles);
      if (n <= m) {
        go(n, fn)
        |> Option.map(
             fun
             | Y(pre, suf) => Y(pre, suf @ [Ap(status, arg)])
             | Z(zfn) => Z(ApZ_fn(status, zfn, arg)),
           );
      } else {
        let zarg = place_after(arg);
        Some(Z(ApZ_arg(status, fn, zarg)));
      };
    | Plus(status, l, r) =>
      let l_tiles = HExp.Tile.unparse(l);
      let r_tiles = HExp.Tile.unparse(r);
      let m = List.length(l_tiles);
      if (n <= m) {
        go(n, l)
        |> Option.map(
             fun
             | Y(pre, suf) => Y(pre, suf @ [Plus(status), ...r_tiles])
             | Z(zl) => Z(PlusZ_l(status, zl, r)),
           );
      } else if (n == m + 1) {
        Some(Y(l_tiles, [Plus(status), ...r_tiles]));
      } else {
        go(n - (m + 1), r)
        |> Option.map(
             fun
             | Y(pre, suf) =>
               Y(pre @ [Plus(status), ...List.rev(l_tiles)], suf)
             | Z(zr) => Z(PlusZ_r(status, l, zr)),
           );
      };
    | OperatorHole(l, r) =>
      let l_tiles = HExp.Tile.unparse(l);
      let r_tiles = HExp.Tile.unparse(r);
      let m = List.length(l_tiles);
      if (n <= m) {
        go(n, l)
        |> Option.map(
             fun
             | Y(pre, suf) => Y(pre, suf @ [OperatorHole, ...r_tiles])
             | Z(zl) => Z(OperatorHoleZ_l(zl, r)),
           );
      } else if (n == m + 1) {
        Some(Y(l_tiles, [OperatorHole, ...r_tiles]));
      } else {
        go(n - (m + 1), r)
        |> Option.map(
             fun
             | Y(pre, suf) =>
               Y(pre @ [OperatorHole, ...List.rev(l_tiles)], suf)
             | Z(zr) => Z(OperatorHoleZ_r(l, zr)),
           );
      };
    };
  go(List.length(prefix), HExp.parse(List.rev(prefix) @ suffix));
};

let move_right =
    (prefix: list(HExp.Tile.t), suffix: list(HExp.Tile.t)): option(t) => {
  let rec go = (n: int, e: HExp.t) =>
    switch (e) {
    | _ when n >= List.length(HExp.Tile.unparse(e)) => None
    | OperandHole
    | Num(_)
    | Var(_) => Some(place_after(e))
    | Paren(body) =>
      let zbody = place_before(body);
      Some(Z(ParenZ(zbody)));
    | Lam(status, p, body) =>
      if (n == 0) {
        let zp = ZPat.place_before(p);
        Some(Z(LamZ_pat(status, zp, body)));
      } else {
        go(n - 1, body)
        |> Option.map(
             fun
             | Y(pre, suf) => Y(pre @ [Lam(status, p)], suf)
             | Z(zbody) => Z(LamZ_body(status, p, zbody)),
           );
      }
    | Ap(status, fn, arg) =>
      let fn_tiles = HExp.Tile.unparse(fn);
      let m = List.length(fn_tiles);
      if (n < m) {
        go(n, fn)
        |> Option.map(
             fun
             | Y(pre, suf) => Y(pre, suf @ [Ap(status, arg)])
             | Z(zfn) => Z(ApZ_fn(status, zfn, arg)),
           );
      } else {
        let zarg = place_before(arg);
        Some(Z(ApZ_arg(status, fn, zarg)));
      };
    | Plus(status, l, r) =>
      let l_tiles = HExp.Tile.unparse(l);
      let r_tiles = HExp.Tile.unparse(r);
      let m = List.length(l_tiles);
      if (n < m) {
        go(n, l)
        |> Option.map(
             fun
             | Y(pre, suf) => Y(pre, suf @ [Plus(status), ...r_tiles])
             | Z(zl) => Z(PlusZ_l(status, zl, r)),
           );
      } else if (n == m) {
        Some(Y([Plus(status), ...l_tiles], r_tiles));
      } else {
        go(n - (m + 1), r)
        |> Option.map(
             fun
             | Y(pre, suf) =>
               Y(pre @ [Plus(status), ...List.rev(l_tiles)], suf)
             | Z(zr) => Z(PlusZ_r(status, l, zr)),
           );
      };
    | OperatorHole(l, r) =>
      let l_tiles = HExp.Tile.unparse(l);
      let r_tiles = HExp.Tile.unparse(r);
      let m = List.length(l_tiles);
      if (n < m) {
        go(n, l)
        |> Option.map(
             fun
             | Y(pre, suf) => Y(pre, suf @ [OperatorHole, ...r_tiles])
             | Z(zl) => Z(OperatorHoleZ_l(zl, r)),
           );
      } else if (n == m) {
        Some(Y([OperatorHole, ...l_tiles], r_tiles));
      } else {
        go(n - (m + 1), r)
        |> Option.map(
             fun
             | Y(pre, suf) =>
               Y(pre @ [OperatorHole, ...List.rev(l_tiles)], suf)
             | Z(zr) => Z(OperatorHoleZ_r(l, zr)),
           );
      };
    };
  go(List.length(prefix), HExp.parse(List.rev(prefix) @ suffix));
};

/*
 let rec insert_tiles =
         (tiles: HExp.Tile.s, {prefix, z, suffix}: t): option(t) => {
   let wrap_z = z => {prefix, z: ztile, suffix};
   switch (z) {
   | Z => Some(mk((prefix, Z, tiles @ suffix)))
   | ParenZ(zbody) =>
     insert_tiles(tiles, zbody)
     |> Option.map(zbody => wrap_ztile(ParenZ(zbody)))
   | IfZ_if(zcond, then_clause) =>
     insert_tiles(tiles, zcond)
     |> Option.map(zcond => wrap_ztile(IfZ_if(zcond, then_clause)))
   | IfZ_then(cond, zthen) =>
     insert_tiles(tiles, zthen)
     |> Option.map(zthen => wrap_ztile(IfZ_then(cond, zthen)))
   | LetZ_pat(_) => None
   | LetZ_def(p, zdef) =>
     insert_tiles(tiles, zdef)
     |> Option.map(zdef => wrap_ztile(LetZ_def(p, zdef)))
   | AnnZ(_) => None
   };
 };
 */
