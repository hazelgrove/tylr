type t =
  | Y(list(HTyp.Tile.t), list(HTyp.Tile.t))
  | Z(unzipped)
and unzipped =
  | ParenZ(t)
  | ArrowZ_l(unzipped, HTyp.t)
  | ArrowZ_r(HTyp.t, unzipped)
  | OperatorHoleZ_l(unzipped, HTyp.t)
  | OperatorHoleZ_r(HTyp.t, unzipped);

let rec erase: t => HTyp.t =
  fun
  | Y(prefix, suffix) => HTyp.parse(List.rev(prefix) @ suffix)
  | Z(z) => erase_unzipped(z)
and erase_unzipped: unzipped => HTyp.t =
  fun
  | ParenZ(zbody) => Paren(erase(zbody))
  | ArrowZ_l(zl, r) => Arrow(erase_unzipped(zl), r)
  | ArrowZ_r(l, zr) => Arrow(l, erase_unzipped(zr))
  | OperatorHoleZ_l(zl, r) => OperatorHole(erase_unzipped(zl), r)
  | OperatorHoleZ_r(l, zr) => OperatorHole(l, erase_unzipped(zr));

let place_before = (ty: HTyp.t): t => Y([], HTyp.Tile.unparse(ty));
let place_after = (ty: HTyp.t): t => Y(HTyp.Tile.unparse(ty), []);

let move_left =
    (prefix: list(HTyp.Tile.t), suffix: list(HTyp.Tile.t)): option(t) => {
  let rec go = (n: int, ty: HTyp.t) =>
    switch (ty) {
    | _ when n <= 0 => None
    | OperandHole
    | Num => Some(place_before(ty))
    | Paren(body) =>
      let zbody = place_after(body);
      Some(Z(ParenZ(zbody)));
    | Arrow(l, r) =>
      let l_tiles = HTyp.Tile.unparse(l);
      let r_tiles = HTyp.Tile.unparse(r);
      let m = List.length(l_tiles);
      if (n <= m) {
        go(n, l)
        |> Option.map(
             fun
             | Y(pre, suf) => Y(pre, suf @ [Arrow, ...r_tiles])
             | Z(zl) => Z(ArrowZ_l(zl, r)),
           );
      } else if (n == m + 1) {
        Some(Y(l_tiles, [Arrow, ...r_tiles]));
      } else {
        go(n - (m + 1), r)
        |> Option.map(
             fun
             | Y(pre, suf) => Y(pre @ [Arrow, ...List.rev(l_tiles)], suf)
             | Z(zr) => Z(ArrowZ_r(l, zr)),
           );
      };
    | OperatorHole(l, r) =>
      let l_tiles = HTyp.Tile.unparse(l);
      let r_tiles = HTyp.Tile.unparse(r);
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
  go(List.length(prefix), HTyp.parse(List.rev(prefix) @ suffix));
};

let move_right =
    (prefix: list(HTyp.Tile.t), suffix: list(HTyp.Tile.t)): option(t) => {
  let rec go = (n: int, ty: HTyp.t) => {
    let len = List.length(HTyp.Tile.unparse(ty));
    switch (ty) {
    | _ when n >= len => None
    | OperandHole
    | Num => Some(place_after(ty))
    | Paren(body) =>
      let zbody = place_before(body);
      Some(Z(ParenZ(zbody)));
    | Arrow(l, r) =>
      let l_tiles = HTyp.Tile.unparse(l);
      let r_tiles = HTyp.Tile.unparse(r);
      let m = List.length(l_tiles);
      if (n < m) {
        go(n, l)
        |> Option.map(
             fun
             | Y(pre, suf) => Y(pre, suf @ [Arrow, ...r_tiles])
             | Z(zl) => Z(ArrowZ_l(zl, r)),
           );
      } else if (n == m) {
        Some(Y([Arrow, ...l_tiles], r_tiles));
      } else {
        go(n - (m + 1), r)
        |> Option.map(
             fun
             | Y(pre, suf) => Y(pre @ [Arrow, ...List.rev(l_tiles)], suf)
             | Z(zr) => Z(ArrowZ_r(l, zr)),
           );
      };
    | OperatorHole(l, r) =>
      let l_tiles = HTyp.Tile.unparse(l);
      let r_tiles = HTyp.Tile.unparse(r);
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
  };
  go(List.length(prefix), HTyp.parse(List.rev(prefix) @ suffix));
};
