type t =
  | Y(list(HPat.Tile.t), list(HPat.Tile.t))
  | Z(unzipped)
and unzipped =
  | ParenZ(t)
  | AnnZ_subj(HoleStatus.t, unzipped, HTyp.t)
  | AnnZ_ann(HoleStatus.t, HPat.t, ZTyp.t)
  | OperatorHoleZ_l(unzipped, HPat.t)
  | OperatorHoleZ_r(HPat.t, unzipped);

let rec set_hole_status = (status, zp) =>
  switch (zp) {
  | Y(prefix, suffix) =>
    let n = List.length(prefix);
    let (prefix, suffix) =
      List.rev(prefix)
      @ suffix
      |> HPat.parse
      |> HPat.set_hole_status(status)
      |> HPat.Tile.unparse
      |> ListUtil.split_n(n);
    Y(prefix, suffix);
  | Z(z) => Z(set_hole_status_unzipped(status, z))
  }
and set_hole_status_unzipped = (status, z) =>
  switch (z) {
  | ParenZ(zbody) => ParenZ(set_hole_status(status, zbody))
  | AnnZ_subj(_, zsubj, ann) => AnnZ_subj(status, zsubj, ann)
  | AnnZ_ann(_, subj, zann) => AnnZ_ann(status, subj, zann)
  | OperatorHoleZ_l(_)
  | OperatorHoleZ_r(_) => z
  };

let rec erase: t => HPat.t =
  fun
  | Y(prefix, suffix) => HPat.parse(List.rev(prefix) @ suffix)
  | Z(z) => erase_unzipped(z)
and erase_unzipped: unzipped => HPat.t =
  fun
  | ParenZ(zbody) => Paren(erase(zbody))
  | AnnZ_subj(status, zsubj, ann) => Ann(status, erase_unzipped(zsubj), ann)
  | AnnZ_ann(status, subj, zann) => Ann(status, subj, ZTyp.erase(zann))
  | OperatorHoleZ_l(zl, r) => OperatorHole(erase_unzipped(zl), r)
  | OperatorHoleZ_r(l, zr) => OperatorHole(l, erase_unzipped(zr));

let place_before = (p: HPat.t): t => Y([], HPat.Tile.unparse(p));
let place_after = (p: HPat.t): t => Y(HPat.Tile.unparse(p), []);

let move_left =
    (prefix: list(HPat.Tile.t), suffix: list(HPat.Tile.t)): option(t) => {
  let rec go = (n: int, p: HPat.t) =>
    switch (p) {
    | _ when n <= 0 => None
    | OperandHole
    | Var(_) => Some(place_before(p))
    | Paren(body) =>
      let zbody = place_after(body);
      Some(Z(ParenZ(zbody)));
    | Ann(status, subj, ann) =>
      let subj_tiles = HPat.Tile.unparse(subj);
      let m = List.length(subj_tiles);
      if (n <= m) {
        go(n, subj)
        |> Option.map(
             fun
             | Y(pre, suf) => Y(pre, suf @ [Ann(status, ann)])
             | Z(zsubj) => Z(AnnZ_subj(status, zsubj, ann)),
           );
      } else {
        let zann = ZTyp.place_after(ann);
        Some(Z(AnnZ_ann(status, subj, zann)));
      };
    | OperatorHole(l, r) =>
      let l_tiles = HPat.Tile.unparse(l);
      let r_tiles = HPat.Tile.unparse(r);
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
  go(List.length(prefix), HPat.parse(List.rev(prefix) @ suffix));
};

let move_right =
    (prefix: list(HPat.Tile.t), suffix: list(HPat.Tile.t)): option(t) => {
  let rec go = (n: int, p: HPat.t) => {
    let len = List.length(HPat.Tile.unparse(p));
    switch (p) {
    | _ when n >= len => None
    | OperandHole
    | Var(_) => Some(place_after(p))
    | Paren(body) =>
      let zbody = place_before(body);
      Some(Z(ParenZ(zbody)));
    | Ann(status, subj, ann) =>
      let subj_tiles = HPat.Tile.unparse(subj);
      let m = List.length(subj_tiles);
      if (n < m) {
        go(n, subj)
        |> Option.map(
             fun
             | Y(pre, suf) => Y(pre, suf @ [Ann(status, ann)])
             | Z(zsubj) => Z(AnnZ_subj(status, zsubj, ann)),
           );
      } else {
        let zann = ZTyp.place_after(ann);
        Some(Z(AnnZ_ann(status, subj, zann)));
      };
    | OperatorHole(l, r) =>
      let l_tiles = HPat.Tile.unparse(l);
      let r_tiles = HPat.Tile.unparse(r);
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
  go(List.length(prefix), HPat.parse(List.rev(prefix) @ suffix));
};
