type t('zoperand, 'zpreop, 'zpostop, 'zbinop, 'tile) = (
  Skel.t,
  ZTiles.t(ZTile.t('zoperand, 'zpreop, 'zpostop, 'zbinop), 'tile),
);

type zroot('zoperand, 'zpreop, 'zpostop, 'zbinop) =
  | ZOperand('zoperand)
  | ZPreOp('zpreop, Skel.t)
  | ZPostOp(Skel.t, 'zpostop)
  | ZBinOp(Skel.t, 'zbinop, Skel.t);

let get_zroot = ((skel, {prefix, z, suffix: _}): t(_)): option(zroot(_)) => {
  switch (z) {
  | None => None
  | Some(ztile) =>
    let n = List.length(prefix);
    let rec go = (skel: Skel.t) =>
      switch (skel) {
      | Operand(m) =>
        assert(n == m);
        ZOperand(ZTile.get_zoperand(ztile));
      | PreOp(m, skel) =>
        assert(n >= m);
        n == m ? ZPreOp(ZTile.get_zpreop(ztile), skel) : go(skel);
      | PostOp(skel, m) =>
        assert(n <= m);
        n == m ? ZPostOp(skel, ZTile.get_zpostop(ztile)) : go(skel);
      | BinOp(skel1, m, skel2) =>
        if (n < m) {
          go(skel1);
        } else if (n > m) {
          go(skel2);
        } else {
          ZBinOp(skel1, ZTile.get_zbinop(ztile), skel2);
        }
      };
    Some(go(skel));
  };
};

let erase =
    (~erase_ztile, (skel, ztiles): t(_, _, _, _, 'tile))
    : (Skel.t, list('tile)) => (
  skel,
  ZTiles.erase(~erase_ztile, ztiles),
);

let place_before = ((skel, tiles)): t(_) => (
  skel,
  ZTiles.place_before(tiles),
);
let place_after = ((skel, tiles)): t(_) => (
  skel,
  ZTiles.place_after(tiles),
);

let set_hole_status =
    (
      ~set_tile: (HoleStatus.t, 'tile) => 'tile,
      ~set_ztile: (HoleStatus.t, ZTile.t(_)) => ZTile.t(_),
      status: HoleStatus.t,
      (skel, ztiles): t(_, _, _, _, 'tile),
    )
    : t(_, _, _, _, 'tile) => {
  let root_index = Skel.root_index(skel);
  let z_index = ZTiles.z_index(ztiles);
  let ztiles =
    if (root_index < z_index) {
      let prefix =
        ztiles.prefix |> ListUtil.map_nth(root_index, set_tile(status));
      {...ztiles, prefix};
    } else {
      switch (ztiles.z) {
      | None =>
        let suffix =
          ztiles.suffix
          |> ListUtil.map_nth(root_index - z_index, set_tile(status));
        {...ztiles, suffix};
      | Some(ztile) =>
        if (root_index == z_index) {
          let z = Some(set_ztile(status, ztile));
          {...ztiles, z};
        } else {
          let suffix =
            ztiles.suffix
            |> ListUtil.map_nth(root_index - (z_index + 1), set_tile(status));
          {...ztiles, suffix};
        }
      };
    };
  (skel, ztiles);
};
