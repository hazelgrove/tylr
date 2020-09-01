type t('ztile, 'tile) = (Skel.t, ZTiles.t('ztile, 'tile));

let erase =
    (~erase_ztile, (skel, ztiles): t('ztile, 'tile))
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
      ~set_ztile: (HoleStatus.t, 'ztile) => 'ztile,
      status: HoleStatus.t,
      (skel, ztiles): t('ztile, 'tile),
    )
    : t('ztile, 'tile) => {
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
