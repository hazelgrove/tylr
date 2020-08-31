type t = ZTiles.t(ztile, HPat.Tile.t)
and ztile =
  | ParenZ_body(t)
  | AnnZ_ann(HoleStatus.t, ZTyp.t);

let rec erase = (zp: t): HPat.t =>
  switch (zp.z) {
  | None => HPat.parse(zp.prefix @ zp.suffix)
  | Some(ztile) =>
    HPat.parse(zp.prefix @ [erase_ztile(ztile), ...zp.suffix])
  }
and erase_ztile: ztile => HPat.Tile.t =
  fun
  | ParenZ_body(zbody) => Paren(erase(zbody))
  | AnnZ_ann(status, zann) => Ann(status, ZTyp.erase(zann));

let rec set_hole_status = (status, zp) => {
  let (n, _) = HPat.Tile.root(erase(zp));
  let m = List.length(zp.prefix);
  let set_tile = HPat.Tile.set_hole_status(status);
  if (n < m) {
    let prefix = zp.prefix |> ListUtil.map_nth(n, set_tile);
    {...zp, prefix};
  } else {
    switch (zp.z) {
    | None =>
      let suffix = zp.suffix |> ListUtil.map_nth(n - m, set_tile);
      {...zp, suffix};
    | Some(ztile) =>
      if (n == m) {
        let z = Some(set_hole_status_ztile(status, ztile));
        {...zp, z};
      } else {
        let suffix = zp.suffix |> ListUtil.map_nth(n - (m + 1), set_tile);
        {...zp, suffix};
      }
    };
  };
}
and set_hole_status_ztile = (status, ztile) =>
  switch (ztile) {
  | ParenZ_body(zp) => ParenZ_body(set_hole_status(status, zp))
  | AnnZ_ann(_, ann) => AnnZ_ann(status, ann)
  };

let place_before = (p: HPat.t): t => ZTiles.place_before(HPat.unparse(p));
let place_after = (p: HPat.t): t => ZTiles.place_after(HPat.unparse(p));

let enter_from_left: HPat.Tile.t => option(ztile) =
  fun
  | OperandHole
  | OperatorHole
  | Var(_) => None
  | Paren(body) => Some(ParenZ_body(place_before(body)))
  | Ann(status, ann) => Some(AnnZ_ann(status, ZTyp.place_before(ann)));
let enter_from_right: HPat.Tile.t => option(ztile) =
  fun
  | OperandHole
  | OperatorHole
  | Var(_) => None
  | Paren(body) => Some(ParenZ_body(place_after(body)))
  | Ann(status, ann) => Some(AnnZ_ann(status, ZTyp.place_after(ann)));

let move_left = ZTiles.move_left(~enter_from_right);
let move_right = ZTiles.move_right(~enter_from_left);
