type t = ZTiles.t(ztile, HTyp.Tile.t)
and ztile =
  | ParenZ_body(t);

let rec erase = (zty: t): HTyp.t =>
  switch (zty.z) {
  | None => HTyp.parse(zty.prefix @ zty.suffix)
  | Some(ztile) =>
    HTyp.parse(zty.prefix @ [erase_ztile(ztile), ...zty.suffix])
  }
and erase_ztile: ztile => HTyp.Tile.t =
  fun
  | ParenZ_body(zbody) => Paren(erase(zbody));

let place_before = (ty: HTyp.t): t => ZTiles.place_before(HTyp.unparse(ty));
let place_after = (ty: HTyp.t): t => ZTiles.place_after(HTyp.unparse(ty));

let enter_from_left: HTyp.Tile.t => option(ztile) =
  fun
  | OperandHole
  | Num
  | Arrow
  | OperatorHole => None
  | Paren(body) => Some(ParenZ_body(place_after(body)));
let enter_from_right: HTyp.Tile.t => option(ztile) =
  fun
  | OperandHole
  | Num
  | Arrow
  | OperatorHole => None
  | Paren(body) => Some(ParenZ_body(place_before(body)));

let move_left = ZTiles.move_left(~enter_from_right);
let move_right = ZTiles.move_right(~enter_from_left);
