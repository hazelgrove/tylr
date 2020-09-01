type t = (Skel.t, ZTiles.t(ztile, HTyp.Tile.t))
and ztile =
  | ParenZ_body(t);

let rec erase = (zty: t): HTyp.t => ZTerm.erase(~erase_ztile, zty)
and erase_ztile: ztile => HTyp.Tile.t =
  fun
  | ParenZ_body(zbody) => Operand(Paren(erase(zbody)));

let place_before: HTyp.t => t = ZTerm.place_before;
let place_after: HTyp.t => t = ZTerm.place_after;

let enter_from_left: HTyp.Tile.t => option(ztile) =
  fun
  | Operand(OperandHole | Num) => None
  | Operand(Paren(body)) => Some(ParenZ_body(place_after(body)))
  | PreOp(_) => raise(HTyp.Void_PreOp)
  | PostOp(_) => raise(HTyp.Void_PostOp)
  | BinOp(Arrow | OperatorHole) => None;
let enter_from_right: HTyp.Tile.t => option(ztile) =
  fun
  | Operand(OperandHole | Num) => None
  | Operand(Paren(body)) => Some(ParenZ_body(place_before(body)))
  | PreOp(_) => raise(HTyp.Void_PreOp)
  | PostOp(_) => raise(HTyp.Void_PostOp)
  | BinOp(Arrow | OperatorHole) => None;

let move_left = ZTiles.move_left(~enter_from_right);
let move_right = ZTiles.move_right(~enter_from_left);
