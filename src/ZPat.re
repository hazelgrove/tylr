type t = (Skel.t, ztiles)
and ztiles = ZTiles.t(ztile, HPat.Tile.t)
and ztile = ZTile.t(zoperand, zpreop, zpostop, zbinop)
and zoperand =
  | ParenZ_body(t)
and zpreop = unit // empty
and zpostop =
  | AnnZ_ann(HoleStatus.t, ZTyp.t)
and zbinop = unit; // empty

exception Void_ZPreOp;
exception Void_ZBinOp;

let rec erase = (zp: t): HPat.t => ZTerm.erase(~erase_ztile, zp)
and erase_ztile: ztile => HPat.Tile.t =
  fun
  | ZOperand(ParenZ_body(zbody)) => Operand(Paren(erase(zbody)))
  | ZPreOp(_) => raise(Void_ZPreOp)
  | ZPostOp(AnnZ_ann(status, zann)) =>
    PostOp(Ann(status, ZTyp.erase(zann)))
  | ZBinOp(_) => raise(Void_ZBinOp);

let rec set_hole_status = (status, zp) =>
  ZTerm.set_hole_status(~set_tile=HPat.set_tile, ~set_ztile, status, zp)
and set_ztile = (status, ztile) =>
  ztile
  |> ZTile.map(
       ~zoperand=
         fun
         | ParenZ_body(zp) => ParenZ_body(set_hole_status(status, zp)),
       ~zpreop=
         fun
         | _ => raise(Void_ZPreOp),
       ~zpostop=
         fun
         | AnnZ_ann(_, ann) => AnnZ_ann(status, ann),
       ~zbinop=
         fun
         | _ => raise(Void_ZBinOp),
     );

let place_before: HPat.t => t = ZTerm.place_before;
let place_after: HPat.t => t = ZTerm.place_after;

let enter_from_left: HPat.Tile.t => option(ztile) =
  fun
  | Operand(OperandHole | Var(_)) => None
  | Operand(Paren(body)) =>
    Some(ZOperand(ParenZ_body(place_before(body))))
  | PreOp(_) => raise(HPat.Void_PreOp)
  | PostOp(Ann(status, ann)) =>
    Some(ZPostOp(AnnZ_ann(status, ZTyp.place_before(ann))))
  | BinOp(OperatorHole) => None;
let enter_from_right: HPat.Tile.t => option(ztile) =
  fun
  | Operand(OperandHole | Var(_)) => None
  | Operand(Paren(body)) =>
    Some(ZOperand(ParenZ_body(place_after(body))))
  | PreOp(_) => raise(HPat.Void_PreOp)
  | PostOp(Ann(status, ann)) =>
    Some(ZPostOp(AnnZ_ann(status, ZTyp.place_after(ann))))
  | BinOp(OperatorHole) => None;

let move_left = ZTiles.move_left(~enter_from_right);
let move_right = ZTiles.move_right(~enter_from_left);
