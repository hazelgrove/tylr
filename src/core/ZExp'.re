type t = ZList.t(option(ztile), HExp.Tile.t)
and ztile = Tile.t(zoperand, zpreop, zpostop, zbinop)
and zoperand =
  | ParenZ_body(t)
and zpreop = unit // empty
and zpostop =
  | ApZ_arg(HoleStatus.t, t)
and zbinop = unit; // empty

exception Void_ZPreOp;
exception Void_ZBinOp;

let index: ztile => int =
  fun
  | Operand(ParenZ_body({prefix, _})) => List.length(prefix)
  | PreOp(_) => raise(Void_ZPreOp)
  | PostOp(ApZ_arg(_, {prefix, _})) => List.length(prefix)
  | BinOp(_) => raise(Void_ZBinOp);

let split_hd: ztile => (ztile, option(ztile)) =
  fun
  | Operand(ParenZ_body({prefix, z, suffix})) => (
      Operand(ParenZ_body({prefix, z: None, suffix})),
      z,
    )
  | PreOp(_) => raise(Void_ZPreOp)
  | PostOp(ApZ_arg(status, {prefix, z, suffix})) => (
      PostOp(ApZ_arg(status, {prefix, z: None, suffix})),
      z,
    )
  | BinOp(_) => raise(Void_ZBinOp);

type zipped =
  | Exp(HExp.t, option(ztile));

let zip = (~subject: option(HExp.Tile.t)=?, ze: t): (HExp.t, option(ztile)) => {
  let subject =
    switch (subject) {
    | None => ze.prefix @ ze.suffix
    | Some(tile) => ze.prefix @ [tile, ...ze.suffix]
    };
  (subject, ze.z);
};
let zip_ztile = (subject: HExp.t, ztile: ztile): zipped =>
  switch (ztile) {
  | Operand(ParenZ_body(ze)) =>
    let (e, rest) =
      zip(~subject=Tile.Operand(HExp.Tile.Paren(subject)), ze);
    Exp(e, rest);
  | PreOp(_) => raise(Void_ZPreOp)
  | PostOp(ApZ_arg(status, ze)) =>
    let (e, rest) =
      zip(~subject=Tile.PostOp(HExp.Tile.Ap(status, subject)), ze);
    Exp(e, rest);
  | BinOp(_) => raise(Void_ZBinOp)
  };
