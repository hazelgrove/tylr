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

type zipped = HExp.t;

let rec zip = (~subject: option(HExp.Tile.t)=?, ze: t): zipped => {
  let subject =
    switch (subject) {
    | None => ze.prefix @ ze.suffix
    | Some(tile) => ze.prefix @ [tile, ...ze.suffix]
    };
  switch (ze.z) {
  | None => subject
  | Some(ztile) => zip_ztile(subject, ztile)
  };
}
and zip_ztile = (subject: HExp.t, ztile: ztile): zipped =>
  switch (ztile) {
  | Operand(ParenZ_body(ze)) =>
    zip(~subject=Tile.Operand(HExp.Tile.Paren(subject)), ze)
  | PreOp(_) => raise(Void_ZPreOp)
  | PostOp(ApZ_arg(status, ze)) =>
    zip(~subject=Tile.PostOp(HExp.Tile.Ap(status, subject)), ze)
  | BinOp(_) => raise(Void_ZBinOp)
  };
