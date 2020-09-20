type t = ZList.t(option(ztile), HPat.Tile.t)
and ztile = Tile.t(zoperand, zpreop, zpostop, zbinop)
and zoperand =
  | ParenZ_body(t)
and zpreop =
  | LamZ_pat(HoleStatus.t, ZExp'.t)
and zpostop = unit // empty
and zbinop = unit; // empty

exception Void_ZPostOp;
exception Void_ZBinOp;

type zipped =
  | Exp(HExp.t, option(ZExp'.ztile))
  | Pat(HPat.t, option(ztile));

let index: ztile => int =
  fun
  | Operand(ParenZ_body({prefix, _})) => List.length(prefix)
  | PreOp(LamZ_pat(_, {prefix, _})) => List.length(prefix)
  | PostOp(_) => raise(Void_ZPostOp)
  | BinOp(_) => raise(Void_ZBinOp);

let zip = (~subject: option(HPat.Tile.t)=?, zp: t): (HPat.t, option(ztile)) => {
  let subject =
    switch (subject) {
    | None => zp.prefix @ zp.suffix
    | Some(tile) => zp.prefix @ [tile, ...zp.suffix]
    };
  (subject, zp.z);
};
let zip_ztile = (subject: HPat.t, ztile: ztile): zipped =>
  switch (ztile) {
  | Operand(ParenZ_body(zp)) =>
    let (p, rest) =
      zip(~subject=Tile.Operand(HPat.Tile.Paren(subject)), zp);
    Pat(p, rest);
  | PreOp(LamZ_pat(status, ze)) =>
    let (e, rest) =
      ZExp'.zip(~subject=Tile.PreOp(HExp.Tile.Lam(status, subject)), ze);
    Exp(e, rest);
  | PostOp(_) => raise(Void_ZPostOp)
  | BinOp(_) => raise(Void_ZBinOp)
  };
