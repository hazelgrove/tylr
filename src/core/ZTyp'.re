type t = ZList.t(option(ztile), HTyp.Tile.t)
and ztile = Tile.t(zoperand, zpreop, zpostop, zbinop)
and zoperand =
  | ParenZ_body(t)
and zpreop = unit // empty
and zpostop =
  | AnnZ_ann(HoleStatus.t, ZPat'.t)
and zbinop = unit; // empty

exception Void_ZPreOp;
exception Void_ZBinOp;

type zipped =
  | Pat(HPat.t, option(ZPat'.ztile))
  | Typ(HTyp.t, option(ztile));

let index: ztile => int =
  fun
  | Operand(ParenZ_body({prefix, _})) => List.length(prefix)
  | PreOp(_) => raise(Void_ZPreOp)
  | PostOp(AnnZ_ann(_, {prefix, _})) => List.length(prefix)
  | BinOp(_) => raise(Void_ZBinOp);

let zip =
    (~subject: option(HTyp.Tile.t)=?, zty: t): (HTyp.t, option(ztile)) => {
  let subject =
    switch (subject) {
    | None => zty.prefix @ zty.suffix
    | Some(tile) => zty.prefix @ [tile, ...zty.suffix]
    };
  (subject, zty.z);
};
let zip_ztile = (subject: HTyp.t, ztile: ztile): zipped =>
  switch (ztile) {
  | Operand(ParenZ_body(zty)) =>
    let (ty, rest) =
      zip(~subject=Tile.Operand(HTyp.Tile.Paren(subject)), zty);
    Typ(ty, rest);
  | PreOp(_) => raise(Void_ZPreOp)
  | PostOp(AnnZ_ann(status, zp)) =>
    let (p, rest) =
      ZPat'.zip(~subject=Tile.PostOp(HPat.Tile.Ann(status, subject)), zp);
    Pat(p, rest);
  | BinOp(_) => raise(Void_ZBinOp)
  };
