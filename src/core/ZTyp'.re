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
  | Exp(HExp.t)
  | Pat(HPat.t)
  | Typ(HTyp.t);

let index: ztile => int =
  fun
  | Operand(ParenZ_body({prefix, _})) => List.length(prefix)
  | PreOp(_) => raise(Void_ZPreOp)
  | PostOp(AnnZ_ann(_, {prefix, _})) => List.length(prefix)
  | BinOp(_) => raise(Void_ZBinOp);

let rec zip = (~subject: option(HTyp.Tile.t)=?, zty: t): zipped => {
  let subject =
    switch (subject) {
    | None => zty.prefix @ zty.suffix
    | Some(tile) => zty.prefix @ [tile, ...zty.suffix]
    };
  switch (zty.z) {
  | None => Typ(subject)
  | Some(ztile) => zip_ztile(subject, ztile)
  };
}
and zip_ztile = (subject: HTyp.t, ztile: ztile): zipped =>
  switch (ztile) {
  | Operand(ParenZ_body(zty)) =>
    zip(~subject=Tile.Operand(HTyp.Tile.Paren(subject)), zty)
  | PreOp(_) => raise(Void_ZPreOp)
  | PostOp(AnnZ_ann(status, zp)) =>
    switch (
      ZPat'.zip(~subject=Tile.PostOp(HPat.Tile.Ann(status, subject)), zp)
    ) {
    | Pat(p) => Pat(p)
    | Exp(e) => Exp(e)
    }
  | BinOp(_) => raise(Void_ZBinOp)
  };
