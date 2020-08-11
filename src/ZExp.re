type t =
  | Z(list(HExp.Tile.t), list(HExp.Tile.t))
  | ParenZ(t)
  | LamZ(ZPat.t, HExp.t)
  | ApZ(HExp.t, t);

let rec erase: t => HExp.t =
  fun
  | Z(prefix, suffix) => HExp.parse(List.rev(prefix) @ suffix)
  | ParenZ(zbody) => Paren(erase(zbody))
  | LamZ(zp, body) => Lam(ZPat.erase(zp), body)
  | ApZ(fn, zarg) => Ap(fn, erase(zarg));

/*
 let rec insert_tiles =
         (tiles: HExp.Tile.s, {prefix, z, suffix}: t): option(t) => {
   let wrap_z = z => {prefix, z: ztile, suffix};
   switch (z) {
   | Z => Some(mk((prefix, Z, tiles @ suffix)))
   | ParenZ(zbody) =>
     insert_tiles(tiles, zbody)
     |> Option.map(zbody => wrap_ztile(ParenZ(zbody)))
   | IfZ_if(zcond, then_clause) =>
     insert_tiles(tiles, zcond)
     |> Option.map(zcond => wrap_ztile(IfZ_if(zcond, then_clause)))
   | IfZ_then(cond, zthen) =>
     insert_tiles(tiles, zthen)
     |> Option.map(zthen => wrap_ztile(IfZ_then(cond, zthen)))
   | LetZ_pat(_) => None
   | LetZ_def(p, zdef) =>
     insert_tiles(tiles, zdef)
     |> Option.map(zdef => wrap_ztile(LetZ_def(p, zdef)))
   | AnnZ(_) => None
   };
 };
 */
