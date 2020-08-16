type t =
  | Z(list(HExp.Tile.t), list(HExp.Tile.t))
  | ParenZ(t)
  | LamZ(HoleStatus.t, ZPat.t, HExp.t)
  | ApZ(HoleStatus.t, HExp.t, t);

let rec set_hole_status = (status, ze) =>
  switch (ze) {
  | Z(prefix, suffix) =>
    let n = List.length(prefix);
    let (prefix, suffix) =
      List.rev(prefix)
      @ suffix
      |> HExp.parse
      |> HExp.set_hole_status(status)
      |> HExp.Tile.unparse
      |> ListUtil.split_n(n);
    Z(prefix, suffix);
  | ParenZ(zbody) => ParenZ(set_hole_status(status, zbody))
  | LamZ(_, zp, body) => LamZ(status, zp, body)
  | ApZ(_, fn, zarg) => ApZ(status, fn, zarg)
  };

let rec erase: t => HExp.t =
  fun
  | Z(prefix, suffix) => HExp.parse(List.rev(prefix) @ suffix)
  | ParenZ(zbody) => Paren(erase(zbody))
  | LamZ(status, zp, body) => Lam(status, ZPat.erase(zp), body)
  | ApZ(status, fn, zarg) => Ap(status, fn, erase(zarg));

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
