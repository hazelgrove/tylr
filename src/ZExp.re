type term = HExp.Term.t;
type tile = HExp.Tile.t;

type t =
  | Z(list(tile), list(tile))
  | ParenZ(t)
  | IfZ_cond(t, term, term)
  | IfZ_then(term, t, term)
  | LetZ_pat(ZPat.t, term, term)
  | LetZ_def(HPat.Term.t, t, term)
  | AnnZ(term, ZTyp.t);

let rec erase: t => term =
  fun
  | Z(prefix, suffix) => HExp.parse(List.rev(prefix) @ suffix)
  | ParenZ(zbody) => Paren(erase(zbody))
  | IfZ_cond(zcond, then_, else_) => If(erase(zcond), then_, else_)
  | IfZ_then(cond, zthen, else_) => If(cond, erase(zthen), else_)
  | LetZ_pat(zp, def, body) => Let(ZPat.erase(zp), def, body)
  | LetZ_def(p, zdef, body) => Let(p, erase(zdef), body)
  | AnnZ(subj, zann) => Ann(subj, ZTyp.erase(zann));

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
