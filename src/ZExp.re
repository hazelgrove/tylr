type t =
  | Y(list(HExp.Tile.t), list(HExp.Tile.t))
  | Z(unzipped)
and unzipped =
  | ParenZ(t)
  | LamZ_pat(HoleStatus.t, ZPat.t, HExp.t)
  | LamZ_body(HoleStatus.t, HPat.t, unzipped)
  | PlusZ_l(HoleStatus.t, unzipped, HExp.t)
  | PlusZ_r(HoleStatus.t, HExp.t, unzipped)
  | ApZ_fn(HoleStatus.t, unzipped, HExp.t)
  | ApZ_arg(HoleStatus.t, HExp.t, t)
  | OperatorHoleZ_l(unzipped, HExp.t)
  | OperatorHoleZ_r(HExp.t, unzipped);

let rec set_hole_status = (status, ze) =>
  switch (ze) {
  | Y(prefix, suffix) =>
    let n = List.length(prefix);
    let (prefix, suffix) =
      List.rev(prefix)
      @ suffix
      |> HExp.parse
      |> HExp.set_hole_status(status)
      |> HExp.Tile.unparse
      |> ListUtil.split_n(n);
    Y(prefix, suffix);
  | Z(z) => Z(set_hole_status_unzipped(status, z))
  }
and set_hole_status_unzipped = (status, z) =>
  switch (z) {
  | ParenZ(zbody) => ParenZ(set_hole_status(status, zbody))
  | LamZ_pat(_, zp, body) => LamZ_pat(status, zp, body)
  | LamZ_body(_, p, zbody) => LamZ_body(status, p, zbody)
  | PlusZ_l(_, zl, r) => PlusZ_l(status, zl, r)
  | PlusZ_r(_, l, zr) => PlusZ_r(status, l, zr)
  | ApZ_fn(_, zfn, arg) => ApZ_fn(status, zfn, arg)
  | ApZ_arg(_, fn, zarg) => ApZ_arg(status, fn, zarg)
  | OperatorHoleZ_l(_)
  | OperatorHoleZ_r(_) => z
  };

let rec erase: t => HExp.t =
  fun
  | Y(prefix, suffix) => HExp.parse(List.rev(prefix) @ suffix)
  | Z(z) => erase_unzipped(z)
and erase_unzipped: unzipped => HExp.t =
  fun
  | ParenZ(zbody) => Paren(erase(zbody))
  | LamZ_pat(status, zp, body) => Lam(status, ZPat.erase(zp), body)
  | LamZ_body(status, p, zbody) => Lam(status, p, erase_unzipped(zbody))
  | ApZ_fn(status, zfn, arg) => Ap(status, erase_unzipped(zfn), arg)
  | ApZ_arg(status, fn, zarg) => Ap(status, fn, erase(zarg))
  | PlusZ_l(status, zl, r) => Plus(status, erase_unzipped(zl), r)
  | PlusZ_r(status, l, zr) => Plus(status, l, erase_unzipped(zr))
  | OperatorHoleZ_l(zl, r) => OperatorHole(erase_unzipped(zl), r)
  | OperatorHoleZ_r(l, zr) => OperatorHole(l, erase_unzipped(zr));

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
