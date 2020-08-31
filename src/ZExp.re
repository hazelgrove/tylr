type t = ZTiles.t(ztile, HExp.Tile.t)
and ztile =
  | ParenZ_body(t)
  | LamZ_pat(HoleStatus.t, ZPat.t)
  | ApZ_arg(HoleStatus.t, t);

let rec erase = (ze: t) =>
  switch (ze.z) {
  | None => HExp.parse(ze.prefix @ ze.suffix)
  | Some(ztile) =>
    HExp.parse(ze.prefix @ [erase_ztile(ztile), ...ze.suffix])
  }
and erase_ztile: ztile => HExp.Tile.t =
  fun
  | ParenZ_body(zbody) => Paren(erase(zbody))
  | LamZ_pat(status, zp) => Lam(status, ZPat.erase(zp))
  | ApZ_arg(status, zarg) => Ap(status, erase(zarg));

let rec set_hole_status = (status, ze) => {
  let n = HExp.Tile.index_of_root(erase(ze));
  let m = List.length(ze.prefix);
  let set_tile = HExp.Tile.set_hole_status(status);
  if (n < m) {
    let prefix = ze.prefix |> ListUtil.map_nth(n, set_tile);
    {...ze, prefix};
  } else {
    switch (ze.z) {
    | None =>
      let suffix = ze.suffix |> ListUtil.map_nth(n - m, set_tile);
      {...ze, suffix};
    | Some(ztile) =>
      if (n == m) {
        let z = Some(set_hole_status_ztile(status, ztile));
        {...ze, z};
      } else {
        let suffix = ze.suffix |> ListUtil.map_nth(n - (m + 1), set_tile);
        {...ze, suffix};
      }
    };
  };
}
and set_hole_status_ztile = (status, ztile) =>
  switch (ztile) {
  | ParenZ_body(ze) => ParenZ_body(set_hole_status(status, ze))
  | LamZ_pat(_, zp) => LamZ_pat(status, zp)
  | ApZ_arg(_, arg) => ApZ_arg(status, arg)
  };

let place_before = (e: HExp.t): t => ZTiles.place_before(HExp.unparse(e));
let place_after = (e: HExp.t): t => ZTiles.place_after(HExp.unparse(e));

let enter_from_left: HExp.Tile.t => option(ztile) =
  fun
  | OperandHole
  | Num(_)
  | Var(_)
  | Plus(_)
  | OperatorHole => None
  | Paren(body) => Some(ParenZ_body(place_before(body)))
  | Lam(status, p) => Some(LamZ_pat(status, ZPat.place_before(p)))
  | Ap(status, arg) => Some(ApZ_arg(status, place_before(arg)));
let enter_from_right: HExp.Tile.t => option(ztile) =
  fun
  | OperandHole
  | Num(_)
  | Var(_)
  | Plus(_)
  | OperatorHole => None
  | Paren(body) => Some(ParenZ_body(place_after(body)))
  | Lam(status, p) => Some(LamZ_pat(status, ZPat.place_after(p)))
  | Ap(status, arg) => Some(ApZ_arg(status, place_after(arg)));

let move_left = ZTiles.move_left(~enter_from_right);
let move_right = ZTiles.move_right(~enter_from_left);

/*
 // assume anchor before focus
 let restructure = (
   anchor: t,
   focus: t,
   target: t,
 ): option(ZExp.t) => {
   switch (anchor, focus, target) {
   | (
       Y(anchor_prefix, anchor_suffix),
       Y(focus_prefix, focus_suffix),
       Y(target_prefix, target_suffix),
     ) =>
     let anchor_n = List.length(anchor_prefix);
     let focus_n = List.length(focus_prefix);
     let target_n = List.length(target_prefix);
     if (anchor_n < target_n && target_n < focus_n) {
       None
     } else {
       // TODO fix prefix suffix order
       let tiles = List.rev(target_prefix) @ target_suffix;
       let (prefix, selected, suffix) =
         ListUtil.split_sublist(anchor_n, focus_n, tiles);
       if (target_n <= anchor_n) {
         let (p1, p2) = ListUtil.split_n(target_n, prefix);
         Some(Y(p1, selected @ p2 @ suffix));
       } else {
         // target_n >= focus_n
         let (s1, s2) = ListUtil.split_n(target_n - focus_n, suffix);
         Some(Y(prefix @ s1, selected @ s2));
       };
     };
   | (
       Y(anchor_prefix, anchor_suffix),
       Y(focus_prefix, focus_suffix),
       Z(target_z),
     ) =>

   | (
       Y(anchor_prefix, anchor_suffix),
       Z(focus_z),
       Y(target_prefix, target_suffix),
     ) =>
     let anchor_n = List.length(anchor_prefix);
     let target_n = List.length(target_prefix);
     if (target_n > anchor_n) {
       None
     } else {
       // target_n <= anchor_n
       let ()
     }
     if (List.length(target_pre) < List.length(anchor_pre)) {

     }
   }
 };
 */

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
