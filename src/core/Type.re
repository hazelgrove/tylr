[@deriving sexp]
type t =
  | Hole
  | Num
  | Bool
  | Arrow(t, t)
  | Prod(t, t);

let rec consistent = (ty, ty') =>
  switch (ty, ty') {
  | (Hole, _)
  | (_, Hole) => true
  | (Arrow(ty1, ty2), Arrow(ty1', ty2')) =>
    consistent(ty1, ty1') && consistent(ty2, ty2')
  | (Prod(ty1, ty2), Prod(ty1', ty2')) =>
    consistent(ty1, ty1') && consistent(ty2, ty2')
  | _ => ty == ty'
  };

let matches_arrow =
  fun
  | Hole => Some((Hole, Hole))
  | Num
  | Bool
  | Prod(_) => None
  | Arrow(ty1, ty2) => Some((ty1, ty2));

let matches_prod =
  fun
  | Hole => Some((Hole, Hole))
  | Num
  | Bool
  | Arrow(_) => None
  | Prod(ty1, ty2) => Some((ty1, ty2));

open Util.OptUtil.Syntax;
let rec join = (ty, ty') =>
  switch (ty, ty') {
  | (Hole, _) => Some(ty')
  | (_, Hole) => Some(ty)
  | (Arrow(ty1, ty2), Arrow(ty1', ty2')) =>
    let+ ty1 = join(ty1, ty1')
    and+ ty2 = join(ty2, ty2');
    Arrow(ty1, ty2);
  | _ => ty == ty' ? Some(ty) : None
  };

// TODO unify with Tile_typ
let precedence =
  fun
  | Hole
  | Num
  | Bool => 0
  | Arrow(_) => 2
  | Prod(_) => 3;
