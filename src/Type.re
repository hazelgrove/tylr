type t =
  | Hole
  | Num
  | Arrow(t, t);

let rec consistent = (ty, ty') =>
  switch (ty, ty') {
  | (Hole, _)
  | (_, Hole) => true
  | (Arrow(ty1, ty2), Arrow(ty1', ty2')) =>
    consistent(ty1, ty1') && consistent(ty2, ty2')
  | _ => ty == ty'
  };

let matched_arrow =
  fun
  | Hole => Some((Hole, Hole))
  | Num => None
  | Arrow(ty1, ty2) => Some((ty1, ty2));
