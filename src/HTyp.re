type t =
  | Hole
  | Num
  | Bool;

let consistent = (ty1, ty2) =>
  switch (ty1, ty2) {
  | (Hole, _)
  | (_, Hole) => true
  | _ => ty1 == ty2
  };
