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

let join = (ty1, ty2) =>
  switch (ty1, ty2) {
  | (Hole, _)
  | (_, Hole) => Some(Hole)
  | (Num, Num) => Some(Num)
  | (Bool, Bool) => Some(Bool)
  | _ => None
  };
