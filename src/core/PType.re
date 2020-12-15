type t =
  | Unspecified
  | Hole
  | Num
  | Bool
  | Arrow(t, t);

let rec of_type = (ty: Type.t): t =>
  switch (ty) {
  | Hole => Hole
  | Num => Num
  | Bool => Bool
  | Arrow(ty1, ty2) => Arrow(of_type(ty1), of_type(ty2))
  };

let rec to_type =
  fun
  | Unspecified
  | Hole => Type.Hole
  | Num => Num
  | Bool => Bool
  | Arrow(pty1, pty2) => Arrow(to_type(pty1), to_type(pty2));

open Util.OptUtil.Syntax;
let rec join = (pty: t, ty: Type.t): option(Type.t) =>
  switch (pty, ty) {
  | (Unspecified, _) => Some(ty)
  | (Hole, _) => Some(Hole)
  | (_, Hole) => Some(to_type(pty))
  | (Arrow(pty1, pty2), Arrow(ty1, ty2)) =>
    let+ ty1 = join(pty1, ty1)
    and+ ty2 = join(pty2, ty2);
    Type.Arrow(ty1, ty2);
  | _ => to_type(pty) == ty ? Some(ty) : None
  };

let join_or_to_type = (pty: t, ty: Type.t): Type.t =>
  switch (join(pty, ty)) {
  | Some(joined) => joined
  | None => to_type(pty)
  };
