module Tile = {
  type op =
    | OpHole
    | Num
    | Bool
    | Paren(s);
  type pre = unit; // empty
  type post = unit; // empty
  type bin =
    | BinHole
    | Arrow;
  type t = Tile.t(op, pre, post, bin);

  let mk: HTile.t => option(t) =
    Tile.get(
      fun
      | HTile.OpHole => Some(OpHole)
      | Text(s) =>
        if (s == "num") {
          Some(Num);
        } else if (s == "bool") {
          Some(Bool);
        } else {
          None;
        }
      | Paren(body) => Some(Paren(body)),
      fun
      | HTile.Lam(_)
      | Let(_) => None,
      fun
      | HTile.Ap(_)
      | Ann(_) => None,
      fun
      | HTile.BinHole => Some(BinHole)
      | Arrow => Some(Arrow)
      | Plus => None,
    );
};

type t =
  | OpHole
  | Num
  | Bool
  | Paren(t)
  | BinHole(t, t)
  | Arrow(t, t);
