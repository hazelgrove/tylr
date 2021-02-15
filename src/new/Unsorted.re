module Tile = {
  type s = list(t)
  and t = Tile.t(op, pre, post, bin)
  and op =
    | OpHole
    | Text(string)
    | Paren(s)
  and pre =
    | Lam(s)
    | Let(s, s)
  and post =
    | Ap(s)
    | Ann(s)
  and bin =
    | BinHole
    | Plus
    | Arrow;
};

module Tessera = {
  type t =
    | OpHole
    | BinHole
    | Text(string)
    | Lam(Tile.s)
    | Ann(Tile.s)
    | Plus
    | Arrow
    | Paren_l
    | Paren_r
    | Let_eq(Tile.s)
    | Let_in;

  module Shape = {
    type t =
      | Text(string)
      | Paren_l
      | Paren_r
      | Lam
      | Let_eq
      | Let_in
      | Ann
      | Plus
      | Arrow;
  };

  let is_closing =
    fun
    | OpHole
    | BinHole
    | Text(_)
    | Lam(_)
    | Ann(_)
    | Plus
    | Arrow
    | Paren_l
    | Let_eq(_) => false
    | Paren_r
    | Let_in => true;

  let is_convex = (_, _) => failwith("todo");
};
