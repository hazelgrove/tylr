type t = (Skel.t, tiles)
and tiles = list(tile)
and tile =
  | Hole
  | Var(Var.t)
  | Paren(t)
  | OpHole;

module Tile = {
  type t = tile;

  let operand_hole = Hole;
  let operator_hole = OpHole;

  let shape: t => Tile.shape =
    fun
    | Hole
    | Var(_)
    | Paren(_) => Operand
    | OpHole => BinOp;

  let precedence: t => int =
    fun
    | Hole
    | Var(_)
    | Paren(_) => 0
    | OpHole => 1;

  let associativity: t => option(Associativity.t) =
    fun
    | Hole
    | Var(_)
    | Paren(_) => None
    | OpHole => Some(Left);
};

let mk = (tiles): t => (Skel.mk((module Tile), tiles), tiles);
