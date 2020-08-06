type t = (Skel.t, tiles)
and tiles = list(tile)
and tile =
  | Hole
  | Num
  | Bool
  | Paren(t)
  | OpHole;

module Tile = {
  type t = tile;

  let operand_hole = Hole;
  let operator_hole = OpHole;

  let shape: t => Tile.shape =
    fun
    | Hole
    | Num
    | Bool
    | Paren(_) => Operand
    | OpHole => BinOp;

  let precedence: t => int =
    fun
    | Hole
    | Num
    | Bool
    | Paren(_) => 0
    | OpHole => 1;

  let associativity: t => option(Associativity.t) =
    fun
    | Hole
    | Num
    | Bool
    | Paren(_) => None
    | OpHole => Some(Left);
};

let mk = (tiles): t => (Skel.mk((module Tile), tiles), tiles);

let unroll = (ty: HTyp.t): t =>
  switch (ty) {
  | Hole =>
  }