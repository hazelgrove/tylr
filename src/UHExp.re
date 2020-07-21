type t = (Skel.t, tiles)
and tiles = list(tile)
and tile =
  | Hole
  | Num(int)
  | Var(Var.t)
  | Paren(t)
  | If(t, t)
  | Let(UHPat.t, t)
  | Ann(UHTyp.t) /* 1 + [2 : Int] -> Int */
  | OpHole
  | Plus
  | Times
  | Eq;

module Tile = {
  type t = tile;

  let operand_hole = Hole;
  let operator_hole = OpHole;

  let shape: t => Tile.shape =
    fun
    | Hole
    | Num(_)
    | Var(_)
    | Paren(_) => Operand
    | If(_)
    | Let(_) => PreOp
    | Ann(_) => PostOp
    | OpHole
    | Plus
    | Times
    | Eq => BinOp;

  let precedence: t => int =
    fun
    | Hole
    | Num(_)
    | Var(_)
    | Paren(_) => 0
    | OpHole => 1
    | Times => 2
    | Plus => 3
    | Eq => 5
    | Ann(_) => 9
    | If(_) => 10
    | Let(_) => 10;

  let associativity: t => option(Associativity.t) =
    fun
    | Hole
    | Num(_)
    | Var(_)
    | Paren(_)
    | Ann(_)
    | If(_)
    | Let(_) => None
    | OpHole
    | Times
    | Plus
    | Eq => Some(Left);
};

let mk = (tiles): t => (Skel.mk((module Tile), tiles), tiles);

let one_two_three_four = (
  Skel.(
    BinOp(
      BinOp(BinOp(Operand(0), 1, Operand(2)), 3, Operand(4)),
      5,
      Operand(6),
    )
  ),
  [Num(1), Plus, Num(2), Plus, Num(3), Plus, Num(4)],
);
