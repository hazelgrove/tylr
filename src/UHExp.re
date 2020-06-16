type t = (Skel.t, tiles)
and tiles = Tiles.t(operand, preop, postop, binop)
and tile = Tile.t(operand, preop, postop, binop)
and operand =
  | Hole
  | Num(int)
  | Var(Var.t)
  | Paren(t)
and preop =
  | If(t, t)
  | Let(Var.t, t)
and postop =
  | Ann(HTyp.t) /* 1 + [2 : Int] -> Int */
and binop =
  | OpHole
  | Plus
  | Times
  | Eq;

let precedence: tile => int =
  fun
  | Operand(_) => 0
  | BinOp(OpHole) => 1
  | BinOp(Times) => 2
  | BinOp(Plus) => 3
  | BinOp(Eq) => 5
  | PostOp(Ann(_)) => 9
  | PreOp(If(_)) => 10
  | PreOp(Let(_)) => 10;

let mk = (tiles): t => (
  Skel.mk(~precedence, ~operand_hole=Hole, ~operator_hole=OpHole, tiles),
  tiles,
);
