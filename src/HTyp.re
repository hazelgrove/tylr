module Term = {
  type t =
    | Hole
    | Num
    | Bool
    | Paren(t)
    | OpHole(t, t);
};

module Tile: {
  type s = list(t)
  and t =
    | EHole
    | Num
    | Bool
    | Paren(s)
    | OHole;

  let operand_hole: t;
  let operator_hole: t;

  let shape: t => TileShape.t(term);

  let roll: s => term;
  let unroll: term => s;
} = {
  type s = list(t)
  and t =
    | EHole
    | Num
    | Bool
    | Paren(s)
    | OHole;

  let operand_hole = EHole;
  let operator_hole = OHole;

  let roll = TileParser.parse((module Tile));
  let unroll =
    fun
    | EHole => [EHole]
    | Num => [Num]
    | Bool => [Bool]
    | Paren(body) => [Paren(unroll(body))]
    | OHole(t1, t2) => unroll(t1) @ [OHole, ...unroll(t2)];

  let shape: t => Tile.shape =
    fun
    | EHole => Operand(EHole)
    | Num => Operand(Num)
    | Bool => Operand(Bool)
    | Paren(body) => Operand(Paren(roll(body)))
    | OHole => BinOp((t1, t2) => OHole(t1, t2), 1, Left);
};
