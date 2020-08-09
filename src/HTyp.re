module Term = {
  type t =
    | EHole
    | Num
    | Bool
    | Paren(t)
    | OHole(t, t);
};

module rec Tile: {
  type s = list(t)
  and t =
    | EHole
    | Num
    | Bool
    | Paren(s)
    | OHole;

  type term = Term.t;

  let operand_hole: unit => t;
  let operator_hole: unit => t;

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

  type term = Term.t;

  let operand_hole = () => EHole;
  let operator_hole = () => OHole;

  let roll = TileParser.parse((module Tile));
  let rec unroll: term => s =
    fun
    | EHole => [EHole]
    | Num => [Num]
    | Bool => [Bool]
    | Paren(body) => [Paren(unroll(body))]
    | OHole(t1, t2) => unroll(t1) @ [OHole, ...unroll(t2)];

  let shape: t => TileShape.t(term) =
    fun
    | EHole => Operand(EHole)
    | Num => Operand(Num)
    | Bool => Operand(Bool)
    | Paren(body) => Operand(Paren(roll(body)))
    | OHole => BinOp((t1, t2) => OHole(t1, t2), 1, Left);
};
