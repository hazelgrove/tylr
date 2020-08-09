module Term = {
  type t =
    | EHole
    | NHole
    | Var(Var.t)
    | Paren(t)
    | OHole(t, t);
};

module rec Tile: {
  type s = list(t)
  and t =
    | EHole
    | Var(Var.t)
    | Paren(t)
    | OHole;

  type term = Term.t;

  let operand_hole: t;
  let operator_hole: t;

  let shape: t => TileShape.t(term);

  let roll: s => term;
  let unroll: term => s;
} = {
  type s = list(t)
  and t =
    | EHole
    | Var(Var.t)
    | Paren(t)
    | OHole;

  type term = Term.t;

  let operand_hole = EHole;
  let operator_hole = OHole;

  let roll = TileParser.parse((module Tile));
  let rec unroll =
    fun
    | EHole => [EHole]
    | NHole(p) =>
      // TODO add err status to tiles
      unroll(p)
    | Var(x) => [Var(x)]
    | Paren(body) => [Paren(unroll(body))]
    | OHole(p1, p2) => unroll(p1) @ [OHole, ...unroll(p2)];

  let shape =
    fun
    | EHole => Operand(EHole)
    | Var(x) => Operand(Var(x))
    | Paren(body) => Operand(Paren(roll(body)))
    | OHole => BinOp((p1, p2) => OHole(p1, p2), 1, Left);
};
