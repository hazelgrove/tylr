module Term = {
  type t =
    | EHole
    | NHole(t)
    | Var(Var.t)
    | Paren(t)
    | OHole(t, t);
};

module Tile = {
  type term = Term.t;
  type t =
    | EHole
    | Var(Var.t)
    | Paren(term)
    | OHole;

  let operand_hole = () => EHole;
  let operator_hole = () => OHole;

  let shape: t => TileShape.t(term) =
    fun
    | EHole => Operand(EHole)
    | Var(x) => Operand(Var(x))
    | Paren(body) => Operand(Paren(body))
    | OHole => BinOp((p1, p2) => OHole(p1, p2), 1, Left);
};

let parse = TileParser.parse((module Tile));
let rec unparse: Term.t => list(Tile.t) =
  fun
  | EHole => [EHole]
  | NHole(p) =>
    // TODO add err status to tiles
    unparse(p)
  | Var(x) => [Var(x)]
  | Paren(body) => [Paren(body)]
  | OHole(p1, p2) => unparse(p1) @ [OHole, ...unparse(p2)];
