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

  let mk_operand_hole = () => EHole;
  let mk_operator_hole = () => OHole;

  let is_operand_hole = (==)(EHole);
  let is_operator_hole = (==)(OHole);

  let shape: t => TileShape.t(term) =
    fun
    | EHole => Operand(EHole)
    | Var(x) => Operand(Var(x))
    | Paren(body) => Operand(Paren(body))
    | OHole => BinOp((p1, p2) => OHole(p1, p2), 1, Left);
};

let fix_empty_holes = TileParser.fix_empty_holes((module Tile));
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
