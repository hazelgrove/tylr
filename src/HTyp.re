module Term = {
  type t =
    | EHole
    | Num
    | Bool
    | Paren(t)
    | OHole(t, t);
};

module Tile = {
  type term = Term.t;
  type t =
    | EHole
    | Num
    | Bool
    | Paren(term)
    | OHole;

  let mk_operand_hole = () => EHole;
  let mk_operator_hole = () => OHole;

  let is_operand_hole = (==)(EHole);
  let is_operator_hole = (==)(OHole);

  let shape: t => TileShape.t(term) =
    fun
    | EHole => Operand(EHole)
    | Num => Operand(Num)
    | Bool => Operand(Bool)
    | Paren(body) => Operand(Paren(body))
    | OHole => BinOp((t1, t2) => OHole(t1, t2), 1, Left);
};

let fix_empty_holes = TileParser.fix_empty_holes((module Tile));
let parse = TileParser.parse((module Tile));
let rec unparse: Term.t => list(Tile.t) =
  fun
  | EHole => [EHole]
  | Num => [Num]
  | Bool => [Bool]
  | Paren(body) => [Paren(body)]
  | OHole(t1, t2) => unparse(t1) @ [OHole, ...unparse(t2)];
