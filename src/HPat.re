type t =
  | OperandHole
  | Var(Var.t)
  | Paren(t)
  | Ann(t, HTyp.t)
  | OperatorHole(t, t);

module Tile = {
  type term = t;
  type t =
    | OperandHole
    | Var(Var.t)
    | Paren(term)
    | Ann(HTyp.t)
    | OperatorHole;

  let mk_operand_hole = () => OperandHole;
  let mk_operator_hole = () => OperatorHole;

  let is_operand_hole = (==)(OperandHole);
  let is_operator_hole = (==)(OperatorHole);

  let shape: t => TileShape.t(term) =
    fun
    | OperandHole => Operand(OperandHole)
    | Var(x) => Operand(Var(x))
    | Paren(body) => Operand(Paren(body))
    | Ann(ann) => PostOp(subj => Ann(subj, ann), 2)
    | OperatorHole => BinOp((p1, p2) => OperatorHole(p1, p2), 1, Left);
};

let fix_empty_holes = TileParser.fix_empty_holes((module Tile));
let parse = TileParser.parse((module Tile));
let rec unparse: t => list(Tile.t) =
  fun
  | OperandHole => [OperandHole]
  | TypeErr(p) =>
    // TODO add err status to tiles
    unparse(p)
  | Var(x) => [Var(x)]
  | Paren(body) => [Paren(body)]
  | Ann(subj, ann) => unparse(subject) @ [Ann(ann)]
  | OperatorHole(p1, p2) => unparse(p1) @ [OperatorHole, ...unparse(p2)];
