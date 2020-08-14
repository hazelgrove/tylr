type t =
  | OperandHole
  | NonemptyHole(t)
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

  let get_open_children =
    fun
    | OperandHole
    | Var(_)
    | Ann(_)
    | OperatorHole => []
    | Paren(body) => [body];

  let rec unparse: term => list(t) =
    fun
    | OperandHole => [OperandHole]
    | NonemptyHole(p) =>
      // TODO add err status to tiles
      unparse(p)
    | Var(x) => [Var(x)]
    | Paren(body) => [Paren(body)]
    | Ann(subj, ann) => unparse(subj) @ [Ann(ann)]
    | OperatorHole(p1, p2) => unparse(p1) @ [OperatorHole, ...unparse(p2)];
};
include TileUtil.Make(Tile);
