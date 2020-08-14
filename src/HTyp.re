type t =
  | OperandHole
  | Num
  | Paren(t)
  | OperatorHole(t, t)
  | Arrow(t, t);

let rec consistent = (ty1, ty2) =>
  switch (ty1, ty2) {
  | (OperandHole | OperatorHole(_), _)
  | (_, OperandHole | OperatorHole(_)) => true
  | (Paren(ty1), _) => consistent(ty1, ty2)
  | (_, Paren(ty2)) => consistent(ty1, ty2)
  | (Arrow(ty1, ty2), Arrow(ty1', ty2')) =>
    consistent(ty1, ty1') && consistent(ty2, ty2')
  | _ => ty1 == ty2
  };

let rec matched_arrow =
  fun
  | OperandHole
  | OperatorHole(_) => Some((OperandHole, OperandHole))
  | Arrow(ty1, ty2) => Some((ty1, ty2))
  | Paren(ty) => matched_arrow(ty)
  | _ => None;

module Tile = {
  type term = t;
  type t =
    | OperandHole
    | Num
    | Paren(term)
    | OperatorHole
    | Arrow;

  let mk_operand_hole = () => OperandHole;
  let mk_operator_hole = () => OperatorHole;

  let is_operand_hole = (==)(OperandHole);
  let is_operator_hole = (==)(OperatorHole);

  let shape: t => TileShape.t(term) =
    fun
    | OperandHole => Operand(OperandHole)
    | Num => Operand(Num)
    | Paren(body) => Operand(Paren(body))
    | OperatorHole => BinOp((t1, t2) => OperatorHole(t1, t2), 1, Left)
    | Arrow => BinOp((t1, t2) => Arrow(t1, t2), 2, Right);

  let get_open_children =
    fun
    | OperandHole
    | Num
    | OperatorHole
    | Arrow => []
    | Paren(body) => [body];

  let rec unparse: term => list(t) =
    fun
    | OperandHole => [OperandHole]
    | Num => [Num]
    | Paren(body) => [Paren(body)]
    | OperatorHole(t1, t2) => unparse(t1) @ [OperatorHole, ...unparse(t2)]
    | Arrow(t1, t2) => unparse(t1) @ [Arrow, ...unparse(t2)];
};
include TileUtil.Make(Tile);
