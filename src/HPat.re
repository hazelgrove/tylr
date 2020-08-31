type t =
  | OperandHole
  | Var(Var.t)
  | Paren(t)
  | Ann(HoleStatus.t, t, HTyp.t)
  | OperatorHole(t, t);

let rec set_hole_status = (status, p) =>
  switch (p) {
  | OperandHole
  | OperatorHole(_)
  | Var(_) => p
  | Paren(body) => Paren(set_hole_status(status, body))
  | Ann(_, subj, ann) => Ann(status, subj, ann)
  };

module Tile = {
  type term = t;
  type t =
    | OperandHole
    | Var(Var.t)
    | Paren(term)
    | Ann(HoleStatus.t, HTyp.t)
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
    | Ann(status, ann) => PostOp(subj => Ann(status, subj, ann), 2)
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
    | Var(x) => [Var(x)]
    | Paren(body) => [Paren(body)]
    | Ann(status, subj, ann) => unparse(subj) @ [Ann(status, ann)]
    | OperatorHole(p1, p2) => unparse(p1) @ [OperatorHole, ...unparse(p2)];

  let root: term => (int, t) =
    fun
    | OperandHole => (0, OperandHole)
    | Var(x) => (0, Var(x))
    | Paren(body) => (0, Paren(body))
    | Ann(status, subj, ann) => (
        List.length(unparse(subj)),
        Ann(status, ann),
      )
    | OperatorHole(l, _) => (List.length(unparse(l)), OperatorHole);

  let set_hole_status = (status, tile) =>
    switch (tile) {
    | OperandHole
    | OperatorHole
    | Var(_) => tile
    | Paren(body) => Paren(set_hole_status(status, body))
    | Ann(_, ann) => Ann(status, ann)
    };
};
include TileUtil.Make(Tile);
let unparse = Tile.unparse;
