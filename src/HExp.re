type t =
  | OperandHole
  | NonemptyHole(t)
  | Num(int)
  | Var(Var.t)
  | Paren(t)
  | Lam(HPat.t, t)
  | Plus(t, t)
  | Ap(t, t)
  | OperatorHole(t, t);

module Tile = {
  type term = t;
  type t =
    | OperandHole
    | Num(int)
    | Var(Var.t)
    | Paren(term)
    | Lam(HPat.t)
    | Ap(term)
    | OperatorHole
    | Plus;

  let mk_operand_hole = () => OperandHole;
  let mk_operator_hole = () => OperatorHole;

  let is_operand_hole = (==)(OperandHole);
  let is_operator_hole = (==)(OperatorHole);

  let shape: t => TileShape.t(term) =
    fun
    | OperandHole => Operand(OperandHole)
    | Num(n) => Operand(Num(n))
    | Var(x) => Operand(Var(x))
    | Paren(body) => Operand(Paren(body))
    | Lam(p) => PreOp(body => Lam(p, body), 10)
    | Ap(arg) => PostOp(fn => Ap(fn, arg), 1)
    | Plus => BinOp((e1, e2) => Plus(e1, e2), 3, Left)
    | OperatorHole => BinOp((e1, e2) => OperatorHole(e1, e2), 2, Left);

  let get_open_children =
    fun
    | OperandHole
    | Num(_)
    | Var(_)
    | Lam(_)
    | Plus
    | OperatorHole => []
    | Paren(body) => [body]
    | Ap(arg) => [arg];
};

let fix_empty_holes = TileParser.fix_empty_holes((module Tile));
let parse = TileParser.parse((module Tile));
let rec unparse: t => list(Tile.t) =
  fun
  | OperandHole => [OperandHole]
  | NonemptyHole(e) =>
    // TODO add err status to tiles
    unparse(e)
  | Num(n) => [Num(n)]
  | Var(x) => [Var(x)]
  | Paren(body) => [Paren(body)]
  | Lam(p, body) => [Lam(p), ...unparse(body)]
  | Ap(fn, arg) => unparse(fn) @ [Ap(arg)]
  | Plus(e1, e2) => unparse(e1) @ [Plus, ...unparse(e2)]
  | OperatorHole(e1, e2) => unparse(e1) @ [OperatorHole, ...unparse(e2)];
