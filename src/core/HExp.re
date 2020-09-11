module Tile = {
  type s = list(t)
  and t = Tile.t(operand, preop, postop, binop)
  and operand =
    | OperandHole
    | Num(HoleStatus.t, int)
    | Var(HoleStatus.t, Var.t)
    | Paren(s)
  and preop =
    | Lam(HoleStatus.t, HPat.t)
  and postop =
    | Ap(HoleStatus.t, s)
  and binop =
    | Plus(HoleStatus.t)
    | OperatorHole;

  let mk_operand_hole = (): t => Operand(OperandHole);
  let mk_operator_hole = (): t => BinOp(OperatorHole);

  let is_operand_hole: t => bool = (==)(Tile.Operand(OperandHole));
  let is_operator_hole: t => bool = (==)(Tile.BinOp(OperatorHole));

  let precedence: t => int =
    fun
    | Operand(_) => 0
    | PreOp(Lam(_)) => 10
    | PostOp(Ap(_)) => 1
    | BinOp(Plus(_)) => 3
    | BinOp(OperatorHole) => 2;

  let associativity =
    [(2, Associativity.Left), (3, Left)] |> List.to_seq |> IntMap.of_seq;

  let get_open_children: t => list(s) =
    fun
    | Operand(OperandHole | Num(_) | Var(_)) => []
    | Operand(Paren(body)) => [body]
    | PreOp(Lam(_)) => []
    | PostOp(Ap(_, arg)) => [arg]
    | BinOp(OperatorHole | Plus(_)) => [];
};
include Tiles.Make(Tile);

type t = Tile.s;

let rec put_hole_status = (status: HoleStatus.t): (t => t) =>
  update_root(
    ~operand=put_hole_status_operand(status),
    ~preop=put_hole_status_preop(status),
    ~postop=put_hole_status_postop(status),
    ~binop=put_hole_status_binop(status),
  )
and put_hole_status_operand = status =>
  fun
  | OperandHole => OperandHole
  | Num(_, n) => Num(status, n)
  | Var(_, x) => Var(status, x)
  | Paren(body) => Paren(put_hole_status(status, body))
and put_hole_status_preop = status =>
  fun
  | Lam(_, p) => Lam(status, p)
and put_hole_status_postop = status =>
  fun
  | Ap(_, arg) => Ap(status, arg)
and put_hole_status_binop = status =>
  fun
  | Plus(_) => Plus(status)
  | OperatorHole => OperatorHole;
