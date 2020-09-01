type t = HTerm.t(operand, preop, postop, binop)
and operand =
  | OperandHole
  | Num(HoleStatus.t, int)
  | Var(HoleStatus.t, Var.t)
  | Paren(t)
and preop =
  | Lam(HoleStatus.t, HPat.t)
and postop =
  | Ap(HoleStatus.t, t)
and binop =
  | Plus(HoleStatus.t)
  | OperatorHole;

let rec set_hole_status = (status: HoleStatus.t, e: t): t =>
  HTerm.set_hole_status(~set_tile, status, e)
and set_tile = (status, tile) =>
  tile
  |> Tile.map(
       ~operand=
         fun
         | OperandHole => OperandHole
         | Num(_, n) => Num(status, n)
         | Var(_, x) => Var(status, x)
         | Paren(body) => Paren(set_hole_status(status, body)),
       ~preop=
         fun
         | Lam(_, p) => Lam(status, p),
       ~postop=
         fun
         | Ap(_, arg) => Ap(status, arg),
       ~binop=
         fun
         | Plus(_) => Plus(status)
         | OperatorHole => OperatorHole,
     );

module Tile = {
  type nonrec operand = operand;
  type nonrec preop = preop;
  type nonrec postop = postop;
  type nonrec binop = binop;
  type t = Tile.t(operand, preop, postop, binop);

  let mk_operand_hole = (): t => Operand(OperandHole);
  let mk_operator_hole = (): t => BinOp(OperatorHole);

  let is_operand_hole = (==)(Tile.Operand(OperandHole));
  let is_operator_hole = (==)(Tile.BinOp(OperatorHole));

  let precedence: t => int =
    fun
    | Operand(_) => 0
    | PreOp(Lam(_)) => 10
    | PostOp(Ap(_)) => 1
    | BinOp(Plus(_)) => 3
    | BinOp(OperatorHole) => 2;

  let associativity =
    [(2, Associativity.Left), (3, Left)] |> List.to_seq |> IntMap.of_seq;

  let get_open_children: t => list(list(t)) =
    fun
    | Operand(OperandHole | Num(_) | Var(_)) => []
    | Operand(Paren((_, body_tiles))) => [body_tiles]
    | PreOp(Lam(_)) => []
    | PostOp(Ap(_, (_, arg_tiles))) => [arg_tiles]
    | BinOp(OperatorHole | Plus(_)) => [];
};
include TileUtil.Make(Tile);
