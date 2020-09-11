module Tile = {
  type s = list(t)
  and t = Tile.t(operand, preop, postop, binop)
  and operand =
    | OperandHole
    | Var(Var.t)
    | Paren(s)
  and preop = unit // empty
  and postop =
    | Ann(HoleStatus.t, HTyp.t)
  and binop =
    | OperatorHole;

  exception Void_PreOp;

  let mk_operand_hole = (): t => Operand(OperandHole);
  let mk_operator_hole = (): t => BinOp(OperatorHole);

  let is_operand_hole: t => bool = (==)(Tile.Operand(OperandHole));
  let is_operator_hole: t => bool = (==)(Tile.BinOp(OperatorHole));

  let precedence: t => int =
    fun
    | Operand(_) => 0
    | PreOp(_) => raise(Void_PreOp)
    | PostOp(Ann(_)) => 2
    | BinOp(OperatorHole) => 1;

  let associativity =
    [(1, Associativity.Left)] |> List.to_seq |> IntMap.of_seq;

  let get_open_children: t => list(s) =
    fun
    | Operand(OperandHole | Var(_)) => []
    | Operand(Paren(body)) => [body]
    | PreOp(_) => raise(Void_PreOp)
    | PostOp(Ann(_)) => []
    | BinOp(OperatorHole) => [];
};
include Tiles.Make(Tile);

type t = Tile.s;

let rec set_hole_status = (status: HoleStatus.t): (t => t) =>
  map_root(
    ~operand=set_hole_status_operand(status),
    ~preop=set_hole_status_preop(status),
    ~postop=set_hole_status_postop(status),
    ~binop=set_hole_status_binop(status),
  )
and set_hole_status_operand = status =>
  fun
  | OperandHole => OperandHole
  | Var(x) => Var(x)
  | Paren(body) => Paren(set_hole_status(status, body))
and set_hole_status_preop = _ =>
  fun
  | _ => raise(Tile.Void_PreOp)
and set_hole_status_postop = status =>
  fun
  | Ann(_, ann) => Ann(status, ann)
and set_hole_status_binop = _ =>
  fun
  | OperatorHole => OperatorHole;
