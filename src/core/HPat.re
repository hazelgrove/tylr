open Sexplib.Std;
open Util;

module Tile = {
  [@deriving sexp]
  type s = list(t)
  [@deriving sexp]
  and t = Tile.t(operand, preop, postop, binop)
  [@deriving sexp]
  and operand =
    | OperandHole
    | Var(Var.t)
    | Paren(s)
  [@deriving sexp]
  and preop = unit // empty
  [@deriving sexp]
  and postop =
    | Ann(HoleStatus.t, HTyp.t)
  [@deriving sexp]
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

[@deriving sexp]
type t = Tile.s;

type inner_tiles =
  | Pat(Tile.s)
  | Other(HTyp.inner_tiles);

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
  | Var(x) => Var(x)
  | Paren(body) => Paren(put_hole_status(status, body))
and put_hole_status_preop = _ =>
  fun
  | _ => raise(Tile.Void_PreOp)
and put_hole_status_postop = status =>
  fun
  | Ann(_, ann) => Ann(status, ann)
and put_hole_status_binop = _ =>
  fun
  | OperatorHole => OperatorHole;
