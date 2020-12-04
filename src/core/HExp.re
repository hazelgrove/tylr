open Sexplib.Std;
open Util;

module Tile = {
  let sort = Sort.Exp;

  [@deriving sexp]
  type s = list(t)
  [@deriving sexp]
  and t = Tile.t(operand, preop, postop, binop)
  [@deriving sexp]
  and operand =
    | OperandHole
    | Num(HoleStatus.t, int)
    | Var(HoleStatus.t, Var.t)
    | Paren(s)
  [@deriving sexp]
  and preop =
    | Lam(HoleStatus.t, HPat.t)
  [@deriving sexp]
  and postop =
    | Ap(HoleStatus.t, s)
  [@deriving sexp]
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

[@deriving sexp]
type t = Tile.s;
include Tiles.Make(Tile);

module Inner = {
  type t =
    | Exp(Tile.s)
    | Other(HPat.Inner.t);

  let wrap = (ts: Tile.s) => Exp(ts);
  let unwrap =
    fun
    | Other(_) => None
    | Exp(ts) => Some(ts);
};

// does not recurse into parentheses
let get_hole_status_operand: Tile.operand => _ =
  fun
  | OperandHole
  | Paren(_) => HoleStatus.NotInHole
  | Num(status, _)
  | Var(status, _) => status;
let get_hole_status_preop: Tile.preop => _ =
  fun
  | Lam(status, _) => status;
let get_hole_status_postop: Tile.postop => _ =
  fun
  | Ap(status, _) => status;
let get_hole_status_binop: Tile.binop => _ =
  fun
  | OperatorHole => HoleStatus.NotInHole
  | Plus(status) => status;
let get_hole_status =
  get_root(
    ~operand=get_hole_status_operand,
    ~preop=get_hole_status_preop,
    ~postop=get_hole_status_postop,
    ~binop=get_hole_status_binop,
  );

// recurses into parentheses
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
