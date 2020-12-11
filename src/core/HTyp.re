open Sexplib.Std;
open Util;

module T = {
  let sort = Sort.Typ;

  [@deriving sexp]
  type s = list(t)
  [@deriving sexp]
  and t = Tile.t(operand, preop, postop, binop)
  [@deriving sexp]
  and operand =
    | OperandHole
    | Num
    | Paren(s)
  [@deriving sexp]
  and preop = unit // empty
  [@deriving sexp]
  and postop = unit // empty
  [@deriving sexp]
  and binop =
    | OperatorHole
    | Arrow;

  exception Void_PreOp;
  exception Void_PostOp;

  let mk_operand_hole = (): t => Operand(OperandHole);
  let mk_operator_hole = (): t => BinOp(OperatorHole);

  let is_operand_hole: t => bool = (==)(Tile.Operand(OperandHole));
  let is_operator_hole: t => bool = (==)(Tile.BinOp(OperatorHole));

  let precedence: t => int =
    fun
    | Operand(_) => 0
    | PreOp () => raise(Void_PreOp)
    | PostOp () => raise(Void_PostOp)
    | BinOp(OperatorHole) => 1
    | BinOp(Arrow) => 2;

  let associativity =
    [(1, Associativity.Left), (2, Right)] |> List.to_seq |> IntMap.of_seq;

  let get_open_children: t => list(s) =
    fun
    | Operand(OperandHole | Num) => []
    | Operand(Paren(body)) => [body]
    | PreOp () => raise(Void_PreOp)
    | PostOp () => raise(Void_PostOp)
    | BinOp(OperatorHole | Arrow) => [];
};
open T;

[@deriving sexp]
type t = T.s;
include Tiles.Make(T);

module Inner = {
  type t =
    | Typ(T.s);

  let wrap = (ts: T.s) => Typ(ts);
  let unwrap =
    fun
    | Typ(ts) => Some(ts);
};

let rec contract = (ty: t): Type.t =>
  switch (root(ty)) {
  | Operand(operand) =>
    switch (operand) {
    | OperandHole => Hole
    | Num => Num
    | Paren(body) => contract(body)
    }
  | PreOp(((), _)) => raise(T.Void_PreOp)
  | PostOp((_, ())) => raise(T.Void_PostOp)
  | BinOp((ty1, binop, ty2)) =>
    switch (binop) {
    | OperatorHole => Hole
    | Arrow => Arrow(contract(ty1), contract(ty2))
    }
  };
