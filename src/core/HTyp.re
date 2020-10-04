module Tile = {
  type s = list(t)
  and t = Tile.t(operand, preop, postop, binop)
  and operand =
    | OperandHole
    | Num
    | Paren(s)
  and preop = unit // empty
  and postop = unit // empty
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
    | PreOp(_) => raise(Void_PreOp)
    | PostOp(_) => raise(Void_PostOp)
    | BinOp(OperatorHole) => 1
    | BinOp(Arrow) => 2;

  let associativity =
    [(1, Associativity.Left), (2, Right)] |> List.to_seq |> IntMap.of_seq;

  let get_open_children: t => list(s) =
    fun
    | Operand(OperandHole | Num) => []
    | Operand(Paren(body)) => [body]
    | PreOp(_) => raise(Void_PreOp)
    | PostOp(_) => raise(Void_PostOp)
    | BinOp(OperatorHole | Arrow) => [];
};
include Tiles.Make(Tile);

type t = Tile.s;

type inner_tiles =
  | Typ(Tile.s);

let rec contract = (ty: t): Type.t =>
  switch (root(ty)) {
  | Operand(operand) =>
    switch (operand) {
    | OperandHole => Hole
    | Num => Num
    | Paren(body) => contract(body)
    }
  | PreOp(_) => raise(Tile.Void_PreOp)
  | PostOp(_) => raise(Tile.Void_PostOp)
  | BinOp((ty1, binop, ty2)) =>
    switch (binop) {
    | OperatorHole => Hole
    | Arrow => Arrow(contract(ty1), contract(ty2))
    }
  };
