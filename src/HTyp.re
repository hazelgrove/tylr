type t = (Skel.t, tiles)
and tiles = list(tile)
and tile = Tile.t(operand, preop, postop, binop)
and operand =
  | OperandHole
  | Num
  | Paren(t)
and preop = unit // empty
and postop = unit // empty
and binop =
  | OperatorHole
  | Arrow;

exception Void_PreOp;
exception Void_PostOp;

let rec contract = ((skel, tiles): t): Typ.t =>
  switch (skel) {
  | Operand(n) =>
    switch (Tile.get_operand(List.nth(tiles, n))) {
    | OperandHole => Hole
    | Num => Num
    | Paren(body) => contract(body)
    }
  | PreOp(_) => raise(Void_PreOp)
  | PostOp(_) => raise(Void_PostOp)
  | BinOp(skel1, n, skel2) =>
    switch (Tile.get_binop(List.nth(tiles, n))) {
    | OperatorHole => Hole
    | Arrow => Arrow(contract((skel1, tiles)), contract((skel2, tiles)))
    }
  };

module Tile = {
  type nonrec operand = operand;
  type nonrec preop = preop;
  type nonrec postop = postop;
  type nonrec binop = binop;
  type t = tile;

  let mk_operand_hole = (): t => Operand(OperandHole);
  let mk_operator_hole = (): t => BinOp(OperatorHole);

  let is_operand_hole = (==)(Tile.Operand(OperandHole));
  let is_operator_hole = (==)(Tile.BinOp(OperatorHole));

  let precedence: t => int =
    fun
    | Operand(_) => 0
    | PreOp(_) => raise(Void_PreOp)
    | PostOp(_) => raise(Void_PostOp)
    | BinOp(OperatorHole) => 1
    | BinOp(Arrow) => 2;

  let associativity =
    [(1, Associativity.Left), (2, Right)] |> List.to_seq |> IntMap.of_seq;

  let get_open_children: t => list(list(t)) =
    fun
    | Operand(OperandHole | Num) => []
    | Operand(Paren((_, body_tiles))) => [body_tiles]
    | PreOp(_) => raise(Void_PreOp)
    | PostOp(_) => raise(Void_PostOp)
    | BinOp(OperatorHole | Arrow) => [];
};
include TileUtil.Make(Tile);

let mk = (tiles: tiles): t => (parse(tiles), tiles);
