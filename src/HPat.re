type t = HTerm.t(operand, preop, postop, binop)
and operand =
  | OperandHole
  | Var(Var.t)
  | Paren(t)
and preop = unit // empty
and postop =
  | Ann(HoleStatus.t, HTyp.t)
and binop =
  | OperatorHole;

exception Void_PreOp;

let rec set_hole_status = (status, p: t): t =>
  HTerm.set_hole_status(~set_tile, status, p)
and set_tile = (status, tile) =>
  tile
  |> Tile.map(
       ~operand=
         fun
         | OperandHole => OperandHole
         | Var(x) => Var(x)
         | Paren(body) => Paren(set_hole_status(status, body)),
       ~preop=
         fun
         | _ => raise(Void_PreOp),
       ~postop=
         fun
         | Ann(_, ann) => Ann(status, ann),
       ~binop=
         fun
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
    | PreOp(_) => raise(Void_PreOp)
    | PostOp(Ann(_)) => 2
    | BinOp(OperatorHole) => 1;

  let associativity =
    [(1, Associativity.Left)] |> List.to_seq |> IntMap.of_seq;

  let get_open_children: t => list(list(t)) =
    fun
    | Operand(OperandHole | Var(_)) => []
    | Operand(Paren((_, body_tiles))) => [body_tiles]
    | PreOp(_) => raise(Void_PreOp)
    | PostOp(Ann(_)) => []
    | BinOp(OperatorHole) => [];
};
include TileUtil.Make(Tile);
