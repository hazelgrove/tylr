open Sexplib.Std;
open Util;

module T = {
  let sort = Sort.Pat;

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
    | PreOp () => raise(Void_PreOp)
    | PostOp(Ann(_)) => 2
    | BinOp(OperatorHole) => 1;

  let associativity =
    [(1, Associativity.Left)] |> List.to_seq |> IntMap.of_seq;

  let get_open_children: t => list(s) =
    fun
    | Operand(OperandHole | Var(_)) => []
    | Operand(Paren(body)) => [body]
    | PreOp () => raise(Void_PreOp)
    | PostOp(Ann(_)) => []
    | BinOp(OperatorHole) => [];
};
open T;

[@deriving sexp]
type t = T.s;
include Tiles.Make(T);

module Inner = {
  type t =
    | Pat(T.s)
    | Other(HTyp.Inner.t);

  let wrap = (ts: T.s) => Pat(ts);
  let unwrap =
    fun
    | Other(_) => None
    | Pat(ts) => Some(ts);
};

// does not recurse into term
let get_hole_status = p =>
  root(p)
  |> Tile.get(
       fun
       | OperandHole
       | Var(_)
       | Paren(_) => HoleStatus.NotInHole,
       (((), _)) => raise(T.Void_PreOp),
       fun
       | (_, Ann(status, _)) => status,
       fun
       | (_, OperatorHole, _) => HoleStatus.NotInHole,
     );

// recurses into term
let rec put_hole_status = (status: HoleStatus.t, p: t): t =>
  Tile.(
    root(p)
    |> get(
         fun
         | OperandHole => [Operand(OperandHole)]
         | Var(x) => [Operand(Var(x))]
         | Paren(body) => [Operand(Paren(put_hole_status(status, body)))],
         (((), _)) => raise(T.Void_PreOp),
         fun
         | (subj, Ann(_, ann)) => subj @ [PostOp(Ann(status, ann))],
         fun
         | (l, OperatorHole as bin, r) => l @ [BinOp(bin), ...r],
       )
  );
