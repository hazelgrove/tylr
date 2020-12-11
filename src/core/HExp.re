open Sexplib.Std;
open Util;

module T = {
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
    | Let(HPat.t, s)
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
    Tile.get(
      _ => 0,
      fun
      | Lam(_) => 10
      | Let(_) => 11,
      fun
      | Ap(_) => 1,
      fun
      | Plus(_) => 3
      | OperatorHole => 2,
    );

  let associativity =
    [(2, Associativity.Left), (3, Left)] |> List.to_seq |> IntMap.of_seq;

  let get_open_children: t => list(s) =
    Tile.get(
      fun
      | OperandHole
      | Num(_)
      | Var(_) => []
      | Paren(body) => [body],
      fun
      | Lam(_) => []
      | Let(_, def) => [def],
      fun
      | Ap(_, arg) => [arg],
      fun
      | OperatorHole
      | Plus(_) => [],
    );
};
open T;

[@deriving sexp]
type t = T.s;
include Tiles.Make(T);

module Inner = {
  type t =
    | Exp(T.s)
    | Other(HPat.Inner.t);

  let wrap = (ts: T.s) => Exp(ts);
  let unwrap =
    fun
    | Other(_) => None
    | Exp(ts) => Some(ts);
};

// does not recurse into term
let get_hole_status = e =>
  root(e)
  |> Tile.get(
       fun
       | OperandHole
       | Paren(_) => HoleStatus.NotInHole
       | Num(status, _)
       | Var(status, _) => status,
       fun
       | (Lam(status, _), _) => status
       | (Let(_), _) => HoleStatus.NotInHole,
       fun
       | (_, Ap(status, _)) => status,
       fun
       | (_, OperatorHole, _) => HoleStatus.NotInHole
       | (_, Plus(status), _) => status,
     );

// recurses into term
let rec put_hole_status = (status: HoleStatus.t, e: t): t =>
  Tile.(
    root(e)
    |> get(
         fun
         | OperandHole as op => [Operand(op)]
         | Num(_, n) => [Operand(Num(status, n))]
         | Var(_, x) => [Operand(Var(status, x))]
         | Paren(body) => [Operand(Paren(put_hole_status(status, body)))],
         fun
         | (Lam(_, p), body) => [PreOp(Lam(status, p)), ...body]
         | (Let(_) as pre, body) => [
             PreOp(pre),
             ...put_hole_status(status, body),
           ],
         fun
         | (fn, Ap(_, arg)) => fn @ [PostOp(Ap(status, arg))],
         fun
         | (l, OperatorHole as bin, r) => l @ [BinOp(bin), ...r]
         | (l, Plus(_), r) => l @ [BinOp(Plus(status)), ...r],
       )
  );
