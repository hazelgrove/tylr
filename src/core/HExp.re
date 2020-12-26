open Sexplib.Std;
open Util;

module T = {
  let sort = Sort.Exp;

  [@deriving sexp]
  type s = list(t)
  [@deriving sexp]
  and t = Tile.t(op, pre, post, bin)
  [@deriving sexp]
  and op =
    | OpHole
    | Num(HoleStatus.t, int)
    | Var(HoleStatus.t, Var.t)
    | Paren(s)
  [@deriving sexp]
  and pre =
    | Lam(HoleStatus.t, HPat.t)
    | Let(HPat.t, s)
  [@deriving sexp]
  and post =
    | Ap(HoleStatus.t, s)
  [@deriving sexp]
  and bin =
    | Plus(HoleStatus.t)
    | BinHole;

  let mk_op_hole = (): t => Op(OpHole);
  let mk_bin_hole = (): t => Bin(BinHole);

  let is_op_hole: t => bool = (==)(Tile.Op(OpHole));
  let is_bin_hole: t => bool = (==)(Tile.Bin(BinHole));

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
      | BinHole => 2,
    );

  let associativity =
    [(2, Associativity.Left), (3, Left)] |> List.to_seq |> IntMap.of_seq;

  let get_open_children: t => list(s) =
    Tile.get(
      fun
      | OpHole
      | Num(_)
      | Var(_) => []
      | Paren(body) => [body],
      fun
      | Lam(_) => []
      | Let(_, def) => [def],
      fun
      | Ap(_, arg) => [arg],
      fun
      | BinHole
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
       | OpHole
       | Paren(_) => HoleStatus.NotInHole
       | Num(status, _)
       | Var(status, _) => status,
       fun
       | (Lam(status, _), _) => status
       | (Let(_), _) => HoleStatus.NotInHole,
       fun
       | (_, Ap(status, _)) => status,
       fun
       | (_, BinHole, _) => HoleStatus.NotInHole
       | (_, Plus(status), _) => status,
     );

// recurses into term
let rec put_hole_status = (status: HoleStatus.t, e: t): t =>
  Tile.(
    root(e)
    |> get(
         fun
         | OpHole as op => [Op(op)]
         | Num(_, n) => [Op(Num(status, n))]
         | Var(_, x) => [Op(Var(status, x))]
         | Paren(body) => [Op(Paren(put_hole_status(status, body)))],
         fun
         | (Lam(_, p), body) => [Pre(Lam(status, p)), ...body]
         | (Let(_) as pre, body) => [
             Pre(pre),
             ...put_hole_status(status, body),
           ],
         fun
         | (fn, Ap(_, arg)) => fn @ [Post(Ap(status, arg))],
         fun
         | (l, BinHole as bin, r) => l @ [Bin(bin), ...r]
         | (l, Plus(_), r) => l @ [Bin(Plus(status)), ...r],
       )
  );
