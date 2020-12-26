open Sexplib.Std;
open Util;

module T = {
  let sort = Sort.Pat;

  [@deriving sexp]
  type s = list(t)
  [@deriving sexp]
  and t = Tile.t(op, pre, post, bin)
  [@deriving sexp]
  and op =
    | OpHole
    | Var(Var.t)
    | Paren(s)
  [@deriving sexp]
  and pre = unit // empty
  [@deriving sexp]
  and post =
    | Ann(HoleStatus.t, HTyp.t)
  [@deriving sexp]
  and bin =
    | BinHole;

  exception Void_pre;

  let mk_op_hole = (): t => Op(OpHole);
  let mk_bin_hole = (): t => Bin(BinHole);

  let is_op_hole: t => bool = (==)(Tile.Op(OpHole));
  let is_bin_hole: t => bool = (==)(Tile.Bin(BinHole));

  let precedence: t => int =
    fun
    | Op(_) => 0
    | Pre () => raise(Void_pre)
    | Post(Ann(_)) => 2
    | Bin(BinHole) => 1;

  let associativity =
    [(1, Associativity.Left)] |> List.to_seq |> IntMap.of_seq;

  let get_open_children: t => list(s) =
    fun
    | Op(OpHole | Var(_)) => []
    | Op(Paren(body)) => [body]
    | Pre () => raise(Void_pre)
    | Post(Ann(_)) => []
    | Bin(BinHole) => [];
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
       | OpHole
       | Var(_)
       | Paren(_) => HoleStatus.NotInHole,
       (((), _)) => raise(T.Void_pre),
       fun
       | (_, Ann(status, _)) => status,
       fun
       | (_, BinHole, _) => HoleStatus.NotInHole,
     );

// recurses into term
let rec put_hole_status = (status: HoleStatus.t, p: t): t =>
  Tile.(
    root(p)
    |> get(
         fun
         | OpHole => [Op(OpHole)]
         | Var(x) => [Op(Var(x))]
         | Paren(body) => [Op(Paren(put_hole_status(status, body)))],
         (((), _)) => raise(T.Void_pre),
         fun
         | (subj, Ann(_, ann)) => subj @ [Post(Ann(status, ann))],
         fun
         | (l, BinHole as bin, r) => l @ [Bin(bin), ...r],
       )
  );
