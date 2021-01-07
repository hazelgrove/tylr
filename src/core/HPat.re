open Sexplib.Std;
open Util;

[@deriving sexp]
type t = list(tile)
[@deriving sexp]
and tile = Tile.t(op, pre, post, bin)
[@deriving sexp]
and op =
  | OpHole
  | Var(Var.t)
  | Paren(t)
[@deriving sexp]
and pre = unit // empty
[@deriving sexp]
and post =
  | Ann(HoleStatus.t, HTyp.t)
[@deriving sexp]
and bin =
  | BinHole;

exception Void_pre;

type closed_descendant = HTyp.descendant;
type descendant = Descendant.t(t, closed_descendant);

module T = {
  let sort = Sort.Pat;

  type s = t;
  type t = tile;
  type nonrec op = op;
  type nonrec pre = pre;
  type nonrec post = post;
  type nonrec bin = bin;

  type nonrec descendant = descendant;
  type nonrec closed_descendant = closed_descendant;

  let mk_op_hole = () => OpHole;
  let mk_bin_hole = () => BinHole;

  let is_op_hole = (==)(OpHole);
  let is_bin_hole = (==)(BinHole);

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
include Tiles.Make(T);

// does not recurse into term
let get_hole_status = p =>
  root(p)
  |> Tile.get(
       fun
       | OpHole
       | Var(_)
       | Paren(_) => HoleStatus.NotInHole,
       (((), _)) => raise(Void_pre),
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
         (((), _)) => raise(Void_pre),
         fun
         | (subj, Ann(_, ann)) => subj @ [Post(Ann(status, ann))],
         fun
         | (l, BinHole as bin, r) => l @ [Bin(bin), ...r],
       )
  );
