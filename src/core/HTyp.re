open Sexplib.Std;
open Util;

[@deriving sexp]
type t = list(tile)
[@deriving sexp]
and tile = Tile.t(op, pre, post, bin)
[@deriving sexp]
and op =
  | OpHole
  | Num
  | Bool
  | Paren(t)
[@deriving sexp]
and pre = unit // empty
[@deriving sexp]
and post = unit // empty
[@deriving sexp]
and bin =
  | BinHole
  | Arrow;
exception Void_pre;
exception Void_post;

type closed_descendant = unit; // empty
type descendant = Descendant.t(t, closed_descendant);
exception Void_closed_descendant;

module T = {
  let sort = Sort.Typ;

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
    | Post () => raise(Void_post)
    | Bin(BinHole) => 1
    | Bin(Arrow) => 2;

  let associativity =
    [(1, Associativity.Left), (2, Right)] |> List.to_seq |> IntMap.of_seq;

  let get_open_children: t => list(s) =
    fun
    | Op(OpHole | Num | Bool) => []
    | Op(Paren(body)) => [body]
    | Pre () => raise(Void_pre)
    | Post () => raise(Void_post)
    | Bin(BinHole | Arrow) => [];
};
include Tiles.Make(T);

let rec contract = (ty: t): Type.t =>
  switch (root(ty)) {
  | Op(op) =>
    switch (op) {
    | OpHole => Hole
    | Num => Num
    | Bool => Bool
    | Paren(body) => contract(body)
    }
  | Pre(((), _)) => raise(Void_pre)
  | Post((_, ())) => raise(Void_post)
  | Bin((ty1, bin, ty2)) =>
    switch (bin) {
    | BinHole => Hole
    | Arrow => Arrow(contract(ty1), contract(ty2))
    }
  };
