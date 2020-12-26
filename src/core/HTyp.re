open Sexplib.Std;
open Util;

module T = {
  let sort = Sort.Typ;

  [@deriving sexp]
  type s = list(t)
  [@deriving sexp]
  and t = Tile.t(op, pre, post, bin)
  [@deriving sexp]
  and op =
    | OpHole
    | Num
    | Bool
    | Paren(s)
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

  let mk_op_hole = (): t => Op(OpHole);
  let mk_bin_hole = (): t => Bin(BinHole);

  let is_op_hole: t => bool = (==)(Tile.Op(OpHole));
  let is_bin_hole: t => bool = (==)(Tile.Bin(BinHole));

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
  | Op(op) =>
    switch (op) {
    | OpHole => Hole
    | Num => Num
    | Bool => Bool
    | Paren(body) => contract(body)
    }
  | Pre(((), _)) => raise(T.Void_pre)
  | Post((_, ())) => raise(T.Void_post)
  | Bin((ty1, bin, ty2)) =>
    switch (bin) {
    | BinHole => Hole
    | Arrow => Arrow(contract(ty1), contract(ty2))
    }
  };
