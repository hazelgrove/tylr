open Util;

open Term_pat;

[@deriving sexp]
type t = Tile.t(op, pre, post, bin);

let precedence: t => int =
  Tile.get(
    _ => 0,
    () => raise(Void_pre),
    fun
    | Ann(_) => 2,
    fun
    | BinHole => 1,
  );

let associativity =
  [(1, Associativity.Left)] |> List.to_seq |> IntMap.of_seq;
