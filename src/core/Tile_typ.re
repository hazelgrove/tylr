open Util;
open Term_typ;

[@deriving sexp]
type t = Tile.t(op, pre, post, bin);

let precedence: t => int =
  Tile.get(
    _ => 0,
    () => raise(Void_pre),
    () => raise(Void_post),
    fun
    | BinHole => 1
    | Arrow => 2
    | Prod => 3,
  );

let associativity =
  [(1, Associativity.Left), (2, Right), (3, Left)]
  |> List.to_seq
  |> IntMap.of_seq;
