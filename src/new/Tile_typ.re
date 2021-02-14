open Util;

module Input: Tile.SORTED_INPUT with module Term := Term_typ = {
  open Term_typ;

  type t = Tile.t(op, pre, post, bin);

  let precedence: t => int =
    Tile.get(
      _ => 0,
      () => raise(Void_pre),
      () => raise(Void_post),
      fun
      | BinHole => 1
      | Arrow => 2,
    );
  let associativity =
    [(1, Associativity.Left), (2, Right)] |> List.to_seq |> IntMap.of_seq;
};
include Input;
include Tile.Make_sorted(Term_typ, Input);
