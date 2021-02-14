open Util;

module Input: Tile.SORTED_INPUT with module Term := Term_pat = {
  open Term_pat;

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
};
include Input;
include Tile.Make_sorted(Term_pat, Input);
