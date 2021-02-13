open Util;

module Input: Tile.SORTED_INPUT with module Term := Term_typ = {
  type t = Tile.t(Term_typ.op, Term_typ.pre, Term_typ.post, Term_typ.bin);
  type s = list(t);

  let precedence: t => int =
    Tile.get(
      _ => 0,
      () => raise(Term_typ.Void_pre),
      () => raise(Term_typ.Void_post),
      fun
      | Term_typ.BinHole => 1
      | Arrow => 2,
    );
  let associativity =
    [(1, Associativity.Left), (2, Right)] |> List.to_seq |> IntMap.of_seq;
};
include Input;
include Tile.Make_sorted(Term_typ, Input);
