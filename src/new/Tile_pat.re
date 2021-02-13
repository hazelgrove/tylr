module Input: Tile.SORTED_INPUT with module Term := Term_pat = {
  module Term = Term_pat;

  type t = Tile.t(Term_pat.op, Term_pat.pre, Term_pat.post, Term_pat.bin);
  type s = list(t);

  let precedence: t => int =
    Tile.get(
      _ => 0,
      () => raise(Term_pat.Void_pre),
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
