module Input: Tile.SORTED_INPUT with module Term := Term_exp = {
  open Term_exp;

  type t = Tile.t(op, pre, post, bin);

  let precedence: t => int =
    Tile.get(
      _ => 0,
      fun
      | Lam(_) => 10
      | Let(_) => 11,
      fun
      | Ap(_) => 1,
      fun
      | Plus(_) => 3
      | BinHole => 2,
    );
  let associativity =
    [(2, Associativity.Left), (3, Left)] |> List.to_seq |> IntMap.of_seq;
};
include Input;
include Tile.Make_sorted(Term_exp, Input);
