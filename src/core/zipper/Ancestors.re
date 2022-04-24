[@deriving show]
type t = list((Ancestor.t, Siblings.t));

let empty = [];

let sort =
  fun
  | [] => Sort.root
  | [(a, _), ..._] => Ancestor.sort(a);

let sort_rank = (ancestors: t): int =>
  List.fold_right(
    ((a, sibs), (s, rank)) => {
      let rank =
        rank
        + Siblings.sort_rank(sibs, s)
        + Ancestor.sort_rank(a, Siblings.sorts(sibs));
      let s' = Ancestor.sort(a);
      (s', rank);
    },
    ancestors,
    (Sort.root, 0),
  )
  |> snd;
