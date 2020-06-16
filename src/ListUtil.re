/**
 * Splits the first n elements from xs.
 */
let rec split_at = (n: int, xs: list('x)): option((list('x), list('x))) =>
  if (n < 0) {
    None;
  } else if (n == 0) {
    Some(([], xs));
  } else {
    switch (xs) {
    | [] => None
    | [x, ...xs] =>
      split_at(n - 1, xs)
      |> Option.map(((left, right)) => ([x, ...left], right))
    };
  };

/**
 * Returns sublist from index i (inclusive)
 * to index j (exclusive), coupled with the
 * surrounding prefix/suffix sublists.
 * Returns None if i > j.
 */
let split_sublist =
    (i: int, j: int, xs: list('x))
    : option((list('x), list('x), list('x))) => {
  switch (split_at(j, xs)) {
  | None => None
  | Some((left, right)) =>
    switch (split_at(i, left)) {
    | None => None
    | Some((left, mid)) => Some((left, mid, right))
    }
  };
};
