/**
 * Splits the first n elements from xs.
 */
let split_n = (n: int, xs: list('x)): (list('x), list('x)) => {
  let rec go = (~prefix: list('x)=[], n: int, xs: list('x)) => {
    if (n < 0) {
      raise(Invalid_argument("split_n"));
    } else if (n == 0) {
      (prefix, xs);
    } else {
      switch (xs) {
      | [] => raise(Invalid_argument("split_n"))
      | [x, ...xs] => go(~prefix=[x, ...prefix], n - 1, xs)
      };
    };
  };
  go(n, xs);
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

let rec split_nth = (n, xs) =>
  switch (n, xs) {
  | (_, []) => failwith("out of bounds")
  | (0, [x, ...suffix]) => ([], x, suffix)
  | (_, [x, ...xs]) =>
    let (prefix, subject, suffix) = split_nth(n - 1, xs);
    ([x, ...prefix], subject, suffix);
  };
