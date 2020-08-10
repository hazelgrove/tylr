let hd_opt =
  fun
  | [] => None
  | [hd, ..._] => Some(hd);

/**
 * Splits the first n elements from xs.
 */
let split_n_opt = (n: int, xs: list('x)): option((list('x), list('x))) => {
  let rec go = (~prefix: list('x)=[], n: int, xs: list('x)) =>
    if (n < 0) {
      None;
    } else if (n == 0) {
      Some((prefix, xs));
    } else {
      switch (xs) {
      | [] => None
      | [x, ...xs] => go(~prefix=[x, ...prefix], n - 1, xs)
      };
    };
  go(n, xs);
};

let split_n = (n: int, xs: list('x)): (list('x), list('x)) =>
  switch (split_n_opt(n, xs)) {
  | None => raise(Invalid_argument("split_n"))
  | Some(r) => r
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
  switch (split_n_opt(j, xs)) {
  | None => None
  | Some((left, right)) =>
    switch (split_n_opt(i, left)) {
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
