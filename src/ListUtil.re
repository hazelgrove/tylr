let hd_opt =
  fun
  | [] => None
  | [hd, ..._] => Some(hd);

/**
 * `split_n_opt(n, xs)` splits the first `n` elements from `xs`
 * if `xs` has `n` or more elements
 */
let split_n_opt = (n: int, xs: list('x)): option((list('x), list('x))) => {
  let rec go = (n: int, xs: list('x)) =>
    if (n < 0) {
      None;
    } else if (n == 0) {
      Some(([], xs));
    } else {
      switch (xs) {
      | [] => None
      | [x, ...xs] =>
        go(n - 1, xs)
        |> Option.map(((prefix, suffix)) => ([x, ...prefix], suffix))
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
let split_sublist_opt =
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
let split_sublist =
    (i: int, j: int, xs: list('x)): (list('x), list('x), list('x)) =>
  switch (split_sublist_opt(i, j, xs)) {
  | None => raise(Invalid_argument("ListUtil.split_sublist"))
  | Some(r) => r
  };

let rec split_nth = (n, xs) =>
  switch (n, xs) {
  | (_, []) =>
    raise(Invalid_argument("ListUtil.split_nth: index out of bounds"))
  | (0, [x, ...suffix]) => ([], x, suffix)
  | (_, [x, ...xs]) =>
    let (prefix, subject, suffix) = split_nth(n - 1, xs);
    ([x, ...prefix], subject, suffix);
  };

let rec put_nth = (n: int, x: 'x, xs: list('x)): list('x) =>
  switch (n, xs) {
  | (_, []) => failwith("out of bounds")
  | (0, [_, ...tl]) => [x, ...tl]
  | (_, [hd, ...tl]) =>
    let tl = put_nth(n - 1, x, tl);
    [hd, ...tl];
  };

let rec map_nth_opt = (n: int, f: 'x => 'x, xs: list('x)): option(list('x)) =>
  switch (n, xs) {
  | (_, []) => None
  | (0, [x, ...xs]) => Some([f(x), ...xs])
  | (_, [x, ...xs]) =>
    map_nth_opt(n - 1, f, xs) |> Option.map(List.cons(x))
  };

let map_nth = (n, f, xs) =>
  switch (map_nth_opt(n, f, xs)) {
  | None => raise(Invalid_argument("ListUtil.map_nth: index out of bounds"))
  | Some(xs) => xs
  };

let rec split_last_opt = (xs: list('x)): option((list('x), 'x)) =>
  switch (xs) {
  | [] => None
  | [x] => Some(([], x))
  | [x, ...xs] =>
    split_last_opt(xs)
    |> Option.map(((leading, last)) => ([x, ...leading], last))
  };
let last_opt = xs => xs |> split_last_opt |> Option.map(snd);

let split_last = (xs: list('x)): (list('x), 'x) =>
  switch (split_last_opt(xs)) {
  | None => raise(Invalid_argument("ListUtil.split_last"))
  | Some(r) => r
  };
let leading = xs => fst(split_last(xs));
