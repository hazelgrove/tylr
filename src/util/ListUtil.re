let is_empty =
  fun
  | [] => true
  | _ => false;

let rec join = (sep: 'x, xs: list('x)): list('x) =>
  switch (xs) {
  | [] => []
  | [x] => [x]
  | [x, ...xs] => [x, sep, ...join(sep, xs)]
  };

let hd_opt =
  fun
  | [] => None
  | [hd, ..._] => Some(hd);

let rec nth_opt = (n, xs) =>
  n < 0
    ? None
    : (
      switch (xs) {
      | [] => None
      | [hd, ...tl] => n == 0 ? Some(hd) : nth_opt(n - 1, tl)
      }
    );

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
  | None =>
    raise(Invalid_argument("ListUtil.split_n: " ++ string_of_int(n)))
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
let sublist = ((i, j), xs: list('x)): list('x) => {
  let (_, sublist, _) = split_sublist(i, j, xs);
  sublist;
};

let rec split_nth_opt = (n, xs) =>
  switch (n, xs) {
  | _ when n < 0 => None
  | (_, []) => None
  | (0, [x, ...suffix]) => Some(([], x, suffix))
  | (_, [x, ...xs]) =>
    split_nth_opt(n - 1, xs)
    |> Option.map(((prefix, subject, suffix)) =>
         ([x, ...prefix], subject, suffix)
       )
  };
let split_nth = (n, xs) =>
  switch (split_nth_opt(n, xs)) {
  | None =>
    raise(Invalid_argument("ListUtil.split_nth: " ++ string_of_int(n)))
  | Some(r) => r
  };

let rec put_nth = (n: int, x: 'x, xs: list('x)): list('x) =>
  switch (n, xs) {
  | (_, []) => failwith("out of bounds")
  | (0, [_, ...tl]) => [x, ...tl]
  | (_, [hd, ...tl]) =>
    let tl = put_nth(n - 1, x, tl);
    [hd, ...tl];
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

let split_first = (xs: list('x)): ('x, list('x)) =>
  switch (xs) {
  | [] => raise(Invalid_argument("ListUtil.split_first"))
  | [first, ...trailing] => (first, trailing)
  };

let rec fold_left_map =
        (f: ('acc, 'x) => ('acc, 'y), start: 'acc, xs: list('x))
        : ('acc, list('y)) =>
  switch (xs) {
  | [] => (start, [])
  | [x, ...xs] =>
    let (new_acc, y) = f(start, x);
    let (final, ys) = fold_left_map(f, new_acc, xs);
    (final, [y, ...ys]);
  };

let rec take_while = (p: 'x => bool, xs: list('x)): (list('x), list('x)) =>
  switch (xs) {
  | [] => ([], [])
  | [hd, ...tl] =>
    if (p(hd)) {
      let (taken, rest) = take_while(p, tl);
      ([hd, ...taken], rest);
    } else {
      ([], xs);
    }
  };

let take_2 =
  fun
  | [x1, x2, ..._] => (x1, x2)
  | _ => raise(Invalid_argument("ListUtil.take_2"));
let take_3 =
  fun
  | [x1, x2, x3, ..._] => (x1, x2, x3)
  | _ => raise(Invalid_argument("ListUtil.take_3"));
let take_4 =
  fun
  | [x1, x2, x3, x4, ..._] => (x1, x2, x3, x4)
  | _ => raise(Invalid_argument("ListUtil.take_4"));
let take_5 =
  fun
  | [x1, x2, x3, x4, x5, ..._] => (x1, x2, x3, x4, x5)
  | _ => raise(Invalid_argument("ListUtil.take_5"));
