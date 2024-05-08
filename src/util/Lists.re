let hd = Base.List.hd;
let hd_exn = Base.List.hd_exn;
let ft = Base.List.last;
let ft_exn = Base.List.last_exn;

module Frame = {
  // head elements are sequentially nearest neighbors of subject
  type t('x) = (list('x), list('x));
};

module Framed = {
  type t('subj, 'x) = ('subj, Frame.t('x));

  let hd =
    fun
    | [] => None
    | [hd, ...tl] => Some((hd, tl));
  let hd_exn = xs =>
    OptUtil.get_or_raise(Invalid_argument("Lists.Framed.hd_exn"), hd(xs));

  let ft = (xs: list('x)): option((list('x), 'x)) => {
    let rec go = (pre, xs) =>
      switch (xs) {
      | [] => None
      | [x] => Some((pre, x))
      | [x, ...xs] => go([x, ...pre], xs)
      };
    go([], xs);
  };
  let ft_exn = xs =>
    OptUtil.get_or_raise(Invalid_argument("Lists.Framed.ft_exn"), ft(xs));

  let nth = (n: int, xs: list('x)): option(t('x, 'x)) => {
    let (l, r) = Base.List.split_n(xs, n);
    switch (r) {
    | [] => None
    | [x, ...r] => Some((x, (l, r)))
    };
  };
  let nth_exn = (n, xs) =>
    nth(n, xs)
    |> OptUtil.get_or_raise(Invalid_argument("Lists.Framed.nth_exn"));

  /**
  * Returns framed sublist from index i (inclusive) to index j (exclusive).
  * Returns (_, ([], _)) if i < 0
  *         (_, (_, [])) if j > length of list
  *         ([], (_, _)) if i >= j
  */
  let sublist = (i: int, j: int, xs: list('x)): t(list('x), 'x) => {
    let (xs, r) = Base.List.split_n(xs, j);
    let (l, xs) = Base.List.split_n(xs, i);
    (xs, (l, r));
  };

  let elems = (xs: list('x)): list(t('x, 'x)) => {
    let rec go = (split: t('x, 'x)): list(t('x, 'x)) =>
      switch (split) {
      | (_, (_, [])) => [split]
      | (x, (l, [hd, ...tl])) => [split, ...go((hd, ([x, ...l], tl)))]
      };
    switch (xs) {
    | [] => []
    | [x, ...xs] => go((x, ([], xs)))
    };
  };
};

// let rec neighbors = (xs: list('x)): list(('x, 'x)) =>
//   switch (xs) {
//   | []
//   | [_] => []
//   | [x1, x2, ...xs] => [(x1, x2), ...neighbors([x2, ...xs])]
//   };

module Syntax = {
  let (let+) = (xs, f) => List.map(f, xs);
  let (and+) = Base.List.cartesian_product;
  let ( let* ) = (xs, f) => List.concat(List.map(f, xs));
};

// use stdlib once we're on 5.1
let find_index = (p, xs) =>
  xs
  |> List.mapi((i, x) => (i, x))
  |> List.filter(((_, x)) => p(x))
  |> Base.List.hd
  |> Option.map(fst);

let min = compare =>
  List.fold_left(
    (min, x) =>
      switch (min) {
      | None => Some(x)
      | Some(y) => Some(compare(x, y) <= 0 ? x : y)
      },
    None,
  );
