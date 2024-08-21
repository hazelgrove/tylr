let is_empty = Base.List.is_empty;

let hd = Base.List.hd;
let hd_exn = Base.List.hd_exn;
let ft = Base.List.last;
let ft_exn = Base.List.last_exn;

let map_hd = f =>
  fun
  | [hd, ...tl] => [f(hd), ...tl]
  | xs => xs;
let map_ft = (f, xs) => List.rev(xs) |> map_hd(f) |> List.rev;

let single = x => [x];

let range = Base.List.range;

let snoc = (x, xs) => xs @ [x];

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
    Options.get_exn(Invalid_argument("Lists.Framed.hd_exn"), hd(xs));

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
    Options.get_exn(Invalid_argument("Lists.Framed.ft_exn"), ft(xs));
  let put_ft = (pre: list('x), ft: 'x) => List.rev([ft, ...pre]);

  let nth = (n: int, xs: list('x)): option(t('x, 'x)) => {
    let (l, r) = Base.List.split_n(xs, n);
    switch (r) {
    | [] => None
    | [x, ...r] => Some((x, (l, r)))
    };
  };
  let nth_exn = (n, xs) =>
    nth(n, xs) |> Options.get_exn(Invalid_argument("Lists.Framed.nth_exn"));

  /**
  * Returns framed sublist from index i (inclusive) to index j (exclusive).
  * Returns (_, ([], _)) if i < 0
  *         (_, (_, [])) if j > length of list
  *         ([], (_, _)) if i >= j
  */
  let sublist = (i: int, j: int, xs: list('x)): t(list('x), 'x) => {
    let (xs, r) = Base.List.split_n(xs, j);
    let (l, xs) = Base.List.split_n(xs, i);
    (xs, (List.rev(l), r));
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
  let return = single;
};

// use stdlib once we're on 5.1
let find_index = (p, xs) =>
  xs
  |> List.mapi((i, x) => (i, x))
  |> List.filter(((_, x)) => p(x))
  |> Base.List.hd
  |> Option.map(fst);

// picks first minimum element
let min = compare =>
  List.fold_left(
    (min, x) =>
      switch (min) {
      | None => Some(x)
      | Some(y) => Some(compare(y, x) <= 0 ? y : x)
      },
    None,
  );

let split_n = Base.List.split_n;

let split_bins = (num_bins, xs) => {
  open Syntax;
  let return = bins => return(List.rev(bins));
  let rec go = (~bins=[], num_bins, xs) =>
    switch (xs) {
    | _ when num_bins <= 0 => []
    | _ when num_bins == 1 => return([xs, ...bins])
    | [] => return(List.init(num_bins, Fun.const([])) @ bins)
    | [_, ..._] =>
      let* bin_size =
        range(~start=`inclusive, ~stop=`inclusive, 0, List.length(xs));
      let (bin, rest) = split_n(xs, bin_size);
      go(~bins=[bin, ...bins], num_bins - 1, rest);
    };
  go(num_bins, xs);
};

let fold_left = Base.List.fold_left;
let fold_right = Base.List.fold_right;

let fold_map = Base.List.fold_map;
