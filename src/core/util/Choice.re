[@deriving sexp]
type t('a) =
  | Nil
  | One('a)
  | Either(t('a), t('a))
  | Prefer(t('a), t('a));

let nil = Nil;
let one = a => One(a);
let either = (l, r) => Either(l, r);
let prefer = (l, r) => Prefer(l, r);

let eithers = cs => List.fold_left(either, nil, cs);
let prefers = cs => List.fold_left(prefer, nil, cs);

let any = as_ => eithers(List.map(one, as_));
let ord = as_ => prefers(List.map(one, as_));

let rec map = f =>
  fun
  | Nil => Nil
  | One(a) => One(f(a))
  | Either(l, r) => Either(map(f, l), map(f, r))
  | Prefer(l, r) => Prefer(map(f, l), map(f, r));
let rec bind = f =>
  fun
  | Nil => Nil
  | One(a) => f(a)
  | Either(l, r) => Either(bind(f, l), bind(f, r))
  | Prefer(l, r) => Prefer(bind(f, l), bind(f, r));

let rec fold = (~nil, ~one, ~either, ~prefer, c) => {
  let go = fold(~nil, ~one, ~either, ~prefer);
  switch (c) {
  | Nil => nil
  | One(c) => one(c)
  | Either(l, r) => either(go(l), go(r))
  | Prefer(l, r) => prefer(go(l), go(r))
  };
};

module Syntax = {
  let ( let* ) = (c, f) => bind(f, c);
  let (let+) = (c, f) => map(f, c);
};
