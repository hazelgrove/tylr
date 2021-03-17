open Sexplib.Std;

[@deriving sexp]
type t('z, 'x) = (list('x), 'z, list('x));

let erase = (erase_z: 'z => 'x, (prefix, z, suffix): t('z, 'x)) =>
  prefix @ [erase_z(z), ...suffix];

let rec split_at = (n: int, xs: list('x)): t('x, 'x) =>
  switch (n, xs) {
  | (_, []) => failwith("list index out of bounds")
  | (0, [x, ...xs]) => ([], x, xs)
  | (_, [x, ...xs]) =>
    let (prefix, z, suffix) = split_at(n - 1, xs);
    ([x, ...prefix], z, suffix);
  };
