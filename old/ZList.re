type t('z, 'a) = (list('a), 'z, list('a));

let rec split_at = (
  n: int,
  xs: list('x),
): t('x, 'x) =>
  switch (n, xs) {
  | (_, []) => failwith("list index out of bounds")
  | (0, [x, ...xs]) => ([], x, xs)
  | (_, [x, ...xs]) =>
    let (prefix, z, suffix) = split_at(n - 1, xs);
    ([x, ...prefix], z, suffix);
  };