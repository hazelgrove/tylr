let map2 = (f, o1, o2) =>
  switch (o1, o2) {
  | (None, _)
  | (_, None) => None
  | (Some(v1), Some(v2)) => Some(f(v1, v2))
  };

let product = (o1, o2) =>
  switch (o1, o2) {
  | (None, _)
  | (_, None) => None
  | (Some(a), Some(b)) => Some((a, b))
  };

module Syntax = {
  let ( let* ) = Option.bind;
  let (let+) = (o, f) => Option.map(f, o);
  let (and+) = product;
};
