[@deriving (show({with_path: false}), sexp, yojson, hash)]
type t =
  | L
  | R;

let toggle =
  fun
  | L => R
  | R => L;

// order(d, order(d, (x, y))) == (x, y)
let order: 'a. (t, ('a, 'a)) => ('a, 'a) =
  (d, (x, y)) =>
    switch (d) {
    | L => (x, y)
    | R => (y, x)
    };

let pick = (d, p) => fst(order(d, p));
