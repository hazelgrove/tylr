[@deriving (show({with_path: false}), sexp, yojson, ord)]
type t =
  | Conv
  | Conc;

[@deriving (show({with_path: false}), sexp, yojson, ord)]
type s = (t, t);

let display = ((l, r): s) => (l == Conv ? "<" : ">", r == Conv ? ">" : "<");
