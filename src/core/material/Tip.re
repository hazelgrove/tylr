[@deriving (show({with_path: false}), sexp, yojson, ord)]
type t =
  | Conv
  | Conc;

[@deriving (show({with_path: false}), sexp, yojson, ord)]
type s = (t, t);
let s_all = [(Conv, Conv), (Conv, Conc), (Conc, Conv), (Conc, Conc)];

let is_conc =
  fun
  | Conv => false
  | Conc => true;

let display = ((l, r): s) => (l == Conv ? "<" : ">", r == Conv ? ">" : "<");
