module Sym: {
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t = Sym.t(Label.t, Sort.t);
};

// [@deriving (show({with_path: false}), sexp, yojson)]
type t = Sort.Map.t(Prec.Table.t(Regex.t(Sym.t)));
let v: t;
