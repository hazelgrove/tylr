[@deriving (show({with_path: false}), sexp, yojson)]
type t = Bound.t(Token.t);
let root: t = Bound.Root;
let tok = t => Bound.Node(t);
let is_tok: t => _ = Bound.to_opt;
let unwrap = delim => Option.get(is_tok(delim));
let indent =
  fun
  | Bound.Root => false
  | Node(tok) => Token.indent(tok);
let padding =
  fun
  | Bound.Root => Padding.none
  | Node(tok: Token.t) => Mtrl.T.padding(tok.mtrl);
let merges = (l: t, r: t) =>
  switch (l, r) {
  | (Node(l), Node(r)) => Token.merges(l, r)
  | _ => false
  };
