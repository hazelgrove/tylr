type t = Bound.t(Token.t);
let root: t = Bound.Root;
let indent =
  fun
  | Bound.Root => false
  | Node(tok) => Token.indent(tok);