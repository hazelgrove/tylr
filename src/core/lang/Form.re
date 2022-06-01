open Mold;
module P = Precedence;

[@deriving show]
type label = list(string);

[@deriving show]
type t = {
  label,
  mold: Mold.t,
};

let mk = (label, mold) => {label, mold};

let var_exp = (t: string) => {
  assert(Token.is_var(t));
  mk([t], Mold.(mk_op(Exp, [])));
};

let var_pat = (t: string) => {
  assert(Token.is_var(t));
  mk([t], Mold.(mk_op(Pat, [])));
};

let int_exp = (n: int) => {
  assert(Token.is_num(string_of_int(n)));
  mk([string_of_int(n)], Mold.(mk_op(Exp, [])));
};

let mk_in = (str: string, sort: Sort.t, prec) =>
  mk([str], mk_bin(prec, sort, []));

/* Order in this list determines relative remolding
   priority for forms which share the same labels */
let forms = [
  ("times", mk_in("*", Exp, P.mult)),
  ("divide", mk_in("/", Exp, P.mult)),
  ("equals", mk_in("=", Exp, P.eqs)),
  ("gt", mk_in(">", Exp, P.gt)),
  ("plus", mk_in("+", Exp, P.plus)),
  ("minus", mk_in("-", Exp, P.plus)),
  ("comma_exp", mk_in(",", Exp, P.prod)),
  ("comma_pat", mk_in(",", Pat, P.prod)),
  ("fact", mk(["!"], mk_post(P.fact, Exp, []))),
  ("parens_exp", mk(["(", ")"], mk_op(Exp, [Exp]))),
  ("parens_pat", mk(["(", ")"], mk_op(Pat, [Pat]))),
  ("fun_", mk(["fun", "=>"], mk_pre(P.fun_, Pat, [Exp]))),
  ("ap", mk(["(", ")"], mk_post(P.ap, Exp, [Exp]))),
  ("let_", mk(["let", "=", "in"], mk_pre(P.let_, Exp, [Pat, Exp]))),
  ("cond", mk(["?", ":"], mk_bin(P.cond, Exp, [Exp]))),
  ("block", mk(["{", "}"], mk_op(Exp, [Exp]))),
  ("case", mk(["case", "of"], mk_pre(9, Exp, [Exp]))),
  ("rule_first", mk(["|", "=>"], mk_pre(9, Exp, [Pat]))),
  ("rule_rest", mk(["|", "=>"], mk_bin(9, Exp, [Pat]))),
];

let get: string => t = name => List.assoc(name, forms);
