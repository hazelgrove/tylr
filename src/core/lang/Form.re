open Mold;
module P = Precedence;

[@deriving show]
type label = list(string);

[@deriving show]
type dir =
  | Bi
  | Front
  | Back;

[@deriving show]
type expansion_time =
  | Static
  | Instant
  | Delayed;

[@deriving show]
type expansion = (expansion_time, expansion_time);

[@deriving show]
type t = {
  label,
  expansion,
  mold: Mold.t,
};

let mk = (label, mold) => {label, mold, expansion: (Static, Static)};
let mk_ii = (label, mold) => {label, mold, expansion: (Instant, Instant)};
let mk_is = (label, mold) => {label, mold, expansion: (Instant, Static)};
let mk_ds = (label, mold) => {label, mold, expansion: (Delayed, Static)};
let mk_di = (label, mold) => {label, mold, expansion: (Delayed, Instant)};

let mono_molds = (label: list(Token.t)): option(list(Mold.t)) =>
  // TODO(andrew): further abstract this (assoc ls of regexps to molds)
  switch (label) {
  | [t] when Token.is_num(t) => Some([mk_op(Exp, [])])
  | [t] when Token.is_var(t) => Some([mk_op(Pat, []), mk_op(Exp, [])])
  | _ => None
  };

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

let mk_infix = (str: string, sort: Sort.t, prec) =>
  mk([str], mk_bin(prec, sort, []));

/* Order in this list determines relative remolding
   priority for forms which share the same labels */
let forms = [
  ("times", mk_infix("*", Exp, P.mult)),
  ("divide", mk_infix("/", Exp, P.mult)),
  ("equals", mk_infix("=", Exp, P.eqs)),
  ("gt", mk_infix(">", Exp, P.gt)),
  ("plus", mk_infix("+", Exp, P.plus)),
  ("minus", mk_infix("-", Exp, P.plus)),
  ("comma_exp", mk_infix(",", Exp, P.prod)),
  ("comma_pat", mk_infix(",", Pat, P.prod)),
  ("fact", mk(["!"], mk_post(P.fact, Exp, []))),
  ("parens_exp", mk_ii(["(", ")"], mk_op(Exp, [Exp]))),
  ("parens_pat", mk_ii(["(", ")"], mk_op(Pat, [Pat]))),
  ("fun_", mk_di(["fun", "=>"], mk_pre(P.fun_, Pat, [Exp]))),
  /* Something must instant on => as not valid monotile on its own */
  ("ap", mk_ii(["(", ")"], mk_post(P.ap, Exp, [Exp]))),
  ("let_", mk_ds(["let", "=", "in"], mk_pre(P.let_, Exp, [Pat, Exp]))),
  ("cond", mk_ii(["?", ":"], mk_bin(P.cond, Exp, [Exp]))),
  ("block", mk_ii(["{", "}"], mk_op(Exp, [Exp]))),
  ("case", mk_ds(["case", "of"], mk_pre(9, Exp, [Exp]))),
  ("rule_first", mk_is(["|", "=>"], mk_pre(9, Exp, [Pat]))),
  /* Something must instant on | as not valid monotile on its own */
  ("rule_rest", mk_is(["|", "=>"], mk_bin(9, Exp, [Pat]))),
];

let get: string => t = name => List.assoc(name, forms);
