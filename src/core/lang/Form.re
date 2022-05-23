open Mold;
module P = Precedence;

let s = sorts =>
  switch (List.rev(sorts)) {
  | [hd, ...tl] => Sorts.mk(~in_=List.rev(tl), hd)
  | [] => failwith("Form: empty sort list")
  };

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
  mk([t], Mold.(mk_op(s([Exp]))));
};

let var_pat = (t: string) => {
  assert(Token.is_var(t));
  mk([t], Mold.(mk_op(s([Pat]))));
};

let int_exp = (n: int) => {
  assert(Token.is_num(string_of_int(n)));
  mk([string_of_int(n)], Mold.(mk_op(s([Exp]))));
};

let mk_in = (str: string, sort: Sort.t, prec) =>
  mk([str], mk_bin(prec, s([sort])));

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
  ("fact", mk(["!"], mk_post(P.fact, s([Exp])))),
  ("parens_exp", mk(["(", ")"], mk_op(s([Exp, Exp])))),
  ("parens_pat", mk(["(", ")"], mk_op(s([Pat, Pat])))),
  ("fun_", mk(["fun", "=>"], mk_pre(P.fun_, s([Pat, Exp])))),
  ("ap", mk(["[", "]"], mk_post(P.ap, s([Exp, Exp])))),
  ("let_", mk(["let", "=", "in"], mk_pre(P.let_, s([Pat, Exp, Exp])))),
  ("cond", mk(["?", ":"], mk_bin(P.cond, s([Exp, Exp])))),
];

let get: string => t = name => List.assoc(name, forms);
