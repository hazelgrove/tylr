open Mold;
let s = Sorts.mk;

[@deriving show]
type label = list(string);

[@deriving show]
type t = {
  label,
  mold: Mold.t,
};

let var_exp = (str: string) => {label: [str], mold: Mold.(mk_op(s(Exp)))};

let var_pat = (str: string) => {label: [str], mold: Mold.(mk_op(s(Pat)))};

let int_exp = (n: int) => {
  label: [string_of_int(n)],
  mold: Mold.(mk_op(s(Exp))),
};

let bin = (str: string, sort: Sort.t, prec) => {
  label: [str],
  mold: mk_bin(prec, s(sort)),
};

let times = bin("*", Exp, Precedence.mult);
let divide = bin("/", Exp, Precedence.mult);
let equals = bin("=", Exp, Precedence.mult); //TODO prec
let greater_than = bin(">", Exp, Precedence.mult); //TODO prec
let plus = bin("+", Exp, Precedence.plus);
let minus = bin("-", Exp, Precedence.plus);
let comma_exp = bin(",", Exp, Precedence.prod);
let comma_pat = bin(",", Pat, Precedence.prod);

let fact = {label: ["!"], mold: mk_post(Precedence.fact, s(Exp))};
let parens_exp = {label: ["(", ")"], mold: mk_op(s(~in_=[Exp], Exp))};
let parens_pat = {label: ["(", ")"], mold: mk_op(s(~in_=[Pat], Pat))};
let fun_ = {
  label: ["fun", "=>"],
  mold: mk_pre(Precedence.fun_, s(~in_=[Pat], Exp)),
};
let ap = {
  label: ["[", "]"],
  mold: mk_post(Precedence.ap, s(~in_=[Exp], Exp)),
};
let let_ = {
  label: ["let", "=", "in"],
  mold: mk_pre(Precedence.let_, s(~in_=[Pat, Exp], Exp)),
};
let cond = {
  label: ["?", ":"],
  mold: mk_bin(Precedence.cond, s(~in_=[Exp], Exp)),
};

/* Order in this list determines relative remolding
   priority for forms which share the same labels */
let forms = [
  parens_exp,
  parens_pat,
  fun_,
  fact,
  ap,
  times,
  divide,
  equals,
  greater_than,
  plus,
  minus,
  comma_exp,
  comma_pat,
  cond,
  let_,
];
