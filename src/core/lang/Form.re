open Mold;
let s = Sorts.mk;

type t = {
  label: Base.Tile.Label.t,
  mold: Mold.t,
};

let var_exp = (str: string) => {
  label: [str],
  mold: Mold.(mk_op(Sorts.mk(Exp))),
};

let var_pat = (str: string) => {
  label: [str],
  mold: Mold.(mk_op(Sorts.mk(Pat))),
};

let int_exp = (n: int) => {
  label: [string_of_int(n)],
  mold: Mold.(mk_op(Sorts.mk(Exp))),
};

let bin = (str: string, sort: Sort.t, prec) => {
  label: [str],
  mold: mk_bin(prec, s(sort)),
};

let parens_exp = {
  label: ["(", ")"],
  mold: mk_op(Sorts.mk(~in_=[Exp], Exp)),
};
let parens_pat = {
  label: ["(", ")"],
  mold: mk_op(Sorts.mk(~in_=[Pat], Pat)),
};

let lambda = {
  label: ["fun", "=>"],
  mold: mk_pre(Precedence.fun_, Mold.Sorts.mk(~in_=[Pat], Exp)),
};

let fact = {label: ["!"], mold: mk_post(Precedence.fact, s(Exp))};

let app = {
  label: ["[", "]"],
  mold: mk_post(Precedence.ap, s(~in_=[Exp], Exp)),
};

let times = bin("*", Exp, Precedence.mult);
let divide = bin("/", Exp, Precedence.mult);
let equals = bin("=", Exp, Precedence.mult); //TODO prec
let greater_than = bin(">", Exp, Precedence.mult); //TODO prec
let plus = bin("+", Exp, Precedence.plus);
let minus = bin("-", Exp, Precedence.plus);
let comma_exp = bin(",", Exp, Precedence.prod);
let comma_pat = bin(",", Pat, Precedence.prod);

let ternary = {
  label: ["?", ":"],
  mold: mk_bin(Precedence.cond, s(~in_=[Exp], Exp)),
};

let let_ = {
  label: ["let", "=", "in"],
  mold: mk_op(Sorts.mk(~in_=[Pat, Exp], Exp)),
};

let forms = [
  parens_exp,
  parens_pat,
  lambda,
  fact,
  app,
  times,
  divide,
  equals,
  greater_than,
  plus,
  minus,
  comma_exp,
  comma_pat,
  ternary,
  let_,
];
