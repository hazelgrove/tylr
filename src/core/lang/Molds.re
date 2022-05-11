open Mold;

let get = (label: Base.Tile.Label.t): list(Mold.t) => {
  let s = Sorts.mk;
  switch (label) {
  | [t] when Token.is_num(t) => [mk_op(s(Exp))]
  | [t] when Token.is_var(t) => [mk_op(s(Pat)), mk_op(s(Exp))]
  | ["(", ")"] => [
      mk_op(s(~in_=[Pat], Pat)),
      mk_op(s(~in_=[Exp], Exp)),
    ]
  //| ["λ", ".{", "}"] // temp andrew
  | ["λ", "{", "}"] => [mk_op(s(~in_=[Pat, Exp], Exp))]
  | ["!"] => [mk_post(Precedence.fact, s(Exp))]
  | ["[", "]"] => [mk_post(Precedence.ap, s(~in_=[Exp], Exp))]
  | ["*" | "/"] => [mk_bin(Precedence.mult, s(Exp))]
  | ["+" | "-"] => [mk_bin(Precedence.plus, s(Exp))]
  | [","] => [
      mk_bin(Precedence.prod, s(Pat)),
      mk_bin(Precedence.prod, s(Exp)),
    ]
  | ["?", ":"] => [mk_bin(Precedence.cond, s(~in_=[Exp], Exp))]
  | ["let", "=", "in"] => [
      mk_pre(Precedence.let_, s(~in_=[Pat, Exp], Exp)),
    ]
  | _ => []
  };
};
