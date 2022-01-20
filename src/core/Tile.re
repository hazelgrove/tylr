open Util;

module Id = {
  type t = int;
};

module Shape = {
  type t =
    | Op
    | Pre(Precedence.t)
    | Post(Precedence.t)
    | Bin(Precedence.t);

  let precedence: t => Precedence.t = failwith("todo");
};

module Sorts = {
  type t = {
    out: Sort.t,
    in_: list(Sort.t),
  };
};

module Mold = {
  type t = {
    shape: Shape.t,
    sorts: Sorts.t,
  };
  let nibs: (~index: int=?, t) => Nibs.t = failwith("todo");
};

module Form = {
  type t = list(Token.t);

  let molds =
    fun
    | [t] when Token.is_num(t) => [op(Sorts.mk(Exp))]
    | [t] when Token.is_var(t) => [op(Sorts.mk(Pat)), op(Sorts.mk(Exp))]
    | ["(", ")"] => [
        op(Sorts.mk(~in_=[Pat], Pat)),
        op(Sorts.mk(~in_=[Exp], Exp)),
      ]
    | ["λ", "{", "}"] => [op(Sorts.mk(~in_=[Pat, Exp], Exp))]
    | ["!"] => [post(1, Sorts.mk(Exp))]
    | ["[", "]"] => [post(2, Sorts.mk(~in_=[Exp], Exp))]
    | ["*" | "/"] => [bin(3, Sorts.mk(Exp))]
    | ["+" | "-"] => [bin(4, Sorts.mk(Exp))]
    | [","] => [bin(5, Sorts.mk(Exp)), bin(5, Sorts.mk(Pat))]
    | ["?", ":"] => [bin(6, Sorts.mk(~in_=[Exp], Exp))]
    | ["let", "=", "in"] => [pre(9, Sorts.mk(~in_=[Pat, Exp], Exp))]
    | _ => [];
};

[@deriving sexp]
type s = list(t)
and t =
  | Hole(Sort.t)
  | Sep
  | Tokens({
      id: Id.t,
      mold: Mold.t,
      tokens: Aba.t(Token.t, s)
    });

let form: t => Form.t = get(Aba.get_as);

module Frame = {
  type t = Identified.t(Aba.Frame.b(Token.t, s));
};
