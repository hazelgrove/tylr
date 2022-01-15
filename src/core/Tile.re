open Util;

module Mold = {
  module Sorts = {
    type t = {
      out: Sort.t,
      in_: list(Sort.t),
    };

    let of_tokens: list(Token.t) => list(t) =
      fun
      | ["let", "=", "in"] => [{out: Exp, in_: [Pat, Exp]}]
      | _ => [];

    let mk = (~in_: list(Sort.t)=[], out: Sort.t) => {out, in};
  };

  module Role = {
    type t =
      | Op
      | Pre(Precedence.t)
      | Post(Precedence.t)
      | Bin(Precedence.t);

    let precedence =
      fun
      | Op => 0
      | Pre(p)
      | Post(p)
      | Bin(p) => p;
  };

  type t = {
    sorts: Sorts.t,
    role: Role.t,
  };

  let op = (sorts) => {
    sorts,
    role: Role.Op,
  };
  let pre = (p, sorts) => {
    sorts,
    role: Role.Pre(p),
  };

  let nibs = (mold: t): Nibs.t => {
    let (l, r) =
      switch (mold.shape) {
      | Op => (Left, Right)
      | Pre => (Left, Left)
      | Post => (Right, Right)
      | Bin => (Right, Left)
      };
    Nib.({orientation: l, sort}, {orientation: r, sort});
  };
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

let tokens = get(Aba.get_as);

let molds = (tile: t): list(Mold.t) =>
  Mold.(
    switch (tokens(tile)) {
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
    | _ => []
    }
  );

module Frame = {
  type tile = t;
  type t = Identified.t(Aba.Frame.b(Token.t, s));
  type s = ListFrame.t(tile);
};
