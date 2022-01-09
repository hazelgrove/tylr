open Util;

[@deriving sexp]
type s = Aba.t(list(t), Nibbed.t(Hole.t))
and t = Identified.t(Aba.t(Token.t, s));

let get = Identified.get;

let s_empty = ([], []);

let s_of_nibs = (nibs) =>
  ([], List.map(nibs => ((nibs, ()), []), nibs));

let s_len = ((hd, tl): s) =>
  tl
  |> List.map(((_hole, tiles)) => 1 + List.length(tiles))
  |> List.fold_left((+), List.length(hd));

module Mold = {
  type tile = t;

  module Sorts = {
    type t = {
      out: Sort.t,
      in: list(Sort.t),
    };

    let of_tokens: list(Token.t) => list(t) =
      fun
      | ["let", "=", "in"] => [{out: Exp, in:  [Pat, Exp]}]
      | _ => [];

    let mk = (~in: list(Sort.t)=[], out: Sort.t) => {out, in};
  };

  module Shape = {
    type t =
      | Op
      | Pre
      | Post
      | Bin;
  };

  type t = {
    precedence: int,
    sorts: Sorts.t,
    shape: Shape.t,
  };

  let op = (sorts) => {
    sorts,
    precedence: 0,
    shape: Shape.Op,
  };
  let pre = (precedence, sorts) => {
    precedence,
    sorts,
    shape: Shape.Pre,
  };

  let nibs = (mold: t): (Nib.t, Nib.t) => {
    let mk = ((l, r)) => Nib.({orientation: l, sort}, {orientation: r, sort});
    mk(
      switch (mold.shape) {
      | Op => (Left, Right)
      | Pre => (Left, Left)
      | Post => (Right, Right)
      | Bin => (Right, Left)
      }
    );
  };
};

let tokens = get(Aba.get_as);

let molds = (tile: t): list(Mold.t) =>
  Mold.(
    switch (tokens(tile)) {
    | [t] when Token.is_num(t) => [op(Sorts.mk(Exp))]
    | [t] when Token.is_var(t) => [op(Sorts.mk(Pat)), op(Sorts.mk(Exp))]
    | ["(", ")"] => [
        op(Sorts.mk(~in=[Pat], Pat)),
        op(Sorts.mk(~in=[Exp], Exp)),
      ]
    | ["λ", "{", "}"] => [op(Sorts.mk(~in=[Pat, Exp], Exp))]
    | ["!"] => [post(1, Sorts.mk(Exp))]
    | ["[", "]"] => [post(2, Sorts.mk(~in=[Exp], Exp))]
    | ["*" | "/"] => [bin(3, Sorts.mk(Exp))]
    | ["+" | "-"] => [bin(4, Sorts.mk(Exp))]
    | [","] => [bin(5, Sorts.mk(Exp)), bin(5, Sorts.mk(Pat))]
    | ["?", ":"] => [bin(6, Sorts.mk(~in=[Exp], Exp))]
    | ["let", "=", "in"] => [pre(9, Sorts.mk(~in=[Pat, Exp], Exp))]
    | _ => []
    }
  );

module Frame = {
  type tiles = s;
  type tile = t;

  type t = Identified.t(Aba.Frame.b(Token.t, tiles));
  type s = ListFrame.t(tile);
};

// let precedence: t => int =
//   Identified.get(
//     fun
//     | Some(("!", [])) => 1
//     | Some(("[", [(_, "]")])) => 2
//     | Some(("*" | "/", [])) => 3
//     | Some(("+" | "-", [])) => 4
//     | Some(())

//     | None => 0
//     | Some(t') =>
//       switch (t') {
//       |
//       }
//     | Some((_, [])) => 0
//     | Fact => 1
//     | Ap(_) => 2
//     | Times
//     | Div => 3
//     | Plus
//     | Minus => 4
//     | BinHole => 5
//     | Prod => 6
//     | Cond(_) => 7
//     | Let(_) => 9,
//   )