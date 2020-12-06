open Virtual_dom.Vdom;
open Util;
open Core;

let space = ListUtil.join(Node.text(Unicode.nbsp));

// operand
let of_OperandHole = Node.text(Unicode.nbsp);
let of_Var = x => Node.text(x);
let of_NumLit = n => Node.text(string_of_int(n));
let of_Paren = (Node.text("("), Node.text(")"));

// preop
let of_Lam = (Node.text(Unicode.lam), Node.text("."));

// postop
let of_Ap = of_Paren;

// binop
let of_Arrow = Node.text(Unicode.arrow);
let of_Plus = Node.text("+");
let of_OperatorHole = Node.text(Unicode.nbsp);

module Typ = {
  let view_of_tile = _ => failwith("Text.Typ.view_of_tile");
};

module Pat = {
  let view_of_tile = _ => failwith("Text.Pat.view_of_tile");
  let view = _ => failwith("Text.Pat.view");
};

module Exp = {
  let rec view = (e: HExp.t): Node.t => {
    let tiles = List.map(view_of_tile, e);
    Node.span([], space(tiles));
  }
  and view_of_tile = (tile: HExp.Tile.t): Node.t => {
    let vs =
      switch (tile) {
      | Operand(OperandHole) => [of_OperandHole]
      | Operand(Var(_, x)) => [of_Var(x)]
      | Operand(Num(_, n)) => [of_NumLit(n)]
      | Operand(Paren(body)) =>
        let (open_, close) = of_Paren;
        [open_, view(body), close];
      | PreOp(Lam(_, p)) =>
        let (lam, dot) = of_Lam;
        [lam, Pat.view(p), dot];
      | PostOp(Ap(_, arg)) =>
        let (open_, close) = of_Ap;
        [open_, view(arg), close];
      | BinOp(OperatorHole) => [of_OperatorHole]
      | BinOp(Plus(_)) => [of_Plus]
      };
    Node.span([Attr.classes(["code-text"])], space(vs));
  };
};
