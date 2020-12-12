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
let of_Let = (Node.text("let"), Node.text("="), Node.text("in"));

// postop
let of_Ann = (Node.text(":"), Node.text(""));
let of_Ap = of_Paren;

// binop
let of_Arrow = Node.text(Unicode.arrow);
let of_Plus = Node.text("+");
let of_OperatorHole = Node.text(Unicode.nbsp);

module type COMMON = {
  module T: Tile.S;
  let view_of_tile: T.t => Node.t;
  let view: T.s => Node.t;
};

module Make = (T: Tile.S, Sort_specific: {let view_of_tile: T.t => Node.t;}) => {
  let view = (ts: T.s): Node.t => {
    let tiles = List.map(Sort_specific.view_of_tile, ts);
    Node.span([], space(tiles));
  };
};

module type TYP = {include COMMON with module T := HTyp.T;};
module rec Typ: TYP = {
  let view_of_tile = (tile: HTyp.T.t): Node.t => {
    let vs =
      switch (tile) {
      | Operand(OperandHole) => [of_OperandHole]
      | Operand(Num) => [Node.text("Num")]
      | Operand(Paren(body)) =>
        let (open_, close) = of_Paren;
        [open_, Typ.view(body), close];
      | PreOp () => raise(HTyp.T.Void_PreOp)
      | PostOp () => raise(HTyp.T.Void_PostOp)
      | BinOp(OperatorHole) => [of_OperatorHole]
      | BinOp(Arrow) => [of_Arrow]
      };
    Node.span([Attr.classes(["code-text"])], space(vs));
  };
  include Make(HTyp.T, Typ);
};

module type PAT = {include COMMON with module T := HPat.T;};
module rec Pat: PAT = {
  let view_of_tile = (tile: HPat.T.t): Node.t => {
    let vs =
      switch (tile) {
      | Operand(OperandHole) => [of_OperandHole]
      | Operand(Var(x)) => [of_Var(x)]
      | Operand(Paren(body)) =>
        let (open_, close) = of_Paren;
        [open_, Pat.view(body), close];
      | PreOp () => raise(HPat.T.Void_PreOp)
      | PostOp(Ann(_, ann)) =>
        let (open_, close) = of_Ann;
        [open_, Typ.view(ann), close];
      | BinOp(OperatorHole) => [of_OperatorHole]
      };
    Node.span([Attr.classes(["code-text"])], space(vs));
  };
  include Make(HPat.T, Pat);
};

module type EXP = {include COMMON with module T := HExp.T;};
module rec Exp: EXP = {
  open HExp.T;
  let view_of_tile = (tile: HExp.T.t): Node.t => {
    let op =
      fun
      | OperandHole => [of_OperandHole]
      | Var(_, x) => [of_Var(x)]
      | Num(_, n) => [of_NumLit(n)]
      | Paren(body) => {
          let (open_, close) = of_Paren;
          [open_, Exp.view(body), close];
        };
    let pre =
      fun
      | Lam(_, p) => {
          let (lam, dot) = of_Lam;
          [lam, Pat.view(p), dot];
        }
      | Let(p, def) => {
          let (let_, eq, in_) = of_Let;
          [let_, Pat.view(p), eq, Exp.view(def), in_];
        };
    let post =
      fun
      | Ap(_, arg) => {
          let (open_, close) = of_Ap;
          [open_, Exp.view(arg), close];
        };
    let bin =
      fun
      | OperatorHole => [of_OperatorHole]
      | Plus(_) => [of_Plus];
    let tokens = Tile.get(op, pre, post, bin, tile);
    Node.span([Attr.classes(["code-text"])], space(tokens));
  };
  include Make(HExp.T, Exp);
};
