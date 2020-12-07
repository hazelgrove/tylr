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
let of_Ann = (Node.text(":"), Node.text(Unicode.nbsp));
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

module type TYP = {include COMMON with module T := HTyp.Tile;};
module rec Typ: TYP = {
  let view_of_tile = (tile: HTyp.Tile.t): Node.t => {
    let vs =
      switch (tile) {
      | Operand(OperandHole) => [of_OperandHole]
      | Operand(Num) => [Node.text("Num")]
      | Operand(Paren(body)) =>
        let (open_, close) = of_Paren;
        [open_, Typ.view(body), close];
      | PreOp () => raise(HTyp.Tile.Void_PreOp)
      | PostOp () => raise(HTyp.Tile.Void_PostOp)
      | BinOp(OperatorHole) => [of_OperatorHole]
      | BinOp(Arrow) => [of_Arrow]
      };
    Node.span([Attr.classes(["code-text"])], space(vs));
  };
  include Make(HTyp.Tile, Typ);
};

module type PAT = {include COMMON with module T := HPat.Tile;};
module rec Pat: PAT = {
  let view_of_tile = (tile: HPat.Tile.t): Node.t => {
    let vs =
      switch (tile) {
      | Operand(OperandHole) => [of_OperandHole]
      | Operand(Var(x)) => [of_Var(x)]
      | Operand(Paren(body)) =>
        let (open_, close) = of_Paren;
        [open_, Pat.view(body), close];
      | PreOp () => raise(HPat.Tile.Void_PreOp)
      | PostOp(Ann(_, ann)) =>
        let (open_, close) = of_Ann;
        [open_, Typ.view(ann), close];
      | BinOp(OperatorHole) => [of_OperatorHole]
      };
    Node.span([Attr.classes(["code-text"])], space(vs));
  };
  include Make(HPat.Tile, Pat);
};

module type EXP = {include COMMON with module T := HExp.Tile;};
module rec Exp: EXP = {
  let view_of_tile = (tile: HExp.Tile.t): Node.t => {
    let vs =
      switch (tile) {
      | Operand(OperandHole) => [of_OperandHole]
      | Operand(Var(_, x)) => [of_Var(x)]
      | Operand(Num(_, n)) => [of_NumLit(n)]
      | Operand(Paren(body)) =>
        let (open_, close) = of_Paren;
        [open_, Exp.view(body), close];
      | PreOp(Lam(_, p)) =>
        let (lam, dot) = of_Lam;
        [lam, Pat.view(p), dot];
      | PostOp(Ap(_, arg)) =>
        let (open_, close) = of_Ap;
        [open_, Exp.view(arg), close];
      | BinOp(OperatorHole) => [of_OperatorHole]
      | BinOp(Plus(_)) => [of_Plus]
      };
    Node.span([Attr.classes(["code-text"])], space(vs));
  };
  include Make(HExp.Tile, Exp);
};
