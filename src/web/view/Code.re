open Virtual_dom.Vdom;

let view_of_OperandHole = [Node.text(Unicode.nbsp)];

let view_of_Var = x => [Node.text(x)];

let view_of_Paren = body => [Node.text("("), body, Node.text(")")];

let view_of_Ann = (subj, ann) => [subj, Node.text(" : "), ann];

let view_of_OperatorHole = (l, r) => [l, Node.text(" __ "), r];

let view_of_Operand = Node.span([Attr.classes(["Operand"])]);
let view_of_PreOp = Node.span([Attr.classes(["PreOp"])]);
let view_of_PostOp = Node.span([Attr.classes(["PostOp"])]);
let view_of_BinOp = Node.span([Attr.classes(["BinOp"])]);

let length_of_tiles =
    (
      length_of_operand,
      length_of_preop,
      length_of_postop,
      length_of_binop,
      tiles,
    )
    : int =>
  tiles
  |> List.map(
       Tile.map(
         length_of_operand,
         length_of_preop,
         length_of_postop,
         length_of_binop,
       ),
     )
  |> List.map((+)(1))
  |> List.fold_left((+), -1);

module Typ = {
  let rec length = (ty: HTyp.t) =>
    length_of_tiles(
      length_of_operand,
      length_of_preop,
      length_of_postop,
      length_of_binop,
      ty,
    )
  and length_of_operand =
    fun
    | OperandHole => 1
    | Num => 3
    | Paren(body) => 4 + length(body)
  and length_of_preop =
    fun
    | _ => raise(HTyp.Tile.Void_PreOp)
  and length_of_postop =
    fun
    | _ => raise(HTyp.Tile.Void_PostOp)
  and length_of_binop =
    fun
    | OperatorHole
    | Arrow => 1;
};

module Pat = {
  let rec length = (p: HPat.t) =>
    length_of_tiles(
      length_of_operand,
      length_of_preop,
      length_of_postop,
      length_of_binop,
      p,
    )
  and length_of_operand =
    fun
    | OperandHole => 1
    | Var(x) => String.length(x)
    | Paren(body) => 4 + length(body)
  and length_of_preop =
    fun
    | _ => raise(HPat.Tile.Void_PreOp)
  and length_of_postop =
    fun
    | Ann(_, ann) => 2 + Typ.length(ann)
  and length_of_binop =
    fun
    | OperatorHole => 1;
  /*
   let rec view = (p: HPat.t) =>
     switch (HPat.root(p)) {
     | Operand(operand) =>
       let text =
         switch (operand) {
         | OperandHole => view_of_OperandHole
         | Var(x) => view_of_Var(x)
         | Paren(body) => view_of_Paren(view(body))
         };
       view_of_Operand(text);
     | PreOp(_) => raise(HPat.Tile.Void_PreOp)
     | PostOp(subj, Ann(_, ann)) =>
       let text = view_of_Ann(view(subj), Typ.view(ann));
       view_of_PostOp(text);
     | BinOp(l, OperatorHole, r) =>
       let text = view_of_OperatorHole(l, r);
       view_of_BinOp(text);
     };
   */
};

module Exp = {
  let rec length = (e: HExp.t) =>
    length_of_tiles(
      length_of_operand,
      length_of_preop,
      length_of_postop,
      length_of_binop,
      e,
    )
  and length_of_operand =
    fun
    | OperandHole => 1
    | Var(_, x) => String.length(x)
    | Num(_, n) => String.length(string_of_int(n))
    | Paren(body) => 4 + length(body)
  and length_of_preop =
    fun
    | Lam(_, p) => 4 + Pat.length(p)
  and length_of_postop =
    fun
    | Ap(_, arg) => 4 + length(arg)
  and length_of_binop =
    fun
    | OperatorHole
    | Plus(_) => 1;
  /*
   let rec view = (e: HExp.t) =>
       switch (HExp.root(e)) {
       | Operand(operand) =>
         let text =
           switch (operand) {
           | OperandHole => view_of_OperandHole
           | Var(_, x) => view_of_Var(x)
           | Num(_, n) => [Node.text(string_of_int(n))]
           | Paren(body) => view_of_Paren(view(body))
           };
         view_of_Operand(text);
       | PreOp(Lam(_, p), body) =>
         let text = view_of_Lam(Pat.view(p), view(body));

         Node.span([], [
           Node.text(Unicode.lam),
           Pat.view(p),
           Node.text("."),
           view(body),
         ])
       | PostOp(fn, Ap(_, arg)) =>
         Node.span([], [
           view(fn),
           Node.text("("),
           view(arg),
           Node.text(")"),
         ])
       | BinOp(l, binop, r) =>
         let binop_view =
           switch (binop) {
           | Plus(_) => Node.text("+")
           | OperatorHole => Node.text("__")
           };
         Node.span([], [
           view(l),
           binop_view,
           view(r),
         ]);
       }

   let view_z = (~inject, ze: ZExp.t) => Vdom.Node.div([], []);
   */
};

let view = (~inject as _, _) => Node.div([], []);
