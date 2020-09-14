open Virtual_dom.Vdom;

let pad = Node.text(Unicode.nbsp);

let view_of_OperandHole = [Node.text(Unicode.nbsp)];
let view_of_Var = x => [Node.text(x)];
let view_of_Paren = body => [
  Node.text("("),
  pad,
  body,
  pad,
  Node.text(")"),
];

let view_of_Lam = p => [
  Node.text(Unicode.lam),
  pad,
  p,
  pad,
  Node.text("."),
];

let view_of_Ann = ann => [Node.text(":"), pad, ann];
let view_of_Ap = arg => [Node.text("("), pad, arg, pad, Node.text(")")];

let view_of_Arrow = [Node.text(Unicode.arrow)];
let view_of_Plus = [Node.text("+")];
let view_of_OperatorHole = [Node.text(Unicode.nbsp)];

let view_of_Operand = (~highlight=None, operand) => {
  let operand =
    switch (highlight) {
    | None => operand
    | Some(_) =>
      // TODO draw operand tile
      operand
    };
  [Node.span([Attr.classes(["Operand"])], operand)];
};
let view_of_PreOp = (~highlight=None, preop, arg) => {
  let preop =
    switch (highlight) {
    | None => preop
    | Some(_) =>
      // TODO draw preop tile
      preop
    };
  [Node.span([Attr.classes(["PreOp"])], preop), arg];
};
let view_of_PostOp = (~highlight=None, arg, postop) => {
  let postop =
    switch (highlight) {
    | None => postop
    | Some(_) =>
      // TODO draw postop tile
      postop
    };
  [arg, pad, Node.span([Attr.classes(["PostOp"])], postop)];
};
let view_of_BinOp = (~highlight=None, l, binop, r) => {
  let binop =
    switch (highlight) {
    | None => binop
    | Some(_) =>
      // TODO draw binop tile
      binop
    };
  [l, Node.span([Attr.classes(["BinOp"])], binop), r];
};

let length =
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
