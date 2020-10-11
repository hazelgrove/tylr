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

module Exp = {
  let rec view = (~attrs: list(Attr.t)=[], e: HExp.t): Node.t =>
    Node.span(attrs, List.map(view_of_tile, e))
  and view_of_tile = (~attrs: list(Attr.t)=[], tile: HExp.Tile.t): Node.t => {
    let cls =
      switch (tile) {
      | Operand(_) => "Operand"
      | PreOp(_) => "PreOp"
      | PostOp(_) => "PostOp"
      | BinOp(_) => "BinOp"
      };
    let vs =
      switch (tile) {
      | Operand(operand) =>
        switch (operand) {
        | OperandHole => view_of_OperandHole
        | Var(_, x) => view_of_Var(x)
        | Num(_, n) => [Node.text(string_of_int(n))]
        | Paren(body) => view_of_Paren(view(body))
        }
      | PreOp(preop) =>
        switch (preop) {
        | Lam(_, p) => view_of_Lam(Pat.view(p))
        }
      | PostOp(postop) =>
        switch (postop) {
        | Ap(_, arg) => view_of_Ap(view(arg))
        }
      | BinOp(binop) =>
        switch (binop) {
        | OperatorHole => view_of_OperatorHole
        | Plus(_) => view_of_Plus
        }
      };
    Node.span(Attrs.add_class(attrs, cls), vs);
  };

  let view_of_faded_tile = view_of_tile(~attrs=[Attr.classes(["faded"])]);

  let rec view_of_faded =
          (side: [ | `Left | `Right], (steps, j): ZPath.t, e: HExp.t)
          : ZList.t(Node.t, Node.t) =>
    switch (steps) {
    | [] => view(e)
    | [two_step, ...steps] =>
      let (prefix, tile, suffix) =
        switch (ZPath.Exp.unzip(two_step, (e, None))) {
        | `Pat(_) => failwith("expected selection with same sort endpoints")
        | `Exp(e, unzipped) =>
          switch (Option.get(unzipped)) {
          | Operand(ParenZ_body({prefix, suffix, _})) => (
              prefix,
              view_of_Paren(view_of_faded(side, (steps, j), e)),
              suffix,
            )
          | PreOp(_) => raise(ZExp.Void_ZPreOp)
          | PostOp(ApZ_arg(_, {prefix, suffix})) => (
              prefix,
              view_of_Ap(view_of_faded(side, (steps, j), e)),
              suffix,
            )
          | BinOp(_) => raise(ZExp.Void_ZBinOp)
          }
        };
      let (view_of_pre, view_of_suf) =
        switch (side) {
        | `Left => (view_of_faded_tile, view_of_tile)
        | `Right => (view_of_tile, view_of_faded_tile)
        };
      let prefix = List.map(view_of_pre, prefix);
      let suffix = List.map(view_of_suf, suffix);
      {prefix, z: tile, suffix};
    };

  let view_of_restructuring = ((l, r): ZPath.selection, e: HExp.t): Node.t =>
    switch (l, r) {
    | (([], _), ([], _)) => view(e)
    | (([], _), ([_, ..._], _)) =>
      let {prefix, z, suffix} = view_of_faded(`Right, r, e);
      Node.span([], prefix @ [z, ...suffix]);
    | (([_, ..._], _), ([], _)) =>
      let {prefix, z, suffix} = view_of_faded(`Left, l, e);
      Node.span([], prefix @ [z, ...suffix]);
    | (([_, ..._], _), ([_, ..._], _)) =>
      let {prefix, z: z_l, suffix: suffix_l} = view_of_faded(`Left, l, e);
      let {prefix: prefix_r, z: z_r, suffix} = view_of_faded(`Right, r, e);
      let mid =
        ListUtil.split_n(
          List.length(prefix_r) - (List.length(prefix) + 1),
          suffix_l,
        );
      Node.span([], prefix @ [z_l, ...mid] @ [z_r, ...suffix]);
    };
};
