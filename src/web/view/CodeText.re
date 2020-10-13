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

module Exp = {
  let rec view = (~attrs: Attrs.t=[], e: HExp.t): Node.t =>
    Node.span(attrs, List.map(view_of_tile, e))
  and view_of_tile = (~attrs: Attrs.t=[], tile: HExp.Tile.t): Node.t => {
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

  let rec view_with_faded_affix =
          (
            faded_affix: [ | `Prefix | `Suffix],
            (steps, j): ZPath.t,
            e: HExp.t,
          )
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
              view_of_Paren(view_with_faded_affix(side, (steps, j), e)),
              suffix,
            )
          | PreOp(_) => raise(ZExp.Void_ZPreOp)
          | PostOp(ApZ_arg(_, {prefix, suffix, _})) => (
              prefix,
              view_of_Ap(view_with_faded_affix(side, (steps, j), e)),
              suffix,
            )
          | BinOp(_) => raise(ZExp.Void_ZBinOp)
          }
        };
      let (view_of_pre, view_of_suf) =
        switch (faded_affix) {
        | `Prefix => (view_of_faded_tile, view_of_tile)
        | `Suffix => (view_of_tile, view_of_faded_tile)
        };
      let prefix = List.map(view_of_pre, prefix);
      let suffix = List.map(view_of_suf, suffix);
      {prefix, z: tile, suffix};
    };
  let view_with_faded_prefix = view_with_faded_affix(`Prefix);
  let view_with_faded_suffix = view_with_faded_affix(`Suffix);

  let view_of_restructuring = ((l, r): ZPath.selection, e: HExp.t): Node.t =>
    switch (l, r) {
    | (([], _), ([], _)) => view(e)
    | (([], _), ([_, ..._], _)) =>
      let {prefix, z, suffix} = view_with_faded_suffix(r, e);
      Node.span([], prefix @ [z, ...suffix]);
    | (([_, ..._], _), ([], _)) =>
      let {prefix, z, suffix} = view_with_faded_prefix(l, e);
      Node.span([], prefix @ [z, ...suffix]);
    | (([_, ..._], _), ([_, ..._], _)) =>
      let {prefix, z: z_l, suffix: suffix_l} = view_with_faded_prefix(l, e);
      let {prefix: prefix_r, z: z_r, suffix} = view_with_faded_suffix(r, e);
      let mid =
        ListUtil.split_n(
          List.length(prefix_r) - (List.length(prefix) + 1),
          suffix_l,
        );
      Node.span([], prefix @ [z_l, ...mid] @ [z_r, ...suffix]);
    };
};
