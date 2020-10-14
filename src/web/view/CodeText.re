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

let tile_cls: Tile.t(_) => string =
  fun
  | Operand(_) => "Operand"
  | PreOp(_) => "PreOp"
  | PostOp(_) => "PostOp"
  | BinOp(_) => "BinOp";

module Typ = {
  let rec view = (~attrs: Attrs.t=[], ty: HTyp.t): Node.t =>
    Node.span(attrs, List.map(view_of_tile, ty))
  and view_of_tile = (~attrs: Attrs.t=[], tile: HTyp.Tile.t): Node.t => {
    let vs =
      switch (tile) {
      | Operand(operand) =>
        switch (operand) {
        | OperandHole => view_of_OperandHole
        | Num => [Node.text("Num")]
        | Paren(body) => view_of_Paren(view(body))
        }
      | PreOp(_) => raise(HTyp.Tile.Void_PreOp)
      | PostOp(_) => raise(HTyp.Tile.Void_PostOp)
      | BinOp(binop) =>
        switch (binop) {
        | OperatorHole => view_of_OperatorHole
        | Arrow => view_of_Arrow
        }
      };
    Node.span(Attrs.add_class(attrs, tile_cls(tile)), vs);
  };
  let view_of_faded_tile = view_of_tile(~attrs=[Attr.classes(["faded"])]);

  let rec view_with_faded_affix =
          (
            faded_affix: [ | `Prefix | `Suffix],
            two_step: ZPath.two_step,
            path: ZPath.t,
            ty: HTyp.t,
          )
          : ZList.t(Node.t, Node.t) => {
    let (prefix, tile, suffix) =
      switch (ZPath.Typ.unzip(two_step, (ty, None))) {
      | `Typ(ty, unzipped) =>
        switch (Option.get(unzipped)) {
        | Operand(ParenZ_body({prefix, suffix, _})) =>
          let body =
            switch (path) {
            | ([], _) => view(ty)
            | ([two_step, ...steps], j) =>
              let ZList.{prefix, z, suffix} =
                view_with_faded_affix(faded_affix, two_step, (steps, j), ty);
              Node.span([], prefix @ [z, ...suffix]);
            };
          let tile =
            Node.span([Attr.classes(["Operand"])], view_of_Paren(body));
          (prefix, tile, suffix);
        | PreOp(_) => raise(ZTyp.Void_ZPreOp)
        | PostOp(_) => failwith("expected selection with same sort endpoints")
        | BinOp(_) => raise(ZTyp.Void_ZBinOp)
        }
      };
    let (view_of_pre, view_of_suf) =
      switch (faded_affix) {
      | `Prefix => (view_of_faded_tile, view_of_tile(~attrs=[]))
      | `Suffix => (view_of_tile(~attrs=[]), view_of_faded_tile)
      };
    let prefix = List.map(view_of_pre, prefix);
    let suffix = List.map(view_of_suf, suffix);
    {prefix, z: tile, suffix};
  };
  let view_with_faded_prefix = view_with_faded_affix(`Prefix);
  let view_with_faded_suffix = view_with_faded_affix(`Suffix);

  let view_of_restructuring = ((l, r): ZPath.selection, ty: HTyp.t): Node.t =>
    switch (l, r) {
    | (([], _), ([], _)) => view(ty)
    | (([], _), ([two_step, ...steps], j)) =>
      let ZList.{prefix, z, suffix} =
        view_with_faded_suffix(two_step, (steps, j), ty);
      Node.span([], prefix @ [z, ...suffix]);
    | (([two_step, ...steps], j), ([], _)) =>
      let ZList.{prefix, z, suffix} =
        view_with_faded_prefix(two_step, (steps, j), ty);
      Node.span([], prefix @ [z, ...suffix]);
    | (([two_step_l, ...steps_l], j_l), ([two_step_r, ...steps_r], j_r)) =>
      let ZList.{prefix, z: z_l, suffix: suffix_l} =
        view_with_faded_prefix(two_step_l, (steps_l, j_l), ty);
      let ZList.{prefix: prefix_r, z: z_r, suffix} =
        view_with_faded_suffix(two_step_r, (steps_r, j_r), ty);
      let (mid, _) =
        ListUtil.split_n(
          List.length(prefix_r) - (List.length(prefix) + 1),
          suffix_l,
        );
      Node.span([], prefix @ [z_l, ...mid] @ [z_r, ...suffix]);
    };
};

module Pat = {
  let rec view = (~attrs: Attrs.t=[], p: HPat.t): Node.t =>
    Node.span(attrs, List.map(view_of_tile, p))
  and view_of_tile = (~attrs: Attrs.t=[], tile: HPat.Tile.t): Node.t => {
    let vs =
      switch (tile) {
      | Operand(operand) =>
        switch (operand) {
        | OperandHole => view_of_OperandHole
        | Var(x) => view_of_Var(x)
        | Paren(body) => view_of_Paren(view(body))
        }
      | PreOp(_) => raise(HPat.Tile.Void_PreOp)
      | PostOp(postop) =>
        switch (postop) {
        | Ann(_, ann) => view_of_Ann(Typ.view(ann))
        }
      | BinOp(binop) =>
        switch (binop) {
        | OperatorHole => view_of_OperatorHole
        }
      };
    Node.span(Attrs.add_class(attrs, tile_cls(tile)), vs);
  };
  let view_of_faded_tile = view_of_tile(~attrs=[Attr.classes(["faded"])]);

  let rec view_with_faded_affix =
          (
            faded_affix: [ | `Prefix | `Suffix],
            two_step: ZPath.two_step,
            path: ZPath.t,
            p: HPat.t,
          )
          : ZList.t(Node.t, Node.t) => {
    let (prefix, tile, suffix) =
      switch (ZPath.Pat.unzip(two_step, (p, None))) {
      | `Typ(_) => failwith("expected selection with same sort endpoints")
      | `Pat(p, unzipped) =>
        switch (Option.get(unzipped)) {
        | Operand(ParenZ_body({prefix, suffix, _})) =>
          let body =
            switch (path) {
            | ([], _) => view(p)
            | ([two_step, ...steps], j) =>
              let ZList.{prefix, z, suffix} =
                view_with_faded_affix(faded_affix, two_step, (steps, j), p);
              Node.span([], prefix @ [z, ...suffix]);
            };
          let tile =
            Node.span([Attr.classes(["Operand"])], view_of_Paren(body));
          (prefix, tile, suffix);
        | PreOp(_) => failwith("expected selection with same sort endpoints")
        | PostOp(_) => raise(ZPat.Void_ZPostOp)
        | BinOp(_) => raise(ZPat.Void_ZBinOp)
        }
      };
    let (view_of_pre, view_of_suf) =
      switch (faded_affix) {
      | `Prefix => (view_of_faded_tile, view_of_tile(~attrs=[]))
      | `Suffix => (view_of_tile(~attrs=[]), view_of_faded_tile)
      };
    let prefix = List.map(view_of_pre, prefix);
    let suffix = List.map(view_of_suf, suffix);
    {prefix, z: tile, suffix};
  };
  let view_with_faded_prefix = view_with_faded_affix(`Prefix);
  let view_with_faded_suffix = view_with_faded_affix(`Suffix);

  let view_of_restructuring = ((l, r): ZPath.selection, p: HPat.t): Node.t =>
    switch (l, r) {
    | (([], _), ([], _)) => view(p)
    | (([], _), ([two_step_r, ...steps_r], j_r)) =>
      let ZList.{prefix, z, suffix} =
        view_with_faded_suffix(two_step_r, (steps_r, j_r), p);
      Node.span([], prefix @ [z, ...suffix]);
    | (([two_step_l, ...steps_l], j_l), ([], _)) =>
      let ZList.{prefix, z, suffix} =
        view_with_faded_prefix(two_step_l, (steps_l, j_l), p);
      Node.span([], prefix @ [z, ...suffix]);
    | (([two_step_l, ...steps_l], j_l), ([two_step_r, ...steps_r], j_r)) =>
      let ZList.{prefix, z: z_l, suffix: suffix_l} =
        view_with_faded_prefix(two_step_l, (steps_l, j_l), p);
      let ZList.{prefix: prefix_r, z: z_r, suffix} =
        view_with_faded_suffix(two_step_r, (steps_r, j_r), p);
      let (mid, _) =
        ListUtil.split_n(
          List.length(prefix_r) - (List.length(prefix) + 1),
          suffix_l,
        );
      Node.span([], prefix @ [z_l, ...mid] @ [z_r, ...suffix]);
    };
};

module Exp = {
  let rec view = (~attrs: Attrs.t=[], e: HExp.t): Node.t =>
    Node.span(attrs, List.map(view_of_tile, e))
  and view_of_tile = (~attrs: Attrs.t=[], tile: HExp.Tile.t): Node.t => {
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
    Node.span(Attrs.add_class(attrs, tile_cls(tile)), vs);
  };
  let view_of_faded_tile = view_of_tile(~attrs=[Attr.classes(["faded"])]);

  let rec view_with_faded_affix =
          (
            faded_affix: [ | `Prefix | `Suffix],
            two_step: ZPath.two_step,
            path: ZPath.t,
            e: HExp.t,
          )
          : ZList.t(Node.t, Node.t) => {
    let (prefix, tile, suffix) =
      switch (ZPath.Exp.unzip(two_step, (e, None))) {
      | `Pat(_) => failwith("expected selection with same sort endpoints")
      | `Exp(e, unzipped) =>
        switch (Option.get(unzipped)) {
        | Operand(ParenZ_body({prefix, suffix, _})) =>
          let body =
            switch (path) {
            | ([], _) => view(e)
            | ([two_step, ...steps], j) =>
              let ZList.{prefix, z, suffix} =
                view_with_faded_affix(faded_affix, two_step, (steps, j), e);
              Node.span([], prefix @ [z, ...suffix]);
            };
          let tile =
            Node.span([Attr.classes(["Operand"])], view_of_Paren(body));
          (prefix, tile, suffix);
        | PreOp(_) => raise(ZExp.Void_ZPreOp)
        | PostOp(ApZ_arg(_, {prefix, suffix, _})) =>
          let arg =
            switch (path) {
            | ([], _) => view(e)
            | ([two_step, ...steps], j) =>
              let ZList.{prefix, z, suffix} =
                view_with_faded_affix(faded_affix, two_step, (steps, j), e);
              Node.span([], prefix @ [z, ...suffix]);
            };
          let tile =
            Node.span([Attr.classes(["PostOp"])], view_of_Ap(arg));
          (prefix, tile, suffix);
        | BinOp(_) => raise(ZExp.Void_ZBinOp)
        }
      };
    let (view_of_pre, view_of_suf) =
      switch (faded_affix) {
      | `Prefix => (view_of_faded_tile, view_of_tile(~attrs=[]))
      | `Suffix => (view_of_tile(~attrs=[]), view_of_faded_tile)
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
    | (([], _), ([two_step_r, ...steps_r], j_r)) =>
      let ZList.{prefix, z, suffix} =
        view_with_faded_suffix(two_step_r, (steps_r, j_r), e);
      Node.span([], prefix @ [z, ...suffix]);
    | (([two_step_l, ...steps_l], j_l), ([], _)) =>
      let ZList.{prefix, z, suffix} =
        view_with_faded_prefix(two_step_l, (steps_l, j_l), e);
      Node.span([], prefix @ [z, ...suffix]);
    | (([two_step_l, ...steps_l], j_l), ([two_step_r, ...steps_r], j_r)) =>
      let ZList.{prefix, z: z_l, suffix: suffix_l} =
        view_with_faded_prefix(two_step_l, (steps_l, j_l), e);
      let ZList.{prefix: prefix_r, z: z_r, suffix} =
        view_with_faded_suffix(two_step_r, (steps_r, j_r), e);
      let (mid, _) =
        ListUtil.split_n(
          List.length(prefix_r) - (List.length(prefix) + 1),
          suffix_l,
        );
      Node.span([], prefix @ [z_l, ...mid] @ [z_r, ...suffix]);
    };
};
