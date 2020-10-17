open Virtual_dom.Vdom;

module Pat = {
  let length = _ => failwith("unimplemented");
  let view_of_unzipped = (_, _) => failwith("unimplemented");
  let view_of_zipped = (~font_metrics as _, _, _) =>
    failwith("unimplemented");
};

module Exp = {
  let rec length = (e: HExp.t) =>
    e
    |> List.map(
         Tile.map(
           length_of_operand,
           length_of_preop,
           length_of_postop,
           length_of_binop,
         ),
       )
    |> List.map((+)(1))
    |> List.fold_left((+), -1)
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

  let profile_of_tile = (tile: HExp.Tile.t): CodeDecoration.Tile.profile => {
    switch (tile) {
    | Operand(operand) =>
      let (open_children, closed_children, len) =
        switch (operand) {
        | OperandHole
        | Var(_)
        | Num(_) => ([], [], length_of_operand(operand))
        | Paren(body) =>
          let body_len = length(body);
          ([(2, body_len)], [], 2 + body_len + 2);
        };
      {shape: `Operand, len, open_children, closed_children};
    | PreOp(preop) =>
      let (open_children, closed_children, len) =
        switch (preop) {
        | Lam(_, p) =>
          let p_len = Pat.length(p);
          ([], [(2, p_len)], 2 + p_len + 2);
        };
      {shape: `PreOp, len, open_children, closed_children};
    | PostOp(postop) =>
      let (open_children, closed_children, len) =
        switch (postop) {
        | Ap(_, arg) =>
          let arg_len = length(arg);
          ([], [(2, arg_len)], 2 + arg_len + 2);
        };
      {shape: `PostOp, len, open_children, closed_children};
    | BinOp(binop) =>
      let (open_children, closed_children, len) =
        switch (binop) {
        | Plus(_)
        | OperatorHole => ([], [], 1)
        };
      {shape: `BinOp, len, open_children, closed_children};
    };
  };

  let view_of_term = List.map(view_of_tile);

  let rec view_of_zipped =
          (~font_metrics: FontMetrics.t, mode: EditState.Mode.t, e: HExp.t)
          : Node.t =>
    switch (mode) {
    | Normal(([], j)) =>
      let ZList.{prefix, z: root, suffix} = HExp.nth_root(j, e);
      // compute length of root to draw cursor inspector
      // compute length of prefix prior to jth tile to draw caret
      failwith("unimplemented");
    | Normal(([two_step, ...steps], j)) =>
      let mode = EditState.Mode.Normal((steps, j));
      switch (ZPath.Exp.unzip(two_step, (e, None))) {
      | `Pat(p, unzipped) =>
        Pat.view_of_unzipped(unzipped, Pat.view_of_zipped(mode, p))
      | `Exp(e, unzipped) =>
        view_of_unzipped(unzipped, view_of_zipped(mode, e))
      };
    | Selecting((anchor, focus)) =>
      let caret =
        view_of_selecting_caret(offset(anchor, e), offset(focus, e));
      let term = view_of_term(e);
      Node.span([], [caret, term]);
    | Restructuring((l, r) as selection, focus) =>
      let placeholder =
        view_of_restructuring_placeholder(offset(l, e), offset(r, e));
      let caret =
        view_of_restructuring_caret(
          offset(focus, e),
          text_of_selection(selection, e),
        );
      let term = view_of_term(e);
      Node.span([], [placeholder, caret, term]);
    };
};
