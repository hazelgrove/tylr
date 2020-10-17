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

  let view_of_unzipped = (_, _) => failwith("unimplemented");

  let rec view_of_term = (~font_metrics: FontMetrics.t, e: HExp.t) =>
    List.map(view_of_tile(~font_metrics), e)
  and view_of_tile = (~font_metrics: FontMetrics.t, tile: HExp.Tile.t) => {
    let text = CodeText.Exp.view_of_tile(tile);
    let decoration =
      CodeDecoration.Tile.view(
        ~attrs=[Attr.classes(["exp"])],
        profile_of_tile(tile),
      );
    Node.span([Attr.classes(["tile"])], [text, decoration]);
  };

  /*
   let view_of_ztile =
       (ztile: ZExp.ztile): ZList.t(ZList.t(unit, Node.t), Node.t) => {
     let (prefix, suffix) =
       switch (ztile) {
       | Operand(ParenZ_body({prefix, suffix, _})) =>
         let prefix = List.map(view_of_tile, prefix);
         ();
       | PostOp(ApZ_arg(_, {prefix, suffix, _})) => (prefix, suffix)
       | PreOp(_) => raise(Void_ZPreOp)
       | BinOp(_) => raise(Void_ZBinOp)
       };
     ();
   };
   */

  // render this either directly covering selection or above it
  let view_of_selection =
      (~font_metrics: FontMetrics.t, selection: ZPath.selection, e: HExp.t)
      : Node.t => {
    let rec go = ((l, r), e) =>
      switch (l, r) {
      | (([], j_l), ([], j_r)) =>
        let (_, tiles, _) = ListUtil.split_sublist(j_l, j_r, e);
        List.map(view_of_tile(~font_metrics), tiles);
      | (([], j_l), ([two_step_r, ...steps_r], j_r)) =>
        switch (ZPath.Exp.unzip(two_step_r, (e, None))) {
        | `Exp(e, unzipped) =>
          let (prefix, partial_tile) =
            switch (Option.get(unzipped)) {
            | Operand(ParenZ_body({prefix, _})) => (
                prefix,
                CodeText.[pad, Delim.open_Paren, pad],
              )
            | PreOp(_) => raise(ZExp.Void_ZPreOp)
            | PostOp(ApZ_arg(_, {prefix, _})) => (
                prefix,
                CodeText.[pad, Delim.open_Paren, pad],
              )
            | BinOp(_) => raise(ZExp.Void_ZBinOp)
            };
          let prefix_tiles =
            prefix
            |> ListUtil.split_n(j_l)
            |> snd
            |> List.map(view_of_tile(~font_metrics));
          let rest = go((([], 0), (steps_r, j_r)), e);
          List.concat([prefix_tiles, partial_tile, rest]);
        | `Pat(_) => failwith("unconsidered")
        }
      | (([two_step_l, ...steps_l], j_l), ([], j_r)) =>
        switch (ZPath.Exp.unzip(two_step_l, (e, None))) {
        | `Pat(_) => failwith("unconsidered")
        | `Exp(e, unzipped) =>
          let (partial_tile, suffix) =
            switch (Option.get(unzipped)) {
            | Operand(ParenZ_body({suffix, _})) => (
                CodeText.[pad, Delim.close_Paren, pad],
                suffix,
              )
            | PreOp(_) => raise(ZExp.Void_ZPreOp)
            | PostOp(ApZ_arg(_, {suffix, _})) => (
                CodeText.[pad, Delim.close_Paren, pad],
                suffix,
              )
            | BinOp(_) => raise(ZExp.Void_ZBinOp)
            };
          let suffix_tiles =
            suffix
            |> ListUtil.split_n(j_r - fst(two_step_l) - 1)
            |> fst
            |> List.map(view_of_tile(~font_metrics));
          let rest = go(((steps_l, j_l), ([], List.length(e))), e);
          List.concat([suffix_tiles, partial_tile, rest]);
        }
      | (([two_step_l, ...steps_l], j_l), ([two_step_r, ...steps_r], j_r)) =>
        switch (
          ZPath.Exp.unzip(two_step_l, (e, None)),
          ZPath.Exp.unzip(two_step_r, (e, None)),
        ) {
        | (`Pat(_), _)
        | (_, `Pat(_)) => failwith("unconsidered")
        | (`Exp(e_l, unzipped_l), `Exp(e_r, unzipped_r)) =>
          let mid_tiles =
            e
            |> ListUtil.sublist(fst(two_step_l), fst(two_step_r) + 1)
            |> List.map(view_of_tile(~font_metrics));
          let (partial_l, partial_r) = {
            // TODO
            CodeText.(
              [pad, Delim.close_Paren, pad],
              [pad, Delim.open_Paren, pad],
            );
          };
          let (rest_l, rest_r) = (
            go(((steps_l, j_l), ([], List.length(e_l))), e_l),
            go((([], 0), (steps_r, j_r)), e_r),
          );
          List.concat([rest_l, partial_l, mid_tiles, partial_r, rest_r]);
        }
      };
    // TODO add selection highlight
    Node.span([], go(selection, e));
  };

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
