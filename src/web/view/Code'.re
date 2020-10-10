open Virtual_dom.Vdom;

let tip = 0.4;
let child_border_thickness = 0.1;

let tr_bl = (~child_border: option([ | `North | `South])=?, ()) =>
  SvgUtil.Path.(
    switch (child_border) {
    | None =>
      let diag = L_({dx: Float.neg(tip), dy: 0.5});
      [diag];
    | Some(d) =>
      let t = child_border_thickness /. 0.5;
      let dx = (1. -. t) *. tip;
      let diag = L_({dx: Float.neg(dx), dy: (1. -. t) *. 0.5});
      let border = H_({dx: Float.neg(1. - dx)});
      switch (d) {
      | `North => [border, diag]
      | `South => [diag, border]
      };
    }
  );
let bl_tr = (~child_border: option([ | `North | `South])=?, ()) =>
  SvgUtil.Path.reverse(tr_bl(~child_border?, ()));

let tl_br = (~child_border: option([ | `North | `South])=?, ()) =>
  SvgUtil.Path.(
    switch (child_border) {
    | None =>
      let diag = L_({dx: tip, dy: 0.5});
      [diag];
    | Some(d) =>
      let t = child_border_thickness /. 0.5;
      let dx = (1. -. t) *. tip;
      let diag = L_({dx, dy: (1. -. t) *. 0.5});
      let border = H_({dx: 1. - dx});
      switch (d) {
      | `North => [border, diag]
      | `South => [diag, border]
      };
    }
  );
let br_tl = (~child_border: option([ | `North | `South])=?, ()) =>
  SvgUtil.Path.reverse(tl_br(~child_border?, ()));

let closed_child_paths =
  List.map(((start, len)) =>
    SvgUtil.Path.[
      M({x: Float.of_int(start), y: child_border_thickness}),
      tr_bl(~child_border=`North, ()),
      tl_br(~child_border=`South, ()),
      H_({dx: Float.of_int(len)}),
      bl_tr(~child_border=`South, ()),
      br_tl(~child_border=`North, ()),
      Z,
    ]
  );

let outer_path =
    (
      open_children: list((int, int)),
      len: int,
      shape: [ | `Operand | `PreOp | `PostOp | `BinOp],
    )
    : SvgUtil.Path.t => {
  open SvgUtil.Path;
  let (left_tip, right_tip) =
    switch (shape) {
    | `Operand => ([br_tl(), bl_tr()], [tl_br(), tr_bl()])
    | `PreOp => ([br_tl(), bl_tr()], [tr_bl(), tl_br()])
    | `PostOp => ([bl_tr(), br_tl()], [tl_br(), tr_bl()])
    | `BinOp => ([bl_tr(), br_tl()], [tr_bl(), tl_br()])
    };
  let left_and_top_sides =
    open_children
    |> List.map(((start, len)) =>
         [
           H({x: Float.of_int(start) -. 1. +. tip}),
           tr_bl(),
           tl_br(~child_border=`South, ()),
           H_({dx: Float.of_int(len)}),
           bl_tr(~child_border=`South, ()),
           br_tl(),
         ]
       )
    |> List.cons([M({x: 0., y: 1.}), ...left_tip])
    |> List.flatten;
  List.concat([
    left_and_top_sides,
    [H({x: Float.of_int(len)}), tl_br(), tr_bl()],
    [Z],
  ]);
};

let view_of_tile =
    (
      ~attrs: list(Attr.t)=[],
      ~closed_children: list((int, int))=[],
      ~open_children: list((int, int))=[],
      ~len: int,
      ~shape: [ | `Operand | `PreOp | `PostOp | `BinOp],
    )
    : Node.t => {
  let closed_child_paths = closed_child_paths(closed_children);
  let outer_path = outer_path(open_children, len, shape);
  let path = outer_path @ List.concat(closed_child_paths);
  view(~attrs, path);
};

module Exp: {
  let view_of_unzipped: (option(ZExp.ztile), Node.t) => Node.t;
  let view_of_zipped: (EditState.Mode.t, HExp.t) => Node.t;
} = {
  let rec length = (e: HExp.t) =>
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
    |> List.fold_left((+), -1)
  and length_of_operand =
    fun
    | OperandHole => 1
    | Var(_, x) => String.length(x)
    | Num(_, n) => String.length(string_of_int(n))
    | Paren(body) => 4 + length(body)
  and length_of_preop =
    fun
    | Lam(_, p) => 4 + Code_Pat.length(p)
  and length_of_postop =
    fun
    | Ap(_, arg) => 4 + length(arg)
  and length_of_binop =
    fun
    | OperatorHole
    | Plus(_) => 1;

  let view_of_tile =
      (
        ~decorate: option([ | `ShowChild | `HideChild])=?,
        ~font_metrics: FontMetrics.t,
        tile: HExp.Tile.t,
      ) =>
    failwith("unimplemented");

  let view_of_term = List.map(view_of_tile);

  let rec view_of_zipped =
          (~font_metrics: FontMetrics.t, mode: EditState.Mode.t, e: HExp.t)
          : Node.t =>
    switch (mode) {
    | Normal(([], j)) =>
      let {prefix, z: root, suffix} = HExp.nth_root(j, e);
      // compute length of root to draw cursor inspector
      // compute length of prefix prior to jth tile to draw caret
      failwith("unimplemented");
    | Normal(([two_step, ...steps], j)) =>
      let mode = EditState.Mode.Normal((steps, j));
      switch (Exp.unzip(two_step, (e, None))) {
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
