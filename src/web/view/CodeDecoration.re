open Virtual_dom.Vdom;

module Tile = {
  type profile = {
    shape: [ | `Operand(bool) | `PreOp | `PostOp | `BinOp(bool)],
    len: int,
    open_children: list((int, int)),
    closed_children: list((int, int)),
  };

  let tip = 0.3;
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
        let border = H_({dx: Float.neg(1. -. dx)});
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
        let border = H_({dx: 1. -. dx});
        switch (d) {
        | `North => [border, diag]
        | `South => [diag, border]
        };
      }
    );
  let br_tl = (~child_border: option([ | `North | `South])=?, ()) =>
    SvgUtil.Path.reverse(tl_br(~child_border?, ()));

  let contour_path =
      (
        ~sort: Sort.t,
        ~hole_radii as (rx, ry): (float, float),
        ~attrs: list(Attr.t),
        profile: profile,
      )
      : Node.t => {
    open SvgUtil.Path;
    let hole_path =
      switch (profile.shape) {
      | `Operand(true)
      | `BinOp(true) => [
          M({x: 0.5 -. rx, y: 0.5}),
          A_({
            rx,
            ry,
            x_axis_rotation: 0.,
            large_arc_flag: false,
            sweep_flag: false,
            dx: 2. *. rx,
            dy: 0.,
          }),
          A_({
            rx,
            ry,
            x_axis_rotation: 0.,
            large_arc_flag: false,
            sweep_flag: false,
            dx: Float.neg(2. *. rx),
            dy: 0.,
          }),
        ]
      | _ => []
      };
    let closed_child_paths =
      profile.closed_children
      |> List.map(((start, len)) =>
           List.concat([
             [M({x: Float.of_int(start), y: child_border_thickness})],
             tr_bl(~child_border=`North, ()),
             tl_br(~child_border=`South, ()),
             [H_({dx: Float.of_int(len)})],
             bl_tr(~child_border=`South, ()),
             br_tl(~child_border=`North, ()),
             [Z],
           ])
         );
    let outer_path = {
      let (left_tip, right_tip) =
        switch (profile.shape) {
        | `Operand(_) => (br_tl() @ bl_tr(), tl_br() @ tr_bl())
        | `PreOp => (
            br_tl() @ bl_tr(),
            [H_({dx: tip}), ...tr_bl()]
            @ tl_br()
            @ [H_({dx: Float.neg(tip)})],
          )
        | `PostOp => (
            [H_({dx: Float.neg(tip)}), ...bl_tr()]
            @ br_tl()
            @ [H_({dx: tip})],
            tl_br() @ tr_bl(),
          )
        | `BinOp(_) => (
            [H_({dx: Float.neg(tip)}), ...bl_tr()]
            @ br_tl()
            @ [H_({dx: tip})],
            [H_({dx: tip}), ...tr_bl()]
            @ tl_br()
            @ [H_({dx: Float.neg(tip)})],
          )
        };
      let left_and_top_sides =
        profile.open_children
        |> List.map(((start, len)) =>
             List.concat([
               [H({x: Float.of_int(start) -. 1. +. tip})],
               tr_bl(),
               tl_br(~child_border=`South, ()),
               [H_({dx: Float.of_int(len)})],
               bl_tr(~child_border=`South, ()),
               br_tl(),
             ])
           )
        |> List.cons([M({x: 0., y: 1.}), ...left_tip])
        |> List.concat;
      List.concat([
        left_and_top_sides,
        [H({x: Float.of_int(profile.len)}), ...right_tip],
        [Z],
      ]);
    };
    let path = outer_path @ List.concat(closed_child_paths) @ hole_path;
    SvgUtil.Path.view(
      ~attrs=[
        Attr.classes([Sort.to_string(sort), "tile-decoration"]),
        ...attrs,
      ],
      path,
    );
  };

  let shadow_filter = (~sort: Sort.t) =>
    Node.create_svg(
      "filter",
      [Attr.id("outer-drop-shadow")],
      [
        Node.create_svg(
          "feDropShadow",
          [
            Attr.classes([
              "tile-decoration-drop-shadow",
              Sort.to_string(sort),
            ]),
            Attr.create("dx", "0.1"),
            Attr.create("dy", "0.04"),
            Attr.create("stdDeviation", "0"),
          ],
          [],
        ),
      ],
    );

  let view =
      (
        ~sort: Sort.t,
        ~attrs: list(Attr.t)=[],
        ~hole_radii: (float, float),
        profile: profile,
      )
      : list(Node.t) => [
    contour_path(~sort, ~attrs, ~hole_radii, profile),
    shadow_filter(~sort),
  ];
};

module Caret = {
  let view = (~font_metrics: FontMetrics.t, offset: int, _) =>
    Node.div(
      [
        Attr.id("caret"),
        Attr.create(
          "style",
          Printf.sprintf(
            "top: 0; left: %fpx",
            (Float.of_int(offset) +. 0.5) *. font_metrics.col_width,
          ),
        ),
      ],
      [
        Node.table(
          [
            Attr.id("action-table"),
            Attr.create(
              "style",
              Printf.sprintf("top: %fpx", font_metrics.row_height),
            ),
          ],
          [
            Node.tr(
              [],
              [
                Node.td(
                  [Attr.create("colspan", "2")],
                  [Node.text("Move")],
                ),
              ],
            ),
            Node.tr(
              [],
              [
                Node.td(
                  [Attr.create("colspan", "2")],
                  [Node.text("Construct")],
                ),
              ],
            ),
            Node.tr(
              [],
              [
                Node.td(
                  [Attr.create("colspan", "2")],
                  [Node.text("Delete")],
                ),
              ],
            ),
          ],
        ),
      ],
    );
};
