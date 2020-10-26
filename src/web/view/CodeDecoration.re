open Virtual_dom.Vdom;

module Sort = Core.Sort;

let tip = 0.3;
let child_border_thickness = 0.1;

module Diag = {
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
};

module ErrHole = {
  type profile = {
    expanded: bool,
    len: int,
  };

  let view = ({expanded, len}: profile): list(Node.t) => {
    open SvgUtil.Path;
    open Diag;
    let len = Float.of_int(len);
    let tl_br = expanded ? tl_br() : tl_br(~child_border=`North, ());
    let tr_bl = expanded ? tr_bl() : tr_bl(~child_border=`South, ());
    let br_tl = expanded ? br_tl() : br_tl(~child_border=`South, ());
    let bl_tr = expanded ? bl_tr() : bl_tr(~child_border=`North, ());
    let path =
      List.concat([
        [
          M({x: 0., y: expanded ? child_border_thickness : 0.}),
          H_({dx: len}),
        ],
        tl_br,
        tr_bl,
        [H_({dx: Float.neg(len)})],
        br_tl,
        bl_tr,
        [Z],
      ]);
    let attrs =
      Attr.[
        classes(["err-hole-decoration"]),
        create("vector-effect", "non-scaling-stroke"),
      ];
    [view(~attrs, path)];
  };
};

module Tile = {
  type profile = {
    shape: [ | `Operand(bool) | `PreOp | `PostOp | `BinOp(bool)],
    len: int,
    open_children: list((int, int)),
    closed_children: list((int, int)),
  };

  let contour_paths =
      (
        ~sort: Sort.t,
        ~hole_radii as (rx, ry): (float, float),
        ~attrs: list(Attr.t),
        profile: profile,
      )
      : list(Node.t) => {
    open SvgUtil.Path;
    open Diag;
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
    let hole_path =
      SvgUtil.Path.view(
        ~attrs=
          Attr.[
            classes(["tile-hole-path"]),
            create("vector-effect", "non-scaling-stroke"),
          ],
        hole_path,
      );
    let tile_path =
      SvgUtil.Path.view(
        ~attrs=
          Attr.[
            classes([Sort.to_string(sort), "tile-path"]),
            create("vector-effect", "non-scaling-stroke"),
            ...attrs,
          ],
        outer_path @ List.concat(closed_child_paths),
      );
    [tile_path, hole_path];
  };

  let hole_filter =
    Node.create_svg(
      "filter",
      [Attr.id("tile-hole-drop-shadow")],
      [
        Node.create_svg(
          "feOffset",
          Attr.[
            create("in", "SourceAlpha"),
            create("dx", "0.1"),
            create("dy", "0.04"),
            create("result", "offset-alpha"),
          ],
          [],
        ),
        Node.create_svg(
          "feFlood",
          Attr.[
            classes(["tile-hole-inset-shadow"]),
            create("flood-opacity", "1"),
            create("result", "color"),
          ],
          [],
        ),
        Node.create_svg(
          "feComposite",
          Attr.[
            // Attr.classes(["closed-child-drop-shadow"]),
            create("operator", "out"),
            create("in", "SourceAlpha"),
            create("in2", "offset-alpha"),
            create("result", "shadow-shape"),
          ],
          [],
        ),
        Node.create_svg(
          "feComposite",
          Attr.[
            create("operator", "in"),
            create("in", "color"),
            create("in2", "shadow-shape"),
            create("result", "drop-shadow"),
          ],
          [],
        ),
        Node.create_svg(
          "feMerge",
          [],
          [
            Node.create_svg(
              "feMergeNode",
              [Attr.create("in", "SourceGraphic")],
              [],
            ),
            Node.create_svg(
              "feMergeNode",
              [Attr.create("in", "drop-shadow")],
              [],
            ),
          ],
        ),
      ],
    );

  let shadow_filter = (~sort: Sort.t) =>
    Node.create_svg(
      "filter",
      [Attr.id("outer-drop-shadow")],
      [
        Node.create_svg(
          "feDropShadow",
          [
            Attr.classes(["tile-drop-shadow", Sort.to_string(sort)]),
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
    shadow_filter(~sort),
    hole_filter,
    ...contour_paths(~sort, ~attrs, ~hole_radii, profile),
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
