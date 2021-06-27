open Virtual_dom.Vdom;
open Util;
open Cor;
open DecConstants;

module ErrHole = {
  type profile = {
    expanded: bool,
    len: int,
  };

  let view = ({expanded, len}: profile): list(Node.t) => {
    open SvgUtil.Path;
    open Diag;
    // TODO systematize magic numbers
    let len = Float.of_int(len);
    let tl_br =
      tl_br(
        ~hemi=`North,
        ~with_child_border=!expanded,
        ~stretch_x=expanded ? 0.4 : 0.,
        ~stretch_y=expanded ? 0.09 : 0.,
        (),
      );
    let tr_bl =
      tr_bl(
        ~hemi=`South,
        ~with_child_border=!expanded,
        ~stretch_x=expanded ? 0.4 : 0.,
        ~stretch_y=expanded ? 0.09 : 0.,
        (),
      );
    let br_tl =
      br_tl(
        ~hemi=`South,
        ~with_child_border=!expanded,
        ~stretch_x=expanded ? 0.4 : 0.,
        ~stretch_y=expanded ? 0.09 : 0.,
        (),
      );
    let bl_tr =
      bl_tr(
        ~hemi=`North,
        ~with_child_border=!expanded,
        ~stretch_x=expanded ? 0.4 : 0.,
        ~stretch_y=expanded ? 0.09 : 0.,
        (),
      );
    let path =
      // 1.2 and 2.4 are hacks to get non-expanded err hole
      // smaller and aligned with unidelimited child decorations.
      // TODO unify these magic constants with the calculations
      // for diagonal edges
      List.concat([
        [
          M({x: 1.2, y: expanded ? 0. : child_border_thickness}),
          H_({dx: len -. 2.4}),
        ],
        tl_br,
        tr_bl,
        [H_({dx: Float.neg(len -. 2.4)})],
        br_tl,
        bl_tr,
        [Z],
      ]);
    let attrs =
      Attr.[
        classes(["err-hole-path"]),
        create("vector-effect", "non-scaling-stroke"),
      ];
    [
      view(
        ~attrs,
        expanded ? transpose({dx: 0.03, dy: (-0.075)}, path) : path,
      ),
    ];
  };
};

let left_tip_path =
    (~scale_x as s_x=1., ~scale_y as s_y=1., tip: Layout.tip_shape)
    : SvgUtil.Path.t => {
  open SvgUtil.Path;
  open Diag;
  let path =
    switch (tip) {
    | (Convex, _) => br_tl(~hemi=`South, ()) @ bl_tr(~hemi=`North, ())
    | (Concave, n) =>
      let jag = [
        L_({dx: -. jagged_edge_w, dy: -. jagged_edge_h}),
        L_({dx: jagged_edge_w, dy: -. jagged_edge_h}),
        L_({dx: -. jagged_edge_w, dy: -. jagged_edge_h}),
      ];
      let bottom_half =
        n == 0
          ? [H_({dx: Float.neg(tip_width)}), ...bl_tr(~hemi=`South, ())]
          : List.concat([
              [H_({dx: -. (extra_tail +. 0.5)})],
              jag,
              [H_({dx: jagged_edge_w +. extra_tail})],
              bl_tr(~hemi=`South, ~with_child_border=true, ()),
            ]);
      let top_half =
        n == 0 || n == 1
          ? br_tl(~hemi=`North, ()) @ [H_({dx: tip_width})]
          : List.concat([
              br_tl(~hemi=`North, ~with_child_border=true, ()),
              [H_({dx: -. (jagged_edge_w +. extra_tail)})],
              jag,
              [H_({dx: extra_tail +. 0.5})],
            ]);
      bottom_half @ top_half;
    };
  scale_x(s_x, scale_y(s_y, path));
};
let right_tip_path =
    (~scale_x as s_x=1., ~scale_y as s_y=1., tip: Layout.tip_shape)
    : SvgUtil.Path.t => {
  open SvgUtil.Path;
  open Diag;
  let path =
    switch (tip) {
    | (Convex, _) => tl_br(~hemi=`North, ()) @ tr_bl(~hemi=`South, ())
    | (Concave, n) =>
      open SvgUtil.Path;
      let jag = [
        L_({dx: jagged_edge_w, dy: jagged_edge_h}),
        L_({dx: -. jagged_edge_w, dy: jagged_edge_h}),
        L_({dx: jagged_edge_w, dy: jagged_edge_h}),
      ];
      let top_half =
        n == 0 || n == 1
          ? [H_({dx: tip_width}), ...tr_bl(~hemi=`North, ())]
          : List.concat([
              [H_({dx: 0.5 +. extra_tail})],
              jag,
              [H_({dx: -. (extra_tail +. jagged_edge_w)})],
              tr_bl(~hemi=`North, ~with_child_border=true, ()),
            ]);
      let bottom_half =
        n == 0
          ? tl_br(~hemi=`South, ()) @ [H_({dx: Float.neg(tip_width)})]
          : List.concat([
              tl_br(~with_child_border=true, ~hemi=`South, ()),
              [H_({dx: extra_tail})],
              jag,
              [H_({dx: Float.neg(jagged_edge_w +. extra_tail +. 0.5)})],
            ]);
      top_half @ bottom_half;
    };
  scale_x(s_x, scale_y(s_y, path));
};

module EmptyHole = {
  let inset_shadow_filter = (~color) => {
    let c_cls = Color.to_string(color);
    Node.create_svg(
      "filter",
      [Attr.id(Printf.sprintf("empty-hole-inset-shadow-%s", c_cls))],
      [
        Node.create_svg(
          "feOffset",
          Attr.[
            create("in", "SourceAlpha"),
            create("dx", raised_shadow_dx),
            create("dy", raised_shadow_dy),
            create("result", "offset-alpha"),
          ],
          [],
        ),
        Node.create_svg(
          "feFlood",
          Attr.[
            classes(["empty-hole-inset-shadow-flood", c_cls]),
            create("flood-opacity", "1"),
            create("result", "color"),
          ],
          [],
        ),
        Node.create_svg(
          "feComposite",
          Attr.[
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
  };

  let thin_inset_shadow_filter = (~color) => {
    let c_cls = Color.to_string(color);
    Node.create_svg(
      "filter",
      [Attr.id(Printf.sprintf("empty-hole-thin-inset-shadow-%s", c_cls))],
      [
        Node.create_svg(
          "feOffset",
          Attr.[
            create("in", "SourceAlpha"),
            create("dx", shadow_dx),
            create("dy", shadow_dy),
            create("result", "offset-alpha"),
          ],
          [],
        ),
        Node.create_svg(
          "feFlood",
          Attr.[
            classes(["empty-hole-inset-shadow-flood", c_cls]),
            create("flood-opacity", "1"),
            create("result", "color"),
          ],
          [],
        ),
        Node.create_svg(
          "feComposite",
          Attr.[
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
  };

  let path = (tip, offset, s: float) => {
    let x_dilate = 1.5;
    List.concat(
      SvgUtil.Path.[
        [
          M({x: offset +. 0.5, y: 0.5 -. s /. 2.}),
          H_({dx: x_dilate *. s /. 2.}),
        ],
        right_tip_path(~scale_x=s *. x_dilate, ~scale_y=s, (tip, 0)),
        [H_({dx: -. s *. x_dilate})],
        left_tip_path(~scale_x=s *. x_dilate, ~scale_y=s, (tip, 0)),
        [Z],
      ],
    );
  };

  let view =
      (
        ~offset=0,
        ~color: Color.t,
        ~tip: Tip.shape,
        ~inset: option([ | `Thick | `Thin]),
        (),
      )
      : list(Node.t) => {
    let c_cls = Color.to_string(color);
    SvgUtil.Path.[
      view(
        ~attrs=
          AttrUtil.[
            Attr.classes(["empty-hole-path", c_cls]),
            vector_effect("non-scaling-stroke"),
            stroke_width(Option.is_some(inset) ? 0.3 : 1.),
            filter(
              switch (inset) {
              | None => "none"
              | Some(`Thin) =>
                Printf.sprintf("url(#empty-hole-thin-inset-shadow-%s)", c_cls)
              | Some(`Thick) =>
                Printf.sprintf("url(#empty-hole-inset-shadow-%s)", c_cls)
              },
            ),
          ],
        path(tip, Float.of_int(offset), 0.28),
      ),
    ];
  };
};

module UniChild = {
  type profile = {
    sort: Sort.t,
    side: Direction.t,
    len: int,
  };

  let view = ({sort, side, len}: profile): list(Node.t) => {
    open SvgUtil.Path;
    open Diag;
    let len = Float.of_int(len);
    /* old raised tile path
       let path =
         List.concat([
           [M({x: 0., y: 0.}), H_({dx: len})],
           tl_br(),
           tr_bl(),
           [H_({dx: Float.neg(len)})],
           br_tl(),
           bl_tr(),
           [Z],
         ]);
       */
    let (gradient_id, gradient) = {
      let id =
        switch (side) {
        | Left => "uni-child-gradient-left"
        | Right => "uni-child-gradient-right"
        };
      let (x1, x2) =
        switch (side) {
        | Left => ("1", Printf.sprintf("%f", len +. 1.))
        | Right => (Printf.sprintf("%f", len -. 1.), "-1")
        };
      let color =
        switch (sort) {
        | Pat => "var(--pat-shadow-color)"
        | Exp => "var(--exp-shadow-color)"
        };
      let gradient =
        Node.create_svg(
          "linearGradient",
          [
            Attr.id(id),
            Attr.create("gradientUnits", "userSpaceOnUse"),
            Attr.create("x1", x1),
            Attr.create("x2", x2),
          ],
          [
            Node.create_svg(
              "stop",
              [Attr.create("offset", "0%"), AttrUtil.stop_color(color)],
              [],
            ),
            Node.create_svg(
              "stop",
              [
                Attr.create(
                  "offset",
                  Printf.sprintf("%f%%", 100. *. (len -. 1.25) /. len),
                ),
                AttrUtil.stop_color(color),
              ],
              [],
            ),
            Node.create_svg(
              "stop",
              [
                Attr.create(
                  "offset",
                  Printf.sprintf("%f%%", 100. *. (len -. 0.6) /. len),
                ),
                AttrUtil.stop_color(color),
                AttrUtil.stop_opacity(0.),
              ],
              [],
            ),
          ],
        );
      (id, gradient);
    };
    let path =
      switch (side) {
      | Left =>
        List.concat([
          [M({x: len +. 1., y: 0.}), H_({dx: Float.neg(len)})],
          // [M({x: 0., y: 0.})],
          tr_bl(~hemi=`North, ()),
          tl_br(~hemi=`South, ()),
          [H_({dx: len})],
          [M_({dx: 0., dy: 0.02}), H_({dx: Float.neg(len)})],
        ])
      | Right =>
        List.concat([
          [M({x: (-1.), y: 0.}), H_({dx: len})],
          // [M({x: len, y: 0.})],
          tl_br(~hemi=`North, ()),
          tr_bl(~hemi=`South, ()),
          [H_({dx: Float.neg(len)})],
          [M_({dx: 0., dy: 0.02}), H_({dx: len})],
          // bl_tr(),
          // br_tl(),
        ])
      };
    let attrs =
      Attr.[
        classes(["uni-child-path", Sort.to_string(sort)]),
        create("vector-effect", "non-scaling-stroke"),
        create("stroke", Printf.sprintf("url(#%s)", gradient_id)),
      ];
    [gradient, view(~attrs, path)];
  };
};

let closed_child_path = ((start, len)) =>
  List.concat(
    SvgUtil.Path.[
      [M({x: Float.of_int(start) +. 0.5, y: child_border_thickness})],
      Diag.tr_bl(~with_child_border=true, ~hemi=`North, ()),
      Diag.tl_br(~with_child_border=true, ~hemi=`South, ()),
      [H_({dx: Float.of_int(len - 1)})],
      Diag.bl_tr(~with_child_border=true, ~hemi=`South, ()),
      Diag.br_tl(~with_child_border=true, ~hemi=`North, ()),
      [Z],
    ],
  );

module Selem = {
  open Diag;

  type profile = {
    color: Color.t,
    shape: Layout.selem_shape,
    style: SelemStyle.t,
    len: int,
    open_children: list((int, int)),
    closed_children: list((int, int)),
    empty_holes: list((int, Color.t, Tip.shape)),
  };
  let mk_profile =
      (
        ~open_children=[],
        ~closed_children=[],
        ~empty_holes=[],
        ~color,
        ~style,
        ~len,
        ~shape,
        (),
      ) => {
    color,
    shape,
    len,
    open_children,
    closed_children,
    empty_holes,
    style,
  };

  let raised_shadow_filter = (~color: Color.t) => {
    let s = Color.to_string(color);
    Node.create_svg(
      "filter",
      [Attr.id("raised-drop-shadow-" ++ s)],
      [
        Node.create_svg(
          "feDropShadow",
          [
            Attr.classes(["tile-drop-shadow"]),
            Attr.create("dx", raised_shadow_dx),
            Attr.create("dy", raised_shadow_dy),
            Attr.create("stdDeviation", "0"),
          ],
          [],
        ),
      ],
    );
  };

  let shadow_filter = (~color: Color.t) => {
    let s = Color.to_string(color);
    Node.create_svg(
      "filter",
      [Attr.id("drop-shadow-" ++ s)],
      [
        Node.create_svg(
          "feDropShadow",
          [
            Attr.classes(["tile-drop-shadow"]),
            Attr.create("dx", shadow_dx),
            Attr.create("dy", shadow_dy),
            Attr.create("stdDeviation", "0"),
          ],
          [],
        ),
      ],
    );
  };

  let open_child_paths =
      (~start, ~color: Color.t, open_children: list((int, int)))
      : list(Node.t) => {
    open SvgUtil.Path;
    let color =
      switch (color) {
      | Selected => "var(--unsorted-shadow-color)"
      | Typ => "var(--typ-shadow-color)"
      | Pat => "var(--pat-shadow-color)"
      | Exp => "var(--exp-shadow-color)"
      };
    let gradient = (id, start, len) =>
      Node.create_svg(
        "linearGradient",
        [
          Attr.id(id),
          Attr.create("gradientUnits", "userSpaceOnUse"),
          Attr.create("x1", string_of_int(start)),
          // TODO unify with space
          Attr.create("x2", string_of_int(start + len)),
        ],
        NodeUtil.[
          stop(
            AttrUtil.[
              offset(0.6 /. Float.of_int(len)),
              stop_color(color),
              stop_opacity(0.),
            ],
          ),
          stop(
            AttrUtil.[
              offset(1.25 /. Float.of_int(len)),
              stop_color(color),
            ],
          ),
          stop(
            AttrUtil.[
              offset((Float.of_int(len) -. 1.25) /. Float.of_int(len)),
              stop_color(color),
            ],
          ),
          stop(
            AttrUtil.[
              offset((Float.of_int(len) -. 0.6) /. Float.of_int(len)),
              stop_color(color),
              stop_opacity(0.),
            ],
          ),
        ],
      );
    open_children
    |> List.map(((start', len)) => {
         let gradient_id =
           Printf.sprintf(
             "bidelimited-open-child-gradient-%d-%d",
             start,
             start',
           );
         [
           gradient(gradient_id, start', len),
           view(
             ~attrs=
               Attr.[
                 classes(["bidelimited-open-child-path"]),
                 AttrUtil.vector_effect("non-scaling-stroke"),
                 create("stroke", Printf.sprintf("url(#%s)", gradient_id)),
               ],
             [
               M({x: Float.of_int(start'), y: 0.}),
               H_({dx: Float.of_int(len)}),
             ],
           ),
         ];
       })
    |> List.flatten;
  };

  let empty_hole_path =
      (~font_metrics as _, empty_hole: (int, Color.t, Tip.shape))
      : SvgUtil.Path.t => {
    // let (rx, ry) = hole_radii(~font_metrics);
    // let (o, _sort_todo) = empty_hole;
    // SvgUtil.Path.[
    //   M({x: Float.of_int(o) +. 0.5 -. rx, y: 0.5}),
    //   A_({
    //     rx,
    //     ry,
    //     x_axis_rotation: 0.,
    //     large_arc_flag: false,
    //     sweep_flag: false,
    //     dx: 2. *. rx,
    //     dy: 0.,
    //   }),
    //   A_({
    //     rx,
    //     ry,
    //     x_axis_rotation: 0.,
    //     large_arc_flag: false,
    //     sweep_flag: false,
    //     dx: (-2.) *. rx,
    //     dy: 0.,
    //   }),
    // ];
    let (offset, _color, tip) = empty_hole;
    EmptyHole.path(tip, Float.of_int(offset), 0.28);
  };

  let open_child_path = ((start, len): (int, int)) =>
    List.concat(
      SvgUtil.Path.[
        [H({x: Float.of_int(start) +. tip_width})],
        tr_bl(~hemi=`North, ()),
        tl_br(~with_child_border=true, ~hemi=`South, ()),
        [H_({dx: Float.of_int(len - 1)})],
        bl_tr(~with_child_border=true, ~hemi=`South, ()),
        br_tl(~hemi=`North, ()),
      ],
    );

  let contour_path = (~font_metrics, profile: profile): SvgUtil.Path.t => {
    open SvgUtil.Path;
    let empty_hole_paths = {
      let raised_holes =
        SelemStyle.show_children(profile.style)
          // always show holes in empty hole tiles
          ? List.filter(((n, _, _)) => n == 0, profile.empty_holes)
          : profile.empty_holes;
      List.map(empty_hole_path(~font_metrics), raised_holes);
    };
    let closed_child_paths =
      List.map(closed_child_path, profile.closed_children);
    let outer_path: SvgUtil.Path.t = {
      let start =
        SelemStyle.stretched(profile.style) ? Float.neg(stretch_dx) : 0.;
      let end_ =
        SelemStyle.stretched(profile.style)
          ? Float.of_int(profile.len) +. stretch_dx
          : Float.of_int(profile.len);
      List.concat([
        [M({x: start, y: 1.}), ...left_tip_path(fst(profile.shape))],
        ListUtil.flat_map(open_child_path, profile.open_children),
        [H({x: end_}), ...right_tip_path(snd(profile.shape))],
        [Z],
      ]);
    };
    outer_path
    @ List.concat(closed_child_paths)
    @ List.concat(empty_hole_paths);
  };

  let contour_path_attrs = (~attrs, profile) => {
    let clss = {
      let c_cls = Color.to_string(profile.color);
      let highlighted =
        SelemStyle.highlighted(profile.style) ? ["highlighted"] : [];
      let filtered = SelemStyle.filtered(profile.style) ? ["filtered"] : [];
      let raised = ["raised"]; // profile.style.raised ? ["raised"] : [];
      List.concat([["tile-path", c_cls], highlighted, raised, filtered]);
    };
    Attr.[
      classes(clss),
      create("vector-effect", "non-scaling-stroke"),
      ...attrs,
    ];
  };

  let view =
      (
        ~attrs: list(Attr.t)=[],
        ~font_metrics: FontMetrics.t,
        ~start: int,
        profile: profile,
      )
      : list(Node.t) => {
    // TODO maybe remove this flag and just specify via children fields?
    let profile =
      SelemStyle.show_children(profile.style)
        ? profile : {...profile, open_children: [], closed_children: []};
    let open_child_paths =
      open_child_paths(~start, ~color=profile.color, profile.open_children);
    open_child_paths
    @ [
      SvgUtil.Path.view(
        ~attrs=contour_path_attrs(~attrs, profile),
        contour_path(~font_metrics, profile),
      ),
    ];
  };
};

module SelectedBar = {
  let view = (~font_metrics: FontMetrics.t, ~len: int, (sort_l, sort_r)) => {
    let (cls_l, cls_r) =
      TupleUtil.map2(s => Color.(to_string(of_sort(s))), (sort_l, sort_r));
    if (sort_l == sort_r) {
      [
        Node.create_svg(
          "line",
          Attr.[
            create("x1", "0.5"),
            create("y1", "-0.3"),
            create("x2", Printf.sprintf("%fpx", Float.of_int(len) -. 0.5)),
            create("y2", "-0.3"),
            classes(["same-sort", cls_l]),
          ],
          [],
        ),
      ];
    } else {
      let skew_x = 5. /. font_metrics.col_width;
      let skew_y = 5. /. font_metrics.row_height;
      SvgUtil.Path.[
        view(
          ~attrs=Attr.[classes(["different-sort", cls_l])],
          [
            M({x: 0.5, y: (-0.3)}),
            L_({dy: 0., dx: (Float.of_int(len) -. 1. -. skew_x) /. 2.}),
            L_({dx: skew_x, dy: -. skew_y}),
          ],
        ),
        view(
          ~attrs=Attr.[classes(["different-sort", cls_r])],
          [
            M({x: Float.of_int(len) -. 0.5, y: (-0.3)}),
            L_({dy: 0., dx: -. (Float.of_int(len) -. 1. -. skew_x) /. 2.}),
            L_({dx: -. skew_x, dy: skew_y}),
          ],
        ),
      ];
    };
  };
};

// TODO rename
module Bar = {
  let view = (~font_metrics: FontMetrics.t) =>
    Node.div(
      Attr.[
        id("bar"),
        create(
          "style",
          Printf.sprintf(
            "top: calc(%fpx + 1.5px); height: 1.5px;",
            (-0.3) *. font_metrics.row_height,
          ),
        ),
      ],
      [
        Node.create_svg(
          "svg",
          Attr.[
            create("viewBox", "0 0 1 1"),
            create("preserveAspectRatio", "none"),
          ],
          [
            Node.create_svg(
              "rect",
              Attr.[create("width", "1"), create("height", "1")],
              [],
            ),
          ],
        ),
      ],
    );
};

module TargetBounds = {
  let gradient_id = "target-bounds-gradient";
  let gradient =
      (len, (l_strict, r_strict), frame_sort, mode: CaretMode.t): Node.t => {
    let c =
      switch (mode) {
      | Pointing => Color.of_sort(frame_sort)
      | Selecting
      | Restructuring(_) => Selected
      };
    let stop = (offset, opacity) =>
      Node.create_svg(
        "stop",
        [
          Attr.create("offset", Printf.sprintf("%f%%", 100. *. offset)),
          Attr.classes([Color.to_string(c)]),
          AttrUtil.stop_opacity(opacity),
        ],
        [],
      );
    Node.create_svg(
      "linearGradient",
      Attr.[
        id(gradient_id),
        create("gradientUnits", "userSpaceOnUse"),
        create("x1", "0"),
        create("x2", "1"),
      ],
      [
        stop(0., 0.),
        stop(1.5 /. len, l_strict ? 0. : 1.),
        stop(1.5 /. len, 0.),
        stop((len -. 1.5) /. len, 0.),
        stop((len -. 1.5) /. len, r_strict ? 0. : 1.),
        stop(1., 0.),
      ],
    );
  };

  let view =
      (
        ~font_metrics: FontMetrics.t,
        ~origin: int,
        ~len: int,
        strict_bounds: (bool, bool),
        frame_sort: Sort.t,
        mode: CaretMode.t,
      ) => {
    let len = Float.of_int(len + 2);
    let gradient = gradient(len, strict_bounds, frame_sort, mode);
    Node.div(
      Attr.[
        id("target-bounds"),
        create(
          "style",
          Printf.sprintf(
            "top: calc(%fpx + 1px); height: 2px; left: %fpx; width: %fpx;",
            (-0.3) *. font_metrics.row_height,
            Float.of_int(origin - 1) *. font_metrics.col_width,
            len *. font_metrics.col_width,
          ),
        ),
      ],
      [
        Node.create_svg(
          "svg",
          Attr.[
            create("viewBox", "0 0 1 1"),
            create("preserveAspectRatio", "none"),
            create(
              "style",
              "position: absolute; left: 0; top: 0; width: 100%; height: 100%;",
            ),
          ],
          [
            gradient,
            Node.create_svg(
              "rect",
              Attr.[
                create("width", "1"),
                create("height", "1"),
                create("fill", Printf.sprintf("url(#%s)", gradient_id)),
              ],
              [],
            ),
          ],
        ),
      ],
    );
  };
};

module RestructuringGenie = {
  let view = (~length) => [
    // <feGaussianBlur in="SourceAlpha"
    //             stdDeviation="4"
    //             result="blur"/>
    Node.create_svg(
      "filter",
      Attr.[id("restructuring-genie-filter")],
      Node.[
        create_svg(
          "feGaussianBlur",
          Attr.[
            create("in", "SourceGraphic"),
            create("stdDeviation", "0.05"),
          ],
          [],
        ),
      ],
    ),
    SvgUtil.Path.(
      view(
        ~attrs=[Attr.classes(["restructuring-genie-path"])],
        [
          // slightly boost genie tip for better visibility
          // when restructuring selection gets large
          M({x: 0.5, y: (-0.3) +. (-0.05)}),
          // TODO unify with caret height numbers (-1.8)
          V({y: (-2.4)}),
          H_({dx: Float.of_int(length)}),
          V_({dy: 1.4}),
          Z,
        ],
      )
    ),
  ];
};

let container =
    (
      ~font_metrics: FontMetrics.t,
      ~origin: int=0,
      ~length: int,
      ~cls: string,
      ~container_clss=[],
      svgs: list(Node.t),
    )
    : Node.t => {
  let buffered_height = 8;
  let buffered_width = length + 3;

  let buffered_height_px =
    Float.of_int(buffered_height) *. font_metrics.row_height;
  let buffered_width_px =
    Float.of_int(buffered_width) *. font_metrics.col_width;

  let container_origin_x =
    (Float.of_int(origin) -. 1.5) *. font_metrics.col_width;
  let container_origin_y = (-3.5) *. font_metrics.row_height;

  Node.div(
    Attr.[
      classes([
        "decoration-container",
        Printf.sprintf("%s-container", cls),
        ...container_clss,
      ]),
      create(
        "style",
        Printf.sprintf(
          "top: calc(%fpx + 2px); left: %fpx;",
          container_origin_y,
          container_origin_x,
        ),
      ),
    ],
    [
      Node.create_svg(
        "svg",
        Attr.[
          classes([cls]),
          create(
            "viewBox",
            Printf.sprintf(
              "-1.5 -3.5 %d %d",
              buffered_width,
              buffered_height,
            ),
          ),
          create("width", Printf.sprintf("%fpx", buffered_width_px)),
          create("height", Printf.sprintf("%fpx", buffered_height_px)),
          create("preserveAspectRatio", "none"),
        ],
        svgs,
      ),
    ],
  );
};
