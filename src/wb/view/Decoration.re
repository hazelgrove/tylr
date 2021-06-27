open Virtual_dom.Vdom;
open Util;
open Cor;
open DecConstants;

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
    EmptyHoleDec.path(tip, Float.of_int(offset), 0.28);
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
        [M({x: start, y: 1.}), ...Diag.left_tip_path(fst(profile.shape))],
        ListUtil.flat_map(open_child_path, profile.open_children),
        [H({x: end_}), ...Diag.right_tip_path(snd(profile.shape))],
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
