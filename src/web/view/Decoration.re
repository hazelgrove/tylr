open Sexplib.Std;
open Virtual_dom.Vdom;
open Util;

module Sort = Core.Sort;
module Direction = Core.Direction;

let tip_width = 0.3;
let child_border_thickness = 0.1;

let t = child_border_thickness /. 0.5;
let short_tip_width = (1. -. t) *. tip_width;
let short_tip_height = (1. -. t) *. 0.5;

let stretch_dx = 0.15;

let raised_shadow_dx = "0.1";
let raised_shadow_dy = "0.03";
let shadow_dx = "0.06";
let shadow_dy = "0.024";

let hole_radii = (~font_metrics: FontMetrics.t) => {
  let r = 3.5;
  (r /. font_metrics.col_width, r /. font_metrics.row_height);
};

module Diag = {
  let tr_bl = (~child_border: option([ | `North | `South])=?, ()) =>
    SvgUtil.Path.(
      switch (child_border) {
      | None =>
        let diag = L_({dx: Float.neg(tip_width), dy: 0.5});
        [diag];
      | Some(d) =>
        let diag =
          L_({dx: Float.neg(short_tip_width), dy: short_tip_height});
        let border = H_({dx: Float.neg(0.5 -. short_tip_width)});
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
        let diag = L_({dx: tip_width, dy: 0.5});
        [diag];
      | Some(d) =>
        let diag = L_({dx: short_tip_width, dy: short_tip_height});
        let border = H_({dx: 0.5 -. short_tip_width});
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
          M({x: 1., y: expanded ? 0. : child_border_thickness}),
          H_({dx: len -. 2.}),
        ],
        tl_br,
        tr_bl,
        [H_({dx: Float.neg(len -. 2.)})],
        br_tl,
        bl_tr,
        [Z],
      ]);
    let attrs =
      Attr.[
        classes(["err-hole-path"]),
        create("vector-effect", "non-scaling-stroke"),
      ];
    [view(~attrs, path)];
  };
};

module EmptyHole = {
  let inset_shadow_filter =
    Node.create_svg(
      "filter",
      [Attr.id("empty-hole-inset-shadow")],
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
            classes(["empty-hole-inset-shadow-flood"]),
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

  let thin_inset_shadow_filter =
    Node.create_svg(
      "filter",
      [Attr.id("empty-hole-thin-inset-shadow")],
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
            classes(["empty-hole-inset-shadow-flood"]),
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

  let view =
      (
        ~offset=0,
        ~inset: option([ | `Thick | `Thin]),
        ~font_metrics: FontMetrics.t,
        (),
      )
      : list(Node.t) => {
    // necessary because of AttrUtil shadowing below
    let o = offset;
    let (r_x, r_y) = hole_radii(~font_metrics);
    [
      Node.create_svg(
        "ellipse",
        AttrUtil.[
          cx(Float.of_int(o) +. 0.5),
          cy(0.5),
          rx(r_x),
          ry(r_y),
          vector_effect("non-scaling-stroke"),
          stroke_width(Option.is_some(inset) ? 0.3 : 0.75),
          filter(
            switch (inset) {
            | None => "none"
            | Some(`Thin) => "url(#empty-hole-thin-inset-shadow)"
            | Some(`Thick) => "url(#empty-hole-inset-shadow)"
            },
          ),
          Attr.classes(["empty-hole-path"]),
        ],
        [],
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
    let len = Float.of_int(len - 1);
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
        | Left => "open-child-gradient-left"
        | Right => "open-child-gradient-right"
        };
      let (x1, x2) =
        switch (side) {
        | Left => ("1", Printf.sprintf("%f", len +. 1.))
        | Right => (Printf.sprintf("%f", len), "0")
        };
      let color =
        switch (sort) {
        | Typ => "var(--typ-shadow-color)"
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
                AttrUtil.stop_opacity("0"),
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
          tr_bl(),
          tl_br(),
          [H_({dx: len})],
          [M_({dx: 0., dy: 0.02}), H_({dx: Float.neg(len)})],
        ])
      | Right =>
        List.concat([
          [M({x: 0., y: 0.}), H_({dx: len})],
          // [M({x: len, y: 0.})],
          tl_br(),
          tr_bl(),
          [H_({dx: Float.neg(len)})],
          [M_({dx: 0., dy: 0.02}), H_({dx: len +. 0.03})],
          bl_tr(),
          br_tl(),
        ])
      };
    let attrs =
      Attr.[
        classes(["open-child-path", Sort.to_string(sort)]),
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
      Diag.tr_bl(~child_border=`North, ()),
      Diag.tl_br(~child_border=`South, ()),
      [H_({dx: Float.of_int(len - 1)})],
      Diag.bl_tr(~child_border=`South, ()),
      Diag.br_tl(~child_border=`North, ()),
      [Z],
    ],
  );

module Tessera = {
  type profile = {
    shape: Layout.tessera_shape,
    len: int,
    closed_children: list((int, int)),
    style: Layout.tessera_style,
  };

  let extra_tail = 0.;

  let gradient = (id, color, profile) => {
    let (x1, x2) = {
      let dx = 0.5 +. extra_tail;
      let dx = profile.style.stretched ? dx +. stretch_dx : dx;
      (-. dx, Float.of_int(profile.len) +. dx);
    };
    let range = x2 -. x1;
    let (x1, x2) = (Printf.sprintf("%f", x1), Printf.sprintf("%f", x2));
    let gradient =
      Node.create_svg(
        "linearGradient",
        [
          Attr.id(id),
          Attr.create("gradientUnits", "userSpaceOnUse"),
          Attr.create("x1", x1),
          Attr.create("x2", x2),
        ],
        Node.[
          create_svg(
            "stop",
            [
              Attr.create("offset", "0%"),
              AttrUtil.stop_color(color),
              AttrUtil.stop_opacity("0"),
            ],
            [],
          ),
          create_svg(
            "stop",
            [
              Attr.create(
                "offset",
                Printf.sprintf("%f%%", 100. *. extra_tail /. range),
              ),
              AttrUtil.stop_color(color),
            ],
            [],
          ),
          create_svg(
            "stop",
            [
              Attr.create(
                "offset",
                Printf.sprintf("%f%%", 100. *. (1. -. extra_tail /. range)),
              ),
              AttrUtil.stop_color(color),
            ],
            [],
          ),
          create_svg(
            "stop",
            [
              Attr.create("offset", "100%"),
              AttrUtil.stop_color(color),
              AttrUtil.stop_opacity("0"),
            ],
            [],
          ),
        ],
      );
    (id, gradient);
  };

  let fill_gradient =
    gradient("tessera-fill-gradient", "var(--unsorted-bg-color)");
  let stroke_gradient =
    gradient("tessera-stroke-gradient", "var(--unsorted-shadow-color)");

  let raised_shadow_filter = {
    Node.create_svg(
      "filter",
      [Attr.id("raised-drop-shadow-tessera")],
      [
        Node.create_svg(
          "feDropShadow",
          [
            Attr.classes(["tessera-drop-shadow"]),
            Attr.create("dx", raised_shadow_dx),
            Attr.create("dy", raised_shadow_dy),
            Attr.create("stdDeviation", "0"),
            Attr.create("flood-color", "var(--unsorted-shadow-color)"),
          ],
          [],
        ),
      ],
    );
  };
  let shadow_filter = {
    Node.create_svg(
      "filter",
      [Attr.id("drop-shadow-tessera")],
      [
        Node.create_svg(
          "feDropShadow",
          [
            Attr.classes(["tessera-drop-shadow"]),
            Attr.create("dx", shadow_dx),
            Attr.create("dy", shadow_dy),
            Attr.create("stdDeviation", "0"),
            Attr.create("flood-color", "var(--unsorted-shadow-color)"),
          ],
          [],
        ),
      ],
    );
  };

  let view = (profile: profile): list(Node.t) => {
    open SvgUtil.Path;
    open Diag;
    let closed_child_paths =
      List.map(closed_child_path, profile.closed_children);
    let jagged_edge_h = child_border_thickness /. 3.;
    let jagged_edge_w = child_border_thickness /. 1.;
    let (left_tip, right_tip) = {
      let left_tip = connects_left => {
        let bottom_half =
          connects_left
            ? [
                H_({dx: -. (extra_tail +. 0.5)}),
                L_({dx: -. jagged_edge_w, dy: -. jagged_edge_h}),
                L_({dx: jagged_edge_w, dy: -. jagged_edge_h}),
                L_({dx: -. jagged_edge_w, dy: -. jagged_edge_h}),
                // V_({dy: -. child_border_thickness}),
                H_({dx: jagged_edge_w +. extra_tail}),
              ]
              @ bl_tr(~child_border=`South, ())
            : [H_({dx: Float.neg(tip_width)}), ...bl_tr()];
        bottom_half @ br_tl() @ [H_({dx: tip_width})];
      };
      let right_tip = connects_right => {
        let bottom_half =
          connects_right
            ? tl_br(~child_border=`South, ())
              @ [
                H_({dx: extra_tail}),
                L_({dx: jagged_edge_w, dy: jagged_edge_h}),
                L_({dx: -. jagged_edge_w, dy: jagged_edge_h}),
                L_({dx: jagged_edge_w, dy: jagged_edge_h}),
                // V_({dy: child_border_thickness}),
                H_({dx: Float.neg(jagged_edge_w +. extra_tail +. 0.5)}),
              ]
            : tl_br() @ [H_({dx: Float.neg(tip_width)})];
        [H_({dx: tip_width}), ...tr_bl()] @ bottom_half;
      };
      switch (profile.shape) {
      | Op () => (br_tl() @ bl_tr(), tl_br() @ tr_bl())
      | Pre(connects_right) => (
          br_tl() @ bl_tr(),
          right_tip(connects_right),
        )
      | Post(connects_left) => (left_tip(connects_left), tl_br() @ tr_bl())
      | Bin((connects_left, connects_right)) => (
          left_tip(connects_left),
          right_tip(connects_right),
        )
      };
    };
    let start = profile.style.stretched ? Float.neg(stretch_dx) : 0.;
    let end_ =
      profile.style.stretched
        ? Float.of_int(profile.len) +. stretch_dx : Float.of_int(profile.len);
    let outer_path =
      List.concat([
        [M({x: start, y: 1.}), ...left_tip],
        [H({x: end_}), ...right_tip],
        [Z],
      ]);
    let clss = {
      let highlighted = profile.style.highlighted ? ["highlighted"] : [];
      let raised = profile.style.raised ? ["raised"] : [];
      List.concat([["tessera-path"], highlighted, raised]);
    };
    let (_fill_gradient_id, fill_gradient) = fill_gradient(profile);
    let (_stroke_gradient_id, stroke_gradient) = stroke_gradient(profile);
    let path =
      SvgUtil.Path.view(
        ~attrs=
          Attr.[
            classes(clss),
            create("vector-effect", "non-scaling-stroke"),
          ],
        /*create("fill", "var(--unsorted-bg-color)"),*/
        /*create("stroke", "var(--unsorted-shadow-color)"),*/
        outer_path @ List.concat(closed_child_paths),
      );
    [fill_gradient, stroke_gradient, path];
  };
};

module Tile = {
  [@deriving sexp]
  type profile = {
    shape: Layout.tile_shape,
    style: Layout.tile_style,
    len: int,
    open_children: list((int, int)),
    closed_children: list((int, int)),
    empty_holes: list(int),
  };

  let mk_profile =
      (
        ~open_children=[],
        ~closed_children=[],
        ~empty_holes=[],
        ~style=Layout.mk_tile_style(),
        ~len,
        ~shape,
        (),
      ) => {
    shape,
    len,
    open_children,
    closed_children,
    empty_holes,
    style,
  };

  let open_child_paths =
      (~start, ~sort: option(Sort.t), open_children: list((int, int)))
      : list(Node.t) => {
    open SvgUtil.Path;
    let color =
      switch (sort) {
      | None => "var(--unsorted-shadow-color)"
      | Some(Typ) => "var(--typ-shadow-color)"
      | Some(Pat) => "var(--pat-shadow-color)"
      | Some(Exp) => "var(--exp-shadow-color)"
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
              stop_opacity("0"),
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
              stop_opacity("0"),
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

  let contour_path = (~attrs: list(Attr.t), profile: profile): Node.t => {
    open SvgUtil.Path;
    open Diag;
    let closed_child_paths =
      List.map(closed_child_path, profile.closed_children);
    let outer_path = {
      let (left_tip, right_tip) =
        switch (profile.shape) {
        | Op(_) => (br_tl() @ bl_tr(), tl_br() @ tr_bl())
        | Pre () => (
            br_tl() @ bl_tr(),
            [H_({dx: tip_width}), ...tr_bl()]
            @ tl_br()
            @ [H_({dx: Float.neg(tip_width)})],
          )
        | Post () => (
            [H_({dx: Float.neg(tip_width)}), ...bl_tr()]
            @ br_tl()
            @ [H_({dx: tip_width})],
            tl_br() @ tr_bl(),
          )
        | Bin(_) => (
            [H_({dx: Float.neg(tip_width)}), ...bl_tr()]
            @ br_tl()
            @ [H_({dx: tip_width})],
            [H_({dx: tip_width}), ...tr_bl()]
            @ tl_br()
            @ [H_({dx: Float.neg(tip_width)})],
          )
        };
      let open_child_contours =
        profile.open_children
        |> List.map(((start, len)) =>
             List.concat([
               [H({x: Float.of_int(start) +. tip_width})],
               tr_bl(),
               tl_br(~child_border=`South, ()),
               [H_({dx: Float.of_int(len - 1)})],
               bl_tr(~child_border=`South, ()),
               br_tl(),
             ])
           )
        |> List.concat;
      let start = profile.style.stretched ? Float.neg(stretch_dx) : 0.;
      let end_ =
        profile.style.stretched
          ? Float.of_int(profile.len) +. stretch_dx
          : Float.of_int(profile.len);
      List.concat([
        [M({x: start, y: 1.}), ...left_tip],
        open_child_contours,
        [H({x: end_}), ...right_tip],
        [Z],
      ]);
    };
    let clss = {
      let sort =
        switch (profile.style.sort) {
        | None => "unsorted"
        | Some(s) => Sort.to_string(s)
        };
      let highlighted = profile.style.highlighted ? ["highlighted"] : [];
      let raised = profile.style.raised ? ["raised"] : [];
      List.concat([["tile-path", sort], highlighted, raised]);
    };
    SvgUtil.Path.view(
      ~attrs=
        Attr.[
          classes(clss),
          create("vector-effect", "non-scaling-stroke"),
          ...attrs,
        ],
      outer_path @ List.concat(closed_child_paths),
    );
  };

  let raised_shadow_filter = (~sort: option(Sort.t)=?, ()) => {
    let s =
      switch (sort) {
      | None => "unsorted"
      | Some(s) => Sort.to_string(s)
      };
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

  let shadow_filter = (~sort: option(Sort.t)=?, ()) => {
    let s =
      switch (sort) {
      | None => "unsorted"
      | Some(s) => Sort.to_string(s)
      };
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

  let view =
      (
        ~attrs: list(Attr.t)=[],
        ~font_metrics: FontMetrics.t,
        ~start: int,
        profile: profile,
      )
      : list(Node.t) => {
    let empty_holes =
      switch (profile.shape) {
      | Op(true)
      | Bin(true) =>
        let inset_style = profile.style.raised ? `Thick : `Thin;
        EmptyHole.view(
          ~offset=0,
          ~font_metrics,
          ~inset=Some(inset_style),
          (),
        );
      | _ =>
        // TODO review this behavior and check if empty holes getting rendered
        profile.style.show_children
          ? []
          : profile.empty_holes
            |> List.map(offset =>
                 EmptyHole.view(
                   ~offset,
                   ~font_metrics,
                   ~inset=Some(`Thin),
                   (),
                 )
               )
            |> List.flatten
      };
    // TODO maybe remove this flag and just specify via children fields?
    let profile =
      profile.style.show_children
        ? profile : {...profile, open_children: [], closed_children: []};
    let open_child_paths =
      open_child_paths(
        ~start,
        ~sort=profile.style.sort,
        profile.open_children,
      );
    open_child_paths @ [contour_path(~attrs, profile), ...empty_holes];
  };
};

module Selection = {
  let view =
      (
        ~font_metrics: FontMetrics.t,
        style: Layout.selection_style,
        start,
        len,
      ) =>
    Node.div(
      [
        Attr.classes([
          "selection-box",
          ...style.unfocused ? ["unfocused"] : [],
        ]),
        Attr.create(
          "style",
          Printf.sprintf(
            "left: %fpx; top: %fpx; width: %fpx; height: %fpx;",
            (Float.of_int(start) +. 0.5) *. font_metrics.col_width,
            (-0.1) *. font_metrics.row_height,
            Float.of_int(len - 1) *. font_metrics.col_width,
            // not sure why this needs to be 1.3 and not 1.2
            1.3 *. font_metrics.row_height,
          ),
        ),
      ],
      [],
    );
};

module Caret = {
  let action_type = txt =>
    Node.div([Attr.classes(["action-type"])], [Node.text(txt)]);

  let construct_shape = txt =>
    Node.div([Attr.classes(["construct-shape"])], [Node.text(txt)]);

  let key = txt => Node.div([Attr.classes(["key"])], [Node.text(txt)]);

  let keys_container = keys =>
    Node.div(
      [Attr.classes(["keys-container"])],
      [Node.div([Attr.classes(["keys"])], keys)],
    );
  let keys = ks => keys_container(List.map(key, ks));

  let move_row = [
    keys([Unicode.left_arrow, Unicode.right_arrow]),
    action_type("Move"),
  ];

  let select_row = [
    keys_container([
      key("Shift"),
      Node.div([], [Node.text("+")]),
      key(Unicode.left_arrow),
      key(Unicode.right_arrow),
    ]),
    action_type("Select"),
  ];

  let delete_row = [
    keys(["Backspace", "Delete"]),
    action_type("Delete/Restructure"),
  ];

  let undo_row = [
    keys_container([
      key("Ctrl"),
      Node.div([], [Node.text("+")]),
      key("z"),
    ]),
    action_type("Undo"),
  ];
  let redo_row = [
    keys_container([
      key("Ctrl"),
      Node.div([], [Node.text("+")]),
      key("Shift"),
      Node.div([], [Node.text("+")]),
      key("z"),
    ]),
    action_type("Redo"),
  ];

  let buffer_cell = Node.div([], []);
  let buffer_row = [buffer_cell, buffer_cell];

  let construct_rows =
    List.concat(
      Util.ListUtil.join(
        buffer_row,
        [
          [buffer_cell, action_type("Construct")],
          [
            keys_container([
              key("0"),
              Node.div([], [Node.text("-")]),
              key("9"),
            ]),
            construct_shape("single-digit num"),
          ],
          [
            keys_container([
              key("a"),
              Node.div([], [Node.text("-")]),
              key("z"),
            ]),
            construct_shape("single-char var"),
          ],
          [keys(["+"]), construct_shape("plus")],
          [keys(["("]), construct_shape("parentheses")],
          [keys(["\\"]), construct_shape("lambda")],
          [keys(["="]), construct_shape("let expression")],
        ],
      ),
    );

  let typ =
    Node.span([Attr.classes(["sort-label", "typ"])], [Node.text("type")]);
  let pat =
    Node.span(
      [Attr.classes(["sort-label", "pat"])],
      [Node.text("pattern")],
    );
  let exp =
    Node.span(
      [Attr.classes(["sort-label", "exp"])],
      [Node.text("expression")],
    );
  let inconsistent =
    Node.span(
      [Attr.classes(["inconsistent"])],
      [Node.text("inconsistent")],
    );

  let view =
      (
        ~font_metrics: FontMetrics.t,
        ~type_font_metrics: FontMetrics.t,
        ~view_of_layout:
           (~font_metrics: FontMetrics.t, ~transparent: bool=?, Layout.t) =>
           Node.t,
        ~show_type_info: bool,
        offset: int,
        caret: Layout.caret,
      ) => {
    let popped_top = (-1.4) *. font_metrics.row_height;
    let (top, (ss_before, ss_after)) =
      switch (caret) {
      | Pointing(_todo) => (0., ([], []))
      | Selecting => (0., ([], []))
      | Restructuring(selection, (prefix, suffix)) =>
        let ss_before =
          List.rev(prefix)
          |> List.map(s =>
               Node.span(
                 [
                   Attr.create(
                     "style",
                     Printf.sprintf(
                       "margin-left: %fpx;",
                       (-0.5) *. font_metrics.col_width,
                     ),
                   ),
                 ],
                 [view_of_layout(~font_metrics, ~transparent=true, s)],
               )
             );
        let ss_after =
          [selection, ...suffix]
          |> List.mapi((i, s) =>
               Node.span(
                 [
                   Attr.create(
                     "style",
                     Printf.sprintf(
                       "margin-right: %fpx;",
                       (-0.5) *. font_metrics.col_width,
                     ),
                   ),
                 ],
                 [view_of_layout(~font_metrics, ~transparent=i != 0, s)],
               )
             );
        (popped_top, (ss_before, ss_after));
      };

    let expecting_type = (~expected=?, ()) => {
      switch (expected) {
      | None => Node.div([], [Node.text("expecting any type")])
      | Some(expected) =>
        Node.div(
          [],
          [
            Node.text("expecting type "),
            view_of_layout(
              ~font_metrics=type_font_metrics,
              Layout_term.Typ.mk(Core.Term_typ.of_type(expected)),
            ),
          ],
        )
      };
    };
    let got_type = (~expected=?, syn_ty) => {
      let label =
        switch (expected) {
        | None => Node.text("got type ")
        | Some(ty) =>
          Core.Type.consistent(ty, syn_ty)
            ? Node.text("got consistent type ")
            : Node.span(
                [],
                [Node.text("got "), inconsistent, Node.text(" type ")],
              )
        };
      Node.div(
        [],
        [
          label,
          view_of_layout(
            ~font_metrics=type_font_metrics,
            Layout_term.Typ.mk(Core.Term_typ.of_type(syn_ty)),
          ),
        ],
      );
    };
    let type_info =
      if (show_type_info) {
        switch (caret) {
        | Selecting
        | Restructuring(_) => []
        | Pointing(info) =>
          let lines =
            switch (info) {
            | Typ => [
                Node.div(
                  [Attr.classes(["type-info-msg"])],
                  [Node.div([], [typ])],
                ),
              ]
            | Pat(info, syn_ty) => [
                Node.div(
                  [Attr.classes(["type-info-msg"])],
                  [
                    Node.div([], [pat]),
                    ...switch (info.mode) {
                       | Syn(_)
                       | Let_pat(_) => [expecting_type(), got_type(syn_ty)]
                       | Ana(ty, _) => [
                           expecting_type(~expected=ty, ()),
                           got_type(~expected=ty, syn_ty),
                         ]
                       },
                  ],
                ),
              ]
            | Exp(info, syn_ty) => [
                Node.div(
                  [Attr.classes(["type-info-msg"])],
                  [
                    Node.div([], [exp]),
                    ...switch (info.mode) {
                       | Syn(_) => [expecting_type(), got_type(syn_ty)]
                       | Ana(ty, _) => [
                           expecting_type(~expected=ty, ()),
                           got_type(~expected=ty, syn_ty),
                         ]
                       | Fn_pos(_) => failwith("caret fn_pos todo")
                       },
                  ],
                ),
              ]
            };
          [
            Node.div(
              [
                Attr.id("type-info"),
                /*
                 Attr.create(
                   "style",
                   Printf.sprintf(
                     "top: %fpx;",
                     (-1.2) *. font_metrics.row_height,
                   ),
                 ),
                 */
              ],
              lines,
            ),
          ];
        };
      } else {
        [];
      };
    Node.div(
      [
        Attr.id("caret"),
        Attr.create(
          "style",
          Printf.sprintf(
            "top: %fpx; left: %fpx",
            top,
            (Float.of_int(offset) +. 0.5) *. font_metrics.col_width,
          ),
        ),
      ],
      [
        Node.div([Attr.id("caret-bar")], []),
        Node.div(
          [
            Attr.id("action-table"),
            Attr.create(
              "style",
              Printf.sprintf("top: %fpx;", Float.of_int(80) -. top),
            ),
          ],
          List.concat([
            move_row,
            buffer_row,
            select_row,
            buffer_row,
            delete_row,
            buffer_row,
            construct_rows,
            buffer_row,
            undo_row,
            buffer_row,
            redo_row,
          ]),
        ),
        Node.div(
          [
            Attr.id("backpack-pre"),
            Attr.create("style", Printf.sprintf("top: 0; right: 100%%;")),
          ],
          ss_before,
        ),
        Node.div(
          [
            Attr.id("backpack-suf"),
            Attr.create(
              "style",
              Printf.sprintf(
                "top: 0; left: %fpx;",
                (-0.5) *. font_metrics.col_width,
              ),
            ),
          ],
          ss_after,
        ),
        ...type_info,
      ],
    );
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
  let buffered_height = 2;
  let buffered_width = length + 3;

  let buffered_height_px =
    Float.of_int(buffered_height) *. font_metrics.row_height;
  let buffered_width_px =
    Float.of_int(buffered_width) *. font_metrics.col_width;

  let container_origin_x =
    (Float.of_int(origin) -. 1.5) *. font_metrics.col_width;
  let container_origin_y = (-0.5) *. font_metrics.row_height;

  Node.div(
    [
      Attr.classes([
        "decoration-container",
        Printf.sprintf("%s-container", cls),
        ...container_clss,
      ]),
      Attr.create(
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
        [
          Attr.classes([cls]),
          Attr.create(
            "viewBox",
            Printf.sprintf(
              "-1.5 -0.5 %d %d",
              buffered_width,
              buffered_height,
            ),
          ),
          Attr.create("width", Printf.sprintf("%fpx", buffered_width_px)),
          Attr.create("height", Printf.sprintf("%fpx", buffered_height_px)),
          Attr.create("preserveAspectRatio", "none"),
        ],
        svgs,
      ),
    ],
  );
};
