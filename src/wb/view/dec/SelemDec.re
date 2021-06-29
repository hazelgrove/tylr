open Virtual_dom.Vdom;
open Util;
open Cor;
open Diag;
open DecUtil;

module Profile = {
  type t = {
    measurement: Layout.measurement,
    color: Color.t,
    shape: Layout.selem_shape,
    style: SelemStyle.t,
    open_children: list(Layout.measurement),
    closed_children: list(Layout.measurement),
    empty_holes: list((int, Color.t, Tip.shape)),
  };
  let mk =
      (
        ~measurement,
        ~open_children=[],
        ~closed_children=[],
        ~empty_holes=[],
        ~color,
        ~style,
        ~shape,
        (),
      ) => {
    measurement,
    color,
    shape,
    open_children,
    closed_children,
    empty_holes,
    style,
  };
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

let closed_child_path = ({origin, length}: Layout.measurement) =>
  List.concat(
    SvgUtil.Path.[
      [M({x: Float.of_int(origin) +. 0.5, y: child_border_thickness})],
      Diag.tr_bl(~with_child_border=true, ~hemi=`North, ()),
      Diag.tl_br(~with_child_border=true, ~hemi=`South, ()),
      [H_({dx: Float.of_int(length - 1)})],
      Diag.bl_tr(~with_child_border=true, ~hemi=`South, ()),
      Diag.br_tl(~with_child_border=true, ~hemi=`North, ()),
      [Z],
    ],
  );

let open_child_paths =
    (~origin, ~color: Color.t, open_children: list(Layout.measurement))
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
          AttrUtil.[offset(1.25 /. Float.of_int(len)), stop_color(color)],
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
  |> List.map((Layout.{origin: origin', length}) => {
       let gradient_id =
         Printf.sprintf(
           "bidelimited-open-child-gradient-%d-%d",
           origin,
           origin',
         );
       [
         gradient(gradient_id, origin', length),
         view(
           ~attrs=
             Attr.[
               classes(["bidelimited-open-child-path"]),
               AttrUtil.vector_effect("non-scaling-stroke"),
               create("stroke", Printf.sprintf("url(#%s)", gradient_id)),
             ],
           [
             M({x: Float.of_int(origin'), y: 0.}),
             H_({dx: Float.of_int(length)}),
           ],
         ),
       ];
     })
  |> List.flatten;
};

let empty_hole_path =
    (~font_metrics as _, empty_hole: (int, Color.t, Tip.shape))
    : SvgUtil.Path.t => {
  let (offset, _color, tip) = empty_hole;
  EmptyHoleDec.path(tip, Float.of_int(offset), 0.28);
};

let open_child_path = ({origin, length}: Layout.measurement) =>
  List.concat(
    SvgUtil.Path.[
      [H({x: Float.of_int(origin) +. tip_width})],
      tr_bl(~hemi=`North, ()),
      tl_br(~with_child_border=true, ~hemi=`South, ()),
      [H_({dx: Float.of_int(length - 1)})],
      bl_tr(~with_child_border=true, ~hemi=`South, ()),
      br_tl(~hemi=`North, ()),
    ],
  );

let contour_path = (~font_metrics, profile: Profile.t): SvgUtil.Path.t => {
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
        ? Float.of_int(profile.measurement.length) +. stretch_dx
        : Float.of_int(profile.measurement.length);
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

let contour_path_attrs = (profile: Profile.t) => {
  let clss = {
    let c_cls = Color.to_string(profile.color);
    let highlighted =
      SelemStyle.highlighted(profile.style) ? ["highlighted"] : [];
    let filtered = SelemStyle.filtered(profile.style) ? ["filtered"] : [];
    let raised = ["raised"]; // profile.style.raised ? ["raised"] : [];
    List.concat([["tile-path", c_cls], highlighted, raised, filtered]);
  };
  Attr.[classes(clss), create("vector-effect", "non-scaling-stroke")];
};

let view = (~font_metrics: FontMetrics.t, profile: Profile.t): Node.t => {
  // TODO maybe remove this flag and just specify via children fields?
  let profile =
    SelemStyle.show_children(profile.style)
      ? profile : {...profile, open_children: [], closed_children: []};
  let open_child_paths =
    open_child_paths(
      ~origin=profile.measurement.origin,
      ~color=profile.color,
      profile.open_children,
    );
  DecUtil.container(
    ~font_metrics,
    ~measurement=profile.measurement,
    ~cls="tile",
    ~container_clss=[SelemStyle.to_string(profile.style)],
    open_child_paths
    @ [
      SvgUtil.Path.view(
        ~attrs=contour_path_attrs(profile),
        contour_path(~font_metrics, profile),
      ),
    ],
  );
};
