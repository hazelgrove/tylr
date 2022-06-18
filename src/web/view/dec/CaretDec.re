open Virtual_dom.Vdom;
open Util;
open Node;
open SvgUtil;

let height_fudge = 1.0; //extra 1.0 for piece deco shadow

let caret_width_bent = 0.125;
let caret_width_straight = 0.1;
let caret_bend = DecUtil.tip_width;
let caret_color = "#f62116";

module Profile = {
  type t = {
    side: Direction.t,
    origin: Core.Measured.point,
    shape: option(Direction.t),
  };
};

let caret_path = (shape: option(Direction.t)) => {
  let caret_bend_param =
    switch (shape) {
    | Some(Right) => -. caret_bend
    | Some(Left) => caret_bend
    | None => 0.0
    };
  let caret_width_param =
    switch (shape) {
    | Some(Right) => -. caret_width_bent
    | Some(Left) => caret_width_bent
    | None => caret_width_straight
    };
  Path.[
    m(~x=0, ~y=0),
    H({x: caret_width_param}),
    L_({dx: -. caret_bend_param, dy: 0.5}),
    L_({dx: +. caret_bend_param, dy: 0.5}),
    H({x: -. caret_width_param}),
    L_({dx: -. caret_bend_param, dy: (-0.5)}),
    L_({dx: +. caret_bend_param, dy: (-0.5)}),
  ];
};

let caret_position =
    (
      ~profile as {shape, side, origin}: Profile.t,
      ~font_metrics: FontMetrics.t,
    ) => {
  // TODO(andrew): BUG, fix alignment
  let left_fudge =
    switch (side, shape) {
    | (Left, Some(Left)) => 3.0
    | (Left, Some(Right)) => (-1.0)
    | _ => 0.0
    };
  Attr.create(
    "style",
    Printf.sprintf(
      "position: absolute; z-index: 6; left: %fpx; top: %fpx; width: %fpx; height: %fpx;",
      Float.of_int(origin.col) *. font_metrics.col_width +. left_fudge,
      Float.of_int(origin.row) *. font_metrics.row_height,
      font_metrics.col_width,
      font_metrics.row_height +. height_fudge,
    ),
  );
};

let view = (~font_metrics: FontMetrics.t, ~profile: Profile.t) =>
  create_svg(
    "svg",
    Attr.[
      caret_position(~profile, ~font_metrics),
      create("viewBox", Printf.sprintf("0 0 1 1")),
      create("preserveAspectRatio", "none"),
    ],
    [
      SvgUtil.Path.view(
        ~attrs=
          Attr.[
            create("vector-effect", "non-scaling-stroke"),
            create("style", Printf.sprintf("fill: %s;", caret_color)),
          ],
        caret_path(profile.shape),
      ),
    ],
  );
