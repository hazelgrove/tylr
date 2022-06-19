open Util;
open SvgUtil;

let caret_width_straight = 0.1;
let caret_width_bent = 0.1;
let caret_bend = DecUtil.tip_width;

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

let view =
    (
      ~font_metrics: FontMetrics.t,
      ~profile as {shape, side, origin}: Profile.t,
    ) => {
  let l_adj =
    switch (side, shape) {
    | (_, None) => 0.
    | (Left, Some(Left)) => DecUtil.concave_adj
    | (Right, Some(Right)) => -. DecUtil.concave_adj
    | (Left, Some(Right)) => DecUtil.convex_adj
    | (Right, Some(Left)) => -. DecUtil.convex_adj
    };
  DecUtil.code_svg(
    ~font_metrics,
    ~origin,
    ~base_cls=["caret"],
    ~path_cls=["caret-path"],
    ~height_fudge=DecUtil.shadow_adj *. font_metrics.row_height,
    ~left_fudge=l_adj *. font_metrics.col_width,
    caret_path(shape),
  );
};
