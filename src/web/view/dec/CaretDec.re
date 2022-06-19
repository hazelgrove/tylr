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
  let left_fudge =
    switch (side, shape) {
    | (Left, Some(Left)) => 3.5
    | (Right, Some(Right)) => (-3.5)
    | _ => 0.0
    };
  DecUtil.code_svg(
    ~font_metrics,
    ~origin,
    ~base_cls=["caret"],
    ~path_cls=["caret-path"],
    ~height_fudge=1.0, //extra 1.0 for piece deco shadow,
    ~left_fudge,
    caret_path(shape),
  );
};
