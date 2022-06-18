open Virtual_dom.Vdom;

module Profile = {
  type style = [ | `Bare | `Sibling | `Anchor | `Caret];
  type t = {
    style,
    measurement: Core.Measured.measurement,
    color: Color.t,
  };
};

let blur_filter =
  Node.create_svg(
    "filter",
    [Attr.id("caret-position-neighbor-blur")],
    [
      Node.create_svg(
        "feGaussianBlur",
        [
          Attr.create("in", "SourceGraphic"),
          Attr.create("stdDeviation", "0"),
        ],
        [],
      ),
    ],
  );

let caret_position_radii =
    (~font_metrics: FontMetrics.t, ~style: Profile.style) => {
  let r =
    switch (style) {
    | `Caret => 3.75
    | `Anchor
    | `Sibling => 2.75
    | `Bare => 2.0
    };
  (r /. font_metrics.col_width, r /. font_metrics.row_height);
};

let view = (~font_metrics, {style, color, measurement}: Profile.t) => {
  let (r_x, r_y) = caret_position_radii(~font_metrics, ~style);
  let c_cls = Color.to_string(color);
  let cls =
    switch (style) {
    | `Bare => "outer-cousin"
    | `Caret => "current-caret-pos"
    | `Anchor => "anchor"
    | `Sibling => "sibling"
    };
  DecUtil.container2d(
    ~font_metrics,
    ~measurement,
    ~cls,
    [
      Node.create_svg(
        "rect",
        Attr.[
          create("x", Printf.sprintf("%fpx", -. r_x)),
          create("y", Printf.sprintf("%fpx", 0.1 -. r_y)),
          create("width", Printf.sprintf("%fpx", 1. *. r_x)),
          create("height", Printf.sprintf("%fpx", 1. *. r_y)),
          Attr.classes(["caret-position-path", cls, c_cls]),
        ],
        [],
      ),
    ],
  );
};
