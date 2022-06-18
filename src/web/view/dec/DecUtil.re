open Virtual_dom.Vdom;

let tip_width = 0.32;
let child_border_thickness = 0.05;

let t = child_border_thickness /. 0.5;
let short_tip_height = (1. -. t) *. 0.5;

let stretch_dx = 0.15;

let raised_shadow_dx = "0.1";
let raised_shadow_dy = "0.03";
let shadow_dx = "0.12";
let shadow_dy = "0.05";

let extra_tail = 0.;
let jagged_edge_h = child_border_thickness /. 3.;
let jagged_edge_w = child_border_thickness /. 1.;

let short_tip_width = (1. -. t) *. tip_width;

let hole_radii = (~font_metrics: FontMetrics.t) => {
  let r = 3.5;
  (r /. font_metrics.col_width, r /. font_metrics.row_height);
};

//TODO(andrew): deprecate
let container =
    (
      ~font_metrics: FontMetrics.t,
      ~measurement as {origin, length}: Core.Measured.measurement_lin,
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
          "top: calc(%fpx); left: %fpx;",
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

let container2d =
    (
      ~font_metrics: FontMetrics.t,
      ~measurement: Core.Measured.measurement,
      ~cls: string,
      ~container_clss=[],
      svgs: list(Node.t),
    )
    : Node.t => {
  let origin_x = measurement.origin.col;
  let origin_y = measurement.origin.row;
  let length_lin = measurement.last.col - measurement.origin.col;

  let buffered_height = measurement.last.row - measurement.origin.row + 2;
  let buffered_width = length_lin + 3;

  let buffered_height_px =
    Float.of_int(buffered_height) *. font_metrics.row_height;
  let buffered_width_px =
    Float.of_int(buffered_width) *. font_metrics.col_width;

  let container_origin_x =
    (Float.of_int(origin_x) -. 1.5) *. font_metrics.col_width;
  let container_origin_y =
    (Float.of_int(origin_y) -. 3.5) *. font_metrics.row_height;

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
          "top: calc(%fpx); left: %fpx;",
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
