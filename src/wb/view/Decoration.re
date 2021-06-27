open Virtual_dom.Vdom;
open Util;

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
