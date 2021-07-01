open Virtual_dom.Vdom;
open Util;

module Profile = {
  type t = {
    measurement: Layout.measurement,
    ends: (Color.t, Color.t),
  };
};

let view =
    (
      ~font_metrics: FontMetrics.t,
      {measurement, ends: (color_l, color_r)}: Profile.t,
    ) => {
  let (cls_l, cls_r) =
    TupleUtil.map2(c => Color.(to_string(c)), (color_l, color_r));
  let len = measurement.length;
  let vs =
    if (color_l == color_r) {
      [
        Node.create_svg(
          "line",
          Attr.[
            create("x1", "0.5"),
            create("y1", "-0.3"),
            create("x2", Printf.sprintf("%fpx", Float.of_int(len) +. 0.5)),
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
  DecUtil.container(~font_metrics, ~measurement, ~cls="selected-bar", vs);
};
