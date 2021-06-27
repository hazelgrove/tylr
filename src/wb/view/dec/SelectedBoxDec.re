open Virtual_dom.Vdom;

let view = (~font_metrics: FontMetrics.t, ~start, ~len) => {
  Node.div(
    [
      Attr.classes(["selection-box"]),
      Attr.create(
        "style",
        Printf.sprintf(
          "left: %fpx; top: %fpx; width: %fpx; height: %fpx;",
          (Float.of_int(start) +. 0.5) *. font_metrics.col_width,
          (-0.2) *. font_metrics.row_height,
          Float.of_int(len - 1) *. font_metrics.col_width,
          // not sure why this needs to be 1.3 and not 1.2
          1.6 *. font_metrics.row_height,
        ),
      ),
    ],
    [],
  );
};
