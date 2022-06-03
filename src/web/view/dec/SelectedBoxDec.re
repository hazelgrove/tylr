open Virtual_dom.Vdom;

module Profile = {
  type t = Measured.measurement_lin;
};

let view = (~font_metrics: FontMetrics.t, profile: Profile.t) => {
  Node.div(
    [
      Attr.classes(["selection-box"]),
      Attr.create(
        "style",
        Printf.sprintf(
          "left: %fpx; top: %fpx; width: %fpx; height: %fpx;",
          (Float.of_int(profile.origin) -. 0.5) *. font_metrics.col_width,
          (-0.25) *. font_metrics.row_height,
          Float.of_int(profile.length) *. font_metrics.col_width,
          // not sure why this needs to be 1.6 and not 1.5
          1.6 *. font_metrics.row_height,
        ),
      ),
    ],
    [],
  );
};
