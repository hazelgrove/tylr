let tip_width = 0.32;
let child_border_thickness = 0.1;

let t = child_border_thickness /. 0.5;
let short_tip_height = (1. -. t) *. 0.5;

let stretch_dx = 0.15;

let raised_shadow_dx = "0.1";
let raised_shadow_dy = "0.03";
let shadow_dx = "0.06";
let shadow_dy = "0.024";

let extra_tail = 0.;
let jagged_edge_h = child_border_thickness /. 3.;
let jagged_edge_w = child_border_thickness /. 1.;

let short_tip_width = (1. -. t) *. tip_width;

let hole_radii = (~font_metrics: FontMetrics.t) => {
  let r = 3.5;
  (r /. font_metrics.col_width, r /. font_metrics.row_height);
};

let caret_position_radii =
    (
      ~font_metrics: FontMetrics.t,
      ~style: [ | `Anchor | `Sibling | `InnerCousin | `OuterCousin],
    ) => {
  let r =
    switch (style) {
    | `Anchor => 3.5
    | `Sibling => 2.75
    | `InnerCousin
    | `OuterCousin => 2.
    };
  (r /. font_metrics.col_width, r /. font_metrics.row_height);
};
