/*open Virtual_dom.Vdom;
  open Util;
  open Core;

  module Profile = {
    type t = {
      measurement: Measured.measurement_lin,
      sort: Sort.t,
      side: Direction.t,
    };
  };

  let view = (~font_metrics, {measurement, sort, side}: Profile.t): Node.t => {
    open SvgUtil.Path;
    open Diag;
    let len = Float.of_int(measurement.length);
    let (gradient_id, gradient) = {
      let id =
        switch (side) {
        | Left => "uni-child-gradient-left"
        | Right => "uni-child-gradient-right"
        };
      let (x1, x2) =
        switch (side) {
        | Left => ("1", Printf.sprintf("%f", len +. 1.))
        | Right => (Printf.sprintf("%f", len), "0")
        };
      let color =
        switch (sort) {
        // TODO(d)
        | Any => ""
        | Pat => "var(--pat-shadow-color)"
        | Exp => "var(--exp-shadow-color)"
        };
      let gradient =
        Node.create_svg(
          "linearGradient",
          [
            Attr.id(id),
            Attr.create("gradientUnits", "userSpaceOnUse"),
            Attr.create("x1", x1),
            Attr.create("x2", x2),
          ],
          [
            Node.create_svg(
              "stop",
              [Attr.create("offset", "0%"), AttrUtil.stop_color(color)],
              [],
            ),
            Node.create_svg(
              "stop",
              [
                Attr.create(
                  "offset",
                  Printf.sprintf("%f%%", 100. *. (len -. 1.25) /. len),
                ),
                AttrUtil.stop_color(color),
              ],
              [],
            ),
            Node.create_svg(
              "stop",
              [
                Attr.create(
                  "offset",
                  Printf.sprintf("%f%%", 100. *. (len -. 0.6) /. len),
                ),
                AttrUtil.stop_color(color),
                AttrUtil.stop_opacity(0.),
              ],
              [],
            ),
          ],
        );
      (id, gradient);
    };
    let path =
      switch (side) {
      | Left =>
        List.concat([
          [M({x: len +. 1., y: 0.}), H_({dx: Float.neg(len)})],
          // [M({x: 0., y: 0.})],
          tr_bl(~hemi=`North, ()),
          tl_br(~hemi=`South, ()),
          [H_({dx: len})],
          [M_({dx: 0., dy: 0.02}), H_({dx: Float.neg(len)})],
        ])
      | Right =>
        List.concat([
          [M({x: 0., y: 0.}), H_({dx: len})],
          // [M({x: len, y: 0.})],
          tl_br(~hemi=`North, ()),
          tr_bl(~hemi=`South, ()),
          [H_({dx: Float.neg(len)})],
          [M_({dx: 0., dy: 0.02}), H_({dx: len})],
          // bl_tr(),
          // br_tl(),
        ])
      };
    let attrs =
      Attr.[
        classes(["uni-child-path", Sort.to_string(sort)]),
        create("vector-effect", "non-scaling-stroke"),
        create("stroke", Printf.sprintf("url(#%s)", gradient_id)),
      ];
    DecUtil.container(
      ~font_metrics,
      ~measurement,
      ~cls="uni-child",
      [gradient, view(~attrs, path)],
    );
  };
  */
