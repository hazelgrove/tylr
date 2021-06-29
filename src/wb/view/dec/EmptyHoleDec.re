open Virtual_dom.Vdom;
open DecUtil;
open Cor;

module Profile = {
  type t = {
    measurement: Layout.measurement,
    color: Color.t,
    tip: Tip.shape,
    inset: option([ | `Thick | `Thin]),
  };
};

let inset_shadow_filter = (~color) => {
  let c_cls = Color.to_string(color);
  Node.create_svg(
    "filter",
    [Attr.id(Printf.sprintf("empty-hole-inset-shadow-%s", c_cls))],
    [
      Node.create_svg(
        "feOffset",
        Attr.[
          create("in", "SourceAlpha"),
          create("dx", raised_shadow_dx),
          create("dy", raised_shadow_dy),
          create("result", "offset-alpha"),
        ],
        [],
      ),
      Node.create_svg(
        "feFlood",
        Attr.[
          classes(["empty-hole-inset-shadow-flood", c_cls]),
          create("flood-opacity", "1"),
          create("result", "color"),
        ],
        [],
      ),
      Node.create_svg(
        "feComposite",
        Attr.[
          create("operator", "out"),
          create("in", "SourceAlpha"),
          create("in2", "offset-alpha"),
          create("result", "shadow-shape"),
        ],
        [],
      ),
      Node.create_svg(
        "feComposite",
        Attr.[
          create("operator", "in"),
          create("in", "color"),
          create("in2", "shadow-shape"),
          create("result", "drop-shadow"),
        ],
        [],
      ),
      Node.create_svg(
        "feMerge",
        [],
        [
          Node.create_svg(
            "feMergeNode",
            [Attr.create("in", "SourceGraphic")],
            [],
          ),
          Node.create_svg(
            "feMergeNode",
            [Attr.create("in", "drop-shadow")],
            [],
          ),
        ],
      ),
    ],
  );
};

let thin_inset_shadow_filter = (~color) => {
  let c_cls = Color.to_string(color);
  Node.create_svg(
    "filter",
    [Attr.id(Printf.sprintf("empty-hole-thin-inset-shadow-%s", c_cls))],
    [
      Node.create_svg(
        "feOffset",
        Attr.[
          create("in", "SourceAlpha"),
          create("dx", shadow_dx),
          create("dy", shadow_dy),
          create("result", "offset-alpha"),
        ],
        [],
      ),
      Node.create_svg(
        "feFlood",
        Attr.[
          classes(["empty-hole-inset-shadow-flood", c_cls]),
          create("flood-opacity", "1"),
          create("result", "color"),
        ],
        [],
      ),
      Node.create_svg(
        "feComposite",
        Attr.[
          create("operator", "out"),
          create("in", "SourceAlpha"),
          create("in2", "offset-alpha"),
          create("result", "shadow-shape"),
        ],
        [],
      ),
      Node.create_svg(
        "feComposite",
        Attr.[
          create("operator", "in"),
          create("in", "color"),
          create("in2", "shadow-shape"),
          create("result", "drop-shadow"),
        ],
        [],
      ),
      Node.create_svg(
        "feMerge",
        [],
        [
          Node.create_svg(
            "feMergeNode",
            [Attr.create("in", "SourceGraphic")],
            [],
          ),
          Node.create_svg(
            "feMergeNode",
            [Attr.create("in", "drop-shadow")],
            [],
          ),
        ],
      ),
    ],
  );
};

let path = (tip, offset, s: float) => {
  let x_dilate = 1.5;
  List.concat(
    SvgUtil.Path.[
      [
        M({x: offset +. 0.5, y: 0.5 -. s /. 2.}),
        H_({dx: x_dilate *. s /. 2.}),
      ],
      Diag.right_tip_path(~scale_x=s *. x_dilate, ~scale_y=s, (tip, 0)),
      [H_({dx: -. s *. x_dilate})],
      Diag.left_tip_path(~scale_x=s *. x_dilate, ~scale_y=s, (tip, 0)),
      [Z],
    ],
  );
};

let view =
    (~font_metrics, {measurement, color, tip, inset}: Profile.t): Node.t => {
  let c_cls = Color.to_string(color);
  container(
    ~font_metrics,
    ~measurement,
    ~cls="empty-hole",
    SvgUtil.Path.[
      view(
        ~attrs=
          AttrUtil.[
            Attr.classes(["empty-hole-path", c_cls]),
            vector_effect("non-scaling-stroke"),
            stroke_width(Option.is_some(inset) ? 0.3 : 1.),
            filter(
              switch (inset) {
              | None => "none"
              | Some(`Thin) =>
                Printf.sprintf("url(#empty-hole-thin-inset-shadow-%s)", c_cls)
              | Some(`Thick) =>
                Printf.sprintf("url(#empty-hole-inset-shadow-%s)", c_cls)
              },
            ),
          ],
        path(tip, 0., 0.28),
      ),
    ],
  );
};
