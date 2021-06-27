open Virtual_dom.Vdom;
open DecUtil;

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

let view =
    (
      ~font_metrics,
      ~style: [ | `Anchor | `Sibling | `InnerCousin | `OuterCousin],
      color: Color.t,
    ) => {
  let (r_x, r_y) = caret_position_radii(~font_metrics, ~style);
  let c_cls = Color.to_string(color);
  let style_cls =
    switch (style) {
    | `Anchor => "anchor"
    | `Sibling => "sibling"
    | `InnerCousin => "inner-cousin"
    | `OuterCousin => "outer-cousin"
    };
  [
    Node.create_svg(
      "rect",
      Attr.[
        create("x", Printf.sprintf("%fpx", 0.5 -. r_x)),
        create("y", Printf.sprintf("%fpx", (-0.3) -. r_y)),
        create("width", Printf.sprintf("%fpx", 2. *. r_x)),
        create("height", Printf.sprintf("%fpx", 2. *. r_y)),
        Attr.classes(["caret-position-path", style_cls, c_cls]),
      ],
      [],
    ),
    // Node.create_svg(
    //   "ellipse",
    //   AttrUtil.[
    //     cx(0.5),
    //     cy(-0.3),
    //     rx(r_x),
    //     ry(r_y),
    //     Attr.classes(["caret-position-path", style_cls, c_cls]),
    //   ],
    //   [],
    // ),
  ];
};
