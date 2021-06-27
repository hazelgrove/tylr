open Virtual_dom.Vdom;

let view = (~length) => [
  // <feGaussianBlur in="SourceAlpha"
  //             stdDeviation="4"
  //             result="blur"/>
  Node.create_svg(
    "filter",
    Attr.[id("restructuring-genie-filter")],
    Node.[
      create_svg(
        "feGaussianBlur",
        Attr.[
          create("in", "SourceGraphic"),
          create("stdDeviation", "0.05"),
        ],
        [],
      ),
    ],
  ),
  SvgUtil.Path.(
    view(
      ~attrs=[Attr.classes(["restructuring-genie-path"])],
      [
        // slightly boost genie tip for better visibility
        // when restructuring selection gets large
        M({x: 0.5, y: (-0.3) +. (-0.05)}),
        // TODO unify with caret height numbers (-1.8)
        V({y: (-2.4)}),
        H_({dx: Float.of_int(length)}),
        V_({dy: 1.4}),
        Z,
      ],
    )
  ),
];
