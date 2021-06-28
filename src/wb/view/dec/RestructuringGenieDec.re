open Virtual_dom.Vdom;

/**
 * A separate decoration from CaretDec rather than
 * an element of it because of conflicting z-index
 * constraints relative to other decorations
 */
module Profile = {
  type t = {
    origin: int,
    length: int,
  };
};

let view = (~font_metrics, {origin, length}: Profile.t) =>
  DecUtil.container(
    ~font_metrics,
    ~origin,
    ~length,
    ~cls="restructuring-genie",
    [
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
            H_({dx: Float.of_int(length - 1)}),
            V_({dy: 1.4}),
            Z,
          ],
        )
      ),
    ],
  );
