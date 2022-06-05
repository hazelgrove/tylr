open Virtual_dom.Vdom;

/**
 * A separate decoration from CaretDec rather than
 * an element of it because of conflicting z-index
 * constraints relative to other decorations
 */
module Profile = {
  type t = {
    length: int,
    height: int,
    origin: Core.Measured.point,
  };
};

let view = (~font_metrics, measurement: Profile.t) =>
  DecUtil.container2d(
    ~font_metrics,
    ~measurement={
      origin: {
        row: 0, //measurement.origin.row,
        col: measurement.origin.col,
      },
      last: {
        row: measurement.origin.row + measurement.height,
        col: measurement.origin.col + measurement.length,
      },
    },
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
            M({x: 0., y: (-0.1)}),
            // TODO unify with caret height numbers (-1.8)
            V({y: (-2.0)}),
            H_({dx: Float.of_int(measurement.length)}),
            V_({dy: 1.0}),
            Z,
          ],
        )
      ),
    ],
  );
