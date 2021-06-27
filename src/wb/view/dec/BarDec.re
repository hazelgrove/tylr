open Virtual_dom.Vdom;

// TODO rename module

let view = (~font_metrics: FontMetrics.t) =>
  Node.div(
    Attr.[
      id("bar"),
      create(
        "style",
        Printf.sprintf(
          "top: calc(%fpx + 1.5px); height: 1.5px;",
          (-0.3) *. font_metrics.row_height,
        ),
      ),
    ],
    [
      Node.create_svg(
        "svg",
        Attr.[
          create("viewBox", "0 0 1 1"),
          create("preserveAspectRatio", "none"),
        ],
        [
          Node.create_svg(
            "rect",
            Attr.[create("width", "1"), create("height", "1")],
            [],
          ),
        ],
      ),
    ],
  );
