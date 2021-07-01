open Virtual_dom.Vdom;
open Cor;

let gradient_id = "target-bounds-gradient";
let gradient =
    (len, (l_strict, r_strict), frame_sort, mode: CaretMode.t): Node.t => {
  let c =
    switch (mode) {
    | Pointing => Color.of_sort(frame_sort)
    | Selecting
    | Restructuring(_) => Selected
    };
  let stop = (offset, opacity) =>
    Node.create_svg(
      "stop",
      [
        Attr.create("offset", Printf.sprintf("%f%%", 100. *. offset)),
        Attr.classes([Color.to_string(c)]),
        AttrUtil.stop_opacity(opacity),
      ],
      [],
    );
  Node.create_svg(
    "linearGradient",
    Attr.[
      id(gradient_id),
      create("gradientUnits", "userSpaceOnUse"),
      create("x1", "0"),
      create("x2", "1"),
    ],
    [
      stop(0., 0.),
      stop(1.5 /. len, l_strict ? 0. : 1.),
      stop(1.5 /. len, 0.),
      stop((len -. 1.5) /. len, 0.),
      stop((len -. 1.5) /. len, r_strict ? 0. : 1.),
      stop(1., 0.),
    ],
  );
};

let view =
    (
      ~font_metrics: FontMetrics.t,
      ~origin: int,
      ~len: int,
      strict_bounds: (bool, bool),
      frame_sort: Sort.t,
      mode: CaretMode.t,
    ) => {
  let len = Float.of_int(len + 2);
  let gradient = gradient(len, strict_bounds, frame_sort, mode);
  Node.div(
    Attr.[
      id("target-bounds"),
      create(
        "style",
        Printf.sprintf(
          "top: calc(%fpx + 1px); height: 2px; left: %fpx; width: %fpx;",
          (-0.3) *. font_metrics.row_height,
          Float.of_int(origin - 1) *. font_metrics.col_width,
          len *. font_metrics.col_width,
        ),
      ),
    ],
    [
      Node.create_svg(
        "svg",
        Attr.[
          create("viewBox", "0 0 1 1"),
          create("preserveAspectRatio", "none"),
          create(
            "style",
            "position: absolute; left: 0; top: 0; width: 100%; height: 100%;",
          ),
        ],
        [
          gradient,
          Node.create_svg(
            "rect",
            Attr.[
              create("width", "1"),
              create("height", "1"),
              create("fill", Printf.sprintf("url(#%s)", gradient_id)),
            ],
            [],
          ),
        ],
      ),
    ],
  );
};
