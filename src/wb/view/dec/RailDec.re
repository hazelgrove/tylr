open Virtual_dom.Vdom;

let gradient_id = "rail-gradient";
let gradient = (~len, ~color: Color.t): Node.t => {
  let len = Float.of_int(len);
  let stop = (offset, opacity) =>
    Node.create_svg(
      "stop",
      [
        Attr.create("offset", Printf.sprintf("%f%%", 100. *. offset)),
        Attr.classes([Color.to_string(color)]),
        AttrUtil.stop_opacity(opacity),
      ],
      [],
    );
  Node.create_svg(
    "linearGradient",
    Attr.[
      id(gradient_id),
      create("gradientUnits", "userSpaceOnUse"),
      create("x1", "-0.5"),
      create("y1", "-0.3"),
      create("x2", Printf.sprintf("%f", len +. 0.5)),
      create("y2", "-0.3"),
    ],
    [
      stop(0., 1.),
      stop(2. /. (len +. 1.), 0.),
      stop((len +. 1. -. 2.) /. (len +. 1.), 0.),
      stop(1., 1.),
    ],
  );
};

let view = (~len, {color, atomic: _}: RailStyle.t) => {
  let gradient = gradient(~len, ~color);
  // ideally I would just vary the gradient based on atomicity
  // but can't get the gradient to behave...
  let stroke_attr =
    Attr.create(
      "stroke",
      // atomic
      //   ? Printf.sprintf(
      //       "var(--%s-rail-color",
      //       String.lowercase_ascii(Color.to_string(color)),
      //     )
      //   : Printf.sprintf("url(#%s)", gradient_id),
      Printf.sprintf(
        "var(--%s-rail-color",
        String.lowercase_ascii(Color.to_string(color)),
      ),
    );
  [
    gradient,
    // let dash_attr = atomic ? [] : [Attr.create("stroke-dasharray", "4 2")];
    Node.create_svg(
      "line",
      Attr.[
        create("x1", "-0.5"),
        create("y1", "-0.3"),
        create("x2", Printf.sprintf("%f", Float.of_int(len) +. 0.5)),
        create("y2", "-0.3"),
        // classes([Color.to_string(color)]),
        stroke_attr,
      ],
      // ...dash_attr,
      [],
    ),
  ];
};
