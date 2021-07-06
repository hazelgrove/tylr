open Virtual_dom.Vdom;

let undo = (width, height) =>
  Node.create_svg(
    "svg",
    Attr.[
      create("viewBox", "0 0 512 512"),
      create("width", Printf.sprintf("%fpx", width)),
      create("height", Printf.sprintf("%fpx", height)),
      create("preserveAspectRatio", "none"),
    ],
    [
      Node.create_svg(
        "path",
        [
          Attr.create(
            "d",
            "M129.7,46.4l37.2,37.7l-66.6,67.1h254.4c86.8,0,157.2,70.4,157.2,157.2s-70.4,157.2-157.2,157.2h-52.4v-52.4h52.4 c57.9,0,104.8-46.9,104.8-104.8s-46.9-104.8-104.8-104.8H100.4l66.6,65.8l-37.2,36.9L0,177.4L129.7,46.4z",
          ),
        ],
        [],
      ),
    ],
  );

let redo = (width, height) =>
  Node.create_svg(
    "svg",
    Attr.[
      create("viewBox", "0 0 512 512"),
      create("width", Printf.sprintf("%fpx", width)),
      create("height", Printf.sprintf("%fpx", height)),
      create("preserveAspectRatio", "none"),
    ],
    [
      Node.create_svg(
        "path",
        [
          Attr.create(
            "d",
            "M382.3,46.4l-37.2,37.7l66.6,67.1H157.2C70.4,151.2,0,221.6,0,308.4s70.4,157.2,157.2,157.2h52.4v-52.4h-52.4 c-57.9,0-104.8-46.9-104.8-104.8s46.9-104.8,104.8-104.8h254.4l-66.6,65.8l36.9,36.9l130-128.9L382.3,46.4z",
          ),
        ],
        [],
      ),
    ],
  );
