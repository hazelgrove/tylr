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

let circle_question = (width, height) =>
  Node.create_svg(
    "svg",
    Attr.[
      create("viewBox", "-0.5 -0.5 25 25"),
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
            "M12 2c5.514 0 10 4.486 10 10s-4.486 10-10 10-10-4.486-10-10 4.486-10 10-10zm0-2c-6.627 0-12 5.373-12 12s5.373 12 12 12 12-5.373 12-12-5.373-12-12-12zm1.25 17c0 .69-.559 1.25-1.25 1.25-.689 0-1.25-.56-1.25-1.25s.561-1.25 1.25-1.25c.691 0 1.25.56 1.25 1.25zm1.393-9.998c-.608-.616-1.515-.955-2.551-.955-2.18 0-3.59 1.55-3.59 3.95h2.011c0-1.486.829-2.013 1.538-2.013.634 0 1.307.421 1.364 1.226.062.847-.39 1.277-.962 1.821-1.412 1.343-1.438 1.993-1.432 3.468h2.005c-.013-.664.03-1.203.935-2.178.677-.73 1.519-1.638 1.536-3.022.011-.924-.284-1.719-.854-2.297z",
          ),
        ],
        [],
      ),
    ],
  );
