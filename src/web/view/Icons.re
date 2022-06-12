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

let github = (width, height) =>
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
            "M12 0c-6.626 0-12 5.373-12 12 0 5.302 3.438 9.8 8.207 11.387.599.111.793-.261.793-.577v-2.234c-3.338.726-4.033-1.416-4.033-1.416-.546-1.387-1.333-1.756-1.333-1.756-1.089-.745.083-.729.083-.729 1.205.084 1.839 1.237 1.839 1.237 1.07 1.834 2.807 1.304 3.492.997.107-.775.418-1.305.762-1.604-2.665-.305-5.467-1.334-5.467-5.931 0-1.311.469-2.381 1.236-3.221-.124-.303-.535-1.524.117-3.176 0 0 1.008-.322 3.301 1.23.957-.266 1.983-.399 3.003-.404 1.02.005 2.047.138 3.006.404 2.291-1.552 3.297-1.23 3.297-1.23.653 1.653.242 2.874.118 3.176.77.84 1.235 1.911 1.235 3.221 0 4.609-2.807 5.624-5.479 5.921.43.372.823 1.102.823 2.222v3.293c0 .319.192.694.801.576 4.765-1.589 8.199-6.086 8.199-11.386 0-6.627-5.373-12-12-12z",
          ),
        ],
        [],
      ),
    ],
  );

let back = (width, height) =>
  Node.create_svg(
    "svg",
    Attr.[
      create("viewBox", "0 0 330 330"),
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
            "M250.606,154.389l-150-149.996c-5.857-5.858-15.355-5.858-21.213,0.001  c-5.857,5.858-5.857,15.355,0.001,21.213l139.393,139.39L79.393,304.394c-5.857,5.858-5.857,15.355,0.001,21.213  C82.322,328.536,86.161,330,90,330s7.678-1.464,10.607-4.394l149.999-150.004c2.814-2.813,4.394-6.628,4.394-10.606  C255,161.018,253.42,157.202,250.606,154.389z",
          ),
          Attr.create("transform", "scale(-0.75, 0.75) translate(-330, 50)"),
        ],
        [],
      ),
    ],
  );

let forward = (width, height) =>
  Node.create_svg(
    "svg",
    Attr.[
      create("viewBox", "0 0 330 330"),
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
            "M250.606,154.389l-150-149.996c-5.857-5.858-15.355-5.858-21.213,0.001  c-5.857,5.858-5.857,15.355,0.001,21.213l139.393,139.39L79.393,304.394c-5.857,5.858-5.857,15.355,0.001,21.213  C82.322,328.536,86.161,330,90,330s7.678-1.464,10.607-4.394l149.999-150.004c2.814-2.813,4.394-6.628,4.394-10.606  C255,161.018,253.42,157.202,250.606,154.389z",
          ),
          Attr.create("transform", "scale(0.75, 0.75) translate(0, 50)"),
        ],
        [],
      ),
    ],
  );
