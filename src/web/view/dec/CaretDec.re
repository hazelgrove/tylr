open Virtual_dom.Vdom;

module Profile = {
  type t = {
    mode: CaretMode.t,
    origin: int,
    color: Color.t,
    just_failed: option(Core.Action.t),
  };
};

let action_type = txt =>
  Node.div([Attr.classes(["action-type"])], [Node.text(txt)]);

let construct_shape = txt =>
  Node.div([Attr.classes(["construct-shape"])], [Node.text(txt)]);

let key' = Node.div([Attr.classes(["key"])]);
let key = txt => key'([Node.text(txt)]);

let keys_container = keys =>
  Node.div(
    [Attr.classes(["keys-container"])],
    [Node.div([Attr.classes(["keys"])], keys)],
  );
let keys = ks => keys_container(List.map(key, ks));

let keyboard_arrow = s =>
  Node.span(
    [Attr.classes(["keyboard-arrow"])],
    [Node.span([], [Node.text(s)])],
  );

let mark_row = [
  keys_container(
    List.map(
      key',
      [
        [keyboard_arrow(Unicode.up_arrow)],
        [keyboard_arrow(Unicode.down_arrow)],
      ],
    ),
  ),
  action_type("Pick Up | Put Down Selection"),
];

let move_row = [
  keys_container(
    List.map(
      key',
      [
        [keyboard_arrow(Unicode.left_arrow)],
        [keyboard_arrow(Unicode.right_arrow)],
      ],
    ),
  ),
  action_type("Move"),
];

let select_row = [
  keys_container([
    key("Shift"),
    Node.div([], [Node.text("+")]),
    ...List.map(
         key',
         [
           [keyboard_arrow(Unicode.left_arrow)],
           [keyboard_arrow(Unicode.right_arrow)],
         ],
       ),
  ]),
  action_type("Select"),
];

let delete_row = [
  keys(["Backspace", "Delete"]),
  action_type("Delete or Restructure"),
];

let undo_row = [
  keys_container([
    key("Ctrl"),
    Node.div([], [Node.text("+")]),
    key("z"),
  ]),
  action_type("Undo"),
];
let redo_row = [
  keys_container([
    key("Ctrl"),
    Node.div([], [Node.text("+")]),
    key("Shift"),
    Node.div([], [Node.text("+")]),
    key("z"),
  ]),
  action_type("Redo"),
];

let buffer_cell = Node.div([], []);
let buffer_row = [buffer_cell, buffer_cell];

let construct_rows =
  List.concat(
    Util.ListUtil.join(
      buffer_row,
      [
        [buffer_cell, action_type("Construct")],
        [
          keys_container([
            key("0"),
            Node.div([], [Node.text("-")]),
            key("9"),
          ]),
          construct_shape("single-digit num"),
        ],
        [
          keys_container([
            key("a"),
            Node.div([], [Node.text("-")]),
            key("z"),
          ]),
          construct_shape("single-char var"),
        ],
        [keys(["+", "*", ","]), construct_shape("plus | times | pair")],
        [keys(["Space"]), construct_shape("application")],
        // [keys(["*"]), construct_shape("times")],
        // [keys([","]), construct_shape("prod")],
        [keys(["(", ")"]), construct_shape("parentheses")],
        [keys(["\\", "="]), construct_shape("lambda | let")],
      ],
    ),
  );

let typ =
  Node.span([Attr.classes(["sort-label", "typ"])], [Node.text("type")]);
let pat =
  Node.span(
    [Attr.classes(["sort-label", "pat"])],
    [Node.text("pattern")],
  );
let exp =
  Node.span(
    [Attr.classes(["sort-label", "exp"])],
    [Node.text("expression")],
  );
let inconsistent =
  Node.span(
    [Attr.classes(["inconsistent"])],
    [Node.text("inconsistent")],
  );

let view =
    (
      ~font_metrics: FontMetrics.t,
      {color, origin, mode, just_failed}: Profile.t,
    ) => {
  let selection =
    switch (mode) {
    | Pointing
    | Selecting => []
    | Restructuring({view, selection: _}) =>
      Node.[
        span(
          [
            Attr.create(
              "style",
              Printf.sprintf(
                "margin-right: %fpx;",
                (-0.5) *. font_metrics.col_width,
              ),
            ),
          ],
          [view],
        ),
      ]
    };
  let just_failed_clss =
    switch (just_failed) {
    | None => []
    | Some(_) => [JustFailedCls.Bar.mk()]
    };
  let top = (-0.3) *. font_metrics.row_height;
  Node.div(
    [
      Attr.id("caret"),
      Attr.create(
        "style",
        Printf.sprintf(
          "top: %fpx; left: %fpx",
          top,
          (Float.of_int(origin) +. 0.5) *. font_metrics.col_width,
        ),
      ),
    ],
    [
      Node.div(
        [
          Attr.id("caret-bar"),
          Attr.classes([Color.to_string(color), ...just_failed_clss]),
        ],
        [],
      ),
      Node.div(
        [
          Attr.id("action-table"),
          Attr.create(
            "style",
            Printf.sprintf("top: %fpx;", Float.of_int(80) -. top),
          ),
        ],
        List.concat([
          move_row,
          buffer_row,
          select_row,
          buffer_row,
          mark_row,
          buffer_row,
          delete_row,
          buffer_row,
          construct_rows,
        ]),
      ),
      Node.div(
        [
          Attr.id("backpack-suf"),
          Attr.create(
            "style",
            Printf.sprintf(
              "top: %fpx; left: %fpx;",
              (-1.8) *. font_metrics.row_height,
              (-0.5) *. font_metrics.col_width,
            ),
          ),
        ],
        selection,
      ),
    ],
  );
};
