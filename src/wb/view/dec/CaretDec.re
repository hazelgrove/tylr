open Virtual_dom.Vdom;

module Profile = {
  type t = {
    view_of_layout: (DecPaths.t, Layout.t) => Node.t,
    mode: CaretMode.t,
    offset: int,
    color: Color.t,
  };
};

let action_type = txt =>
  Node.div([Attr.classes(["action-type"])], [Node.text(txt)]);

let construct_shape = txt =>
  Node.div([Attr.classes(["construct-shape"])], [Node.text(txt)]);

let key = txt => Node.div([Attr.classes(["key"])], [Node.text(txt)]);

let keys_container = keys =>
  Node.div(
    [Attr.classes(["keys-container"])],
    [Node.div([Attr.classes(["keys"])], keys)],
  );
let keys = ks => keys_container(List.map(key, ks));

let move_row = [
  keys([Unicode.left_arrow, Unicode.right_arrow]),
  action_type("Move"),
];

let select_row = [
  keys_container([
    key("Shift"),
    Node.div([], [Node.text("+")]),
    key(Unicode.left_arrow),
    key(Unicode.right_arrow),
  ]),
  action_type("Select"),
];

let delete_row = [
  keys(["Backspace", "Delete"]),
  action_type("Delete/Restructure"),
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
        [keys(["+"]), construct_shape("plus")],
        [keys(["("]), construct_shape("parentheses")],
        [keys(["\\"]), construct_shape("lambda")],
        [keys(["="]), construct_shape("let expression")],
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
      {view_of_layout, color, offset, mode}: Profile.t,
    ) => {
  let selection =
    switch (mode) {
    | Pointing
    | Selecting => []
    | Restructuring(selection) =>
      let l = Layout.mk_selection(~style=Filtered, Selected, selection);
      // let len = Layout.length(l);
      let dpaths =
        DecPaths.{
          caret: None,
          anchors: [([], 0), ([], List.length(selection))],
        };
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
          [view_of_layout(dpaths, l)],
        ),
      ];
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
          (Float.of_int(offset) +. 0.5) *. font_metrics.col_width,
        ),
      ),
    ],
    [
      Node.div(
        [Attr.id("caret-bar"), Attr.classes([Color.to_string(color)])],
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
          delete_row,
          buffer_row,
          construct_rows,
          buffer_row,
          undo_row,
          buffer_row,
          redo_row,
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
