open Virtual_dom.Vdom;
open Cor;

module Profile = {
  type t = {
    mode: CaretMode.t,
    origin: int,
    color: Color.t,
    just_failed: option(FailedInput.t),
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

let mark_row = [
  keys(["Enter"]),
  action_type("Pick Up / Put Down Selection"),
];

let move_row = [
  keys([Unicode.left_arrow, Unicode.right_arrow]),
  action_type("Move"),
];

let select_row = [
  keys_container([
    key("Alt"),
    Node.div([], [Node.text("+")]),
    key(Unicode.left_arrow),
    key(Unicode.right_arrow),
  ]),
  action_type("Select"),
];

let delete_row = [
  keys(["Backspace", "Delete"]),
  action_type("Delete / Restructure"),
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
        [keys(["Space"]), construct_shape("ap")],
        [keys(["+"]), construct_shape("plus")],
        [keys(["*"]), construct_shape("times")],
        [keys([","]), construct_shape("prod")],
        [keys(["("]), construct_shape("paren")],
        [keys(["\\"]), construct_shape("lambda")],
        [keys(["="]), construct_shape("let")],
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
  let (ss_before, ss_after) =
    switch (mode) {
    | Pointing
    | Selecting(_) => ([], [])
    | Restructuring({backpack: (d, _, _), view: (selection, rest)}) =>
      let view_of_selection = (~focused, view) =>
        Node.span(
          [
            Attr.create(
              "style",
              Printf.sprintf(
                "margin-right: %fpx;",
                0.5 *. font_metrics.col_width,
              ),
            ),
            Attr.classes([
              "restructuring-selection",
              focused ? "focused" : "unfocused",
            ]),
          ],
          [view],
        );

      let selection = view_of_selection(~focused=true, selection);
      let rest = List.map(view_of_selection(~focused=false), rest);
      switch (d) {
      | Left => (rest, [selection])
      | Right => ([], [selection, ...rest])
      };
    };
  let just_failed_clss =
    switch (just_failed) {
    | None => []
    | Some(_) => [JustFailedCls.Bar.mk()]
    };
  let err_msg = {
    open Node;
    let sort_label = sort =>
      span(
        [Attr.classes(["sort-label", Sort.to_string(sort)])],
        [text(Sort.to_proper_string(sort))],
      );
    switch (just_failed) {
    | Some({reason: Unrecognized, prior_attempts}) when prior_attempts >= 2 => [
        text("unrecognized keyboard input"),
      ]
    | Some({reason: Failure(f), prior_attempts}) when prior_attempts >= 2 =>
      switch (f) {
      | Failure.Undefined
      | Cant_move => []
      | Cant_construct(sort, frame_sort) => [
          text("cannot construct "),
          text("a" ++ (sort == Pat ? " " : "n ")),
          sort_label(sort),
          text(" tile in "),
          sort_label(frame_sort),
          text(" position"),
        ]
      | Cant_pick_up_selection => [
          text("cannot pick up selection with different sort bounds"),
        ]
      | Cant_put_down_selection(sort, frame_sort) => [
          text("cannot put down "),
          // br([]),
          text("a" ++ (sort == Pat ? " " : "n ")),
          sort_label(sort),
          text("-bounded selection"),
          // br([]),
          text(" in "),
          sort_label(frame_sort),
          text(" position"),
        ]
      | Cant_construct_in_restructuring => [
          text("cannot construct in restructuring mode"),
        ]
      }
    | _ => []
    };
  };
  let err_msg_style = {
    let y_pos =
      Printf.sprintf(
        "bottom: calc(100%% + %fpx);",
        0.3 *. font_metrics.row_height,
      );
    switch (mode) {
    | Pointing
    | Selecting(_) => "left: 12px;" ++ y_pos
    | Restructuring({backpack: (d, selection, _), _}) =>
      let x_pos =
        switch (d) {
        | Left =>
          let len =
            Layout.length(
              Layout.mk_selection(~frame_color=Selected, selection),
            );
          Printf.sprintf(
            "left: calc(%fpx + 12px); ",
            Float.of_int(len - 1) *. font_metrics.col_width,
          );
        | Right => "right: calc(100% + 12px); "
        };
      x_pos ++ y_pos;
    };
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
        Attr.[
          id("error-message"),
          classes(just_failed_clss),
          create("style", err_msg_style),
        ],
        [Node.span([], err_msg)],
      ),
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
          construct_rows,
          buffer_row,
          delete_row,
          buffer_row,
          select_row,
          buffer_row,
          move_row,
          buffer_row,
          mark_row,
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
              "bottom: calc(100%% + %fpx); right: 100%%;",
              0.8 *. font_metrics.row_height,
            ),
          ),
        ],
        List.rev(ss_before),
      ),
      Node.div(
        [
          Attr.id("backpack-suf"),
          Attr.create(
            "style",
            Printf.sprintf(
              "bottom: calc(100%% + %fpx); left: %fpx;",
              0.8 *. font_metrics.row_height,
              (-0.5) *. font_metrics.col_width,
            ),
          ),
        ],
        ss_after,
      ),
    ],
  );
};
