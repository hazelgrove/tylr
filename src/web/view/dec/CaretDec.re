open Virtual_dom.Vdom;
open Util;
open Core;

module Profile = {
  type t = {
    mode: CaretMode.t,
    delete_actions: (option([ | `Remove | `Restructure]) as 'a, 'a),
    origin: int,
    color: Color.t,
    just_failed: option(FailedInput.t),
  };
};

let action_type' = (~clss=[]) =>
  Node.div([Attr.classes(["action-type", ...clss])]);
let action_type = (~clss=[], txt) => action_type'(~clss, [Node.text(txt)]);

let construct_shape_row = (~disabled=false, shapes) =>
  Node.div(
    [Attr.classes(["construct-shape-row"])],
    ListUtil.join(
      Node.span(
        [Attr.classes(disabled ? ["disabled"] : [])],
        [Node.text(" | ")],
      ),
      shapes,
    ),
  );

let construct_shape' = (~disabled) =>
  Node.span([
    Attr.classes(["construct-shape", ...disabled ? ["disabled"] : []]),
  ]);
let construct_shape = (~disabled, txt) =>
  construct_shape'(~disabled, [Node.text(txt)]);

let key' = (~disabled=false) =>
  Node.div([Attr.classes(["key", ...disabled ? ["disabled"] : []])]);
let key = (~disabled=false, txt) => key'(~disabled, [Node.text(txt)]);

let keys_container = (~with_hyphen=false, keys) =>
  Node.div(
    [Attr.classes(["keys-container"])],
    [
      Node.div(
        [Attr.classes(["keys", ...with_hyphen ? ["with-hyphen"] : []])],
        keys,
      ),
    ],
  );
let keys = (~disabled=false, ks) =>
  keys_container(List.map(key(~disabled), ks));

let keyboard_arrow = s =>
  Node.span(
    [Attr.classes(["keyboard-arrow"])],
    [Node.span([], [Node.text(s)])],
  );

let mark_row = (~disabled: option([ | `Up | `Down | `Both])) => {
  let d = ["disabled"];
  let (up_clss, bar_clss, down_clss, _sel_clss) =
    switch (disabled) {
    | None => ([], [], [], [])
    | Some(`Up) => (d, d, [], [])
    | Some(`Down) => ([], d, d, [])
    | Some(`Both) => (d, d, d, d)
    };
  [
    keys_container(
      List.map(
        key',
        [
          [keyboard_arrow(Unicode.up_arrow)],
          [keyboard_arrow(Unicode.down_arrow)],
        ],
      ),
    ),
    action_type'(
      Node.[
        span([Attr.classes(up_clss)], [text("Pick Up")]),
        span([Attr.classes(bar_clss)], [text(" | ")]),
        span([Attr.classes(down_clss)], [text(" Put Down")]),
      ],
      /*span([Attr.classes(sel_clss)], [text(" Selection")]),*/
    ),
  ];
};

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

let select_row = (~disabled) => [
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
  action_type(~clss=disabled ? ["disabled"] : [], "Select"),
];

let delete_action_type =
    (_dir: Direction.t, a: option([ | `Remove | `Restructure])) => {
  let d = ["disabled"];
  let (rem_clss, res_clss, _arr_clss) =
    switch (a) {
    | None => (d, d, d)
    | Some(`Remove) => ([], d, [])
    | Some(`Restructure) => (d, [], [])
    };
  action_type'(
    Node.[
      span([Attr.classes(rem_clss)], [text("Remove")]),
      span([Attr.classes(d)], [text(" / ")]),
      span([Attr.classes(res_clss)], [text("Restructure")]),
    ],
    // span(
    //   [Attr.classes(["delete-action-arrow", ...arr_clss])],
    //   [
    //     text(" "),
    //     text(dir == Left ? Unicode.left_arrow : Unicode.right_arrow),
    //   ],
    // ),
  );
};

let buffer_cell = Node.div([], []);
let buffer_row = [buffer_cell, buffer_cell];

let delete_row =
    ((action_l, action_r): (option([ | `Remove | `Restructure]) as 'a, 'a)) => {
  [
    keys(["Backspace"]),
    delete_action_type(Left, action_l),
    buffer_cell,
    buffer_cell,
    keys(["Delete"]),
    delete_action_type(Right, action_r),
  ];
};

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

let construct_rows = (color: Color.t, mode: CaretMode.t) => {
  let is_pointing =
    switch (mode) {
    | Pointing
    | Selecting(_, []) => true
    | Selecting(_)
    | Restructuring(_) => false
    };
  let is_exp =
    switch (color) {
    | Exp => true
    | _ => false
    };
  let is_exp_pointing = is_pointing && is_exp;

  let disabled_if_not_pointing = construct_shape(~disabled=!is_pointing);
  let disabled_if_not_exp_pointing =
    construct_shape(~disabled=!is_exp_pointing);

  List.concat(
    Util.ListUtil.join(
      buffer_row,
      [
        [
          buffer_cell,
          action_type(
            ~clss=
              switch (mode) {
              | Pointing
              | Selecting(_, []) => [Color.to_string(color)]
              | _ => ["disabled"]
              },
            "Construct",
          ),
        ],
        [
          keys_container(
            ~with_hyphen=true,
            [
              key(~disabled=!is_exp_pointing, "0"),
              Node.div([Attr.classes(["key-hyphen"])], [Node.text("-")]),
              key(~disabled=!is_exp_pointing, "9"),
            ],
          ),
          construct_shape_row(
            ~disabled=!is_exp_pointing,
            [disabled_if_not_exp_pointing("single-digit num")],
          ),
        ],
        [
          keys_container(
            ~with_hyphen=true,
            [
              key(~disabled=!is_pointing, "a"),
              Node.div([Attr.classes(["key-hyphen"])], [Node.text("-")]),
              key(~disabled=!is_pointing, "z"),
            ],
          ),
          construct_shape_row(
            ~disabled=!is_pointing,
            [disabled_if_not_pointing("single-char var")],
          ),
        ],
        [
          keys_container([
            key(~disabled=!is_exp_pointing, "+"),
            key(~disabled=!is_exp_pointing, "-"),
            key(~disabled=!is_exp_pointing, "*"),
            key(~disabled=!is_exp_pointing, "/"),
          ]),
          construct_shape_row(
            ~disabled=!is_exp_pointing,
            [
              disabled_if_not_exp_pointing("plus"),
              disabled_if_not_exp_pointing("minus"),
              disabled_if_not_exp_pointing("times"),
              disabled_if_not_exp_pointing("div"),
            ],
          ),
        ],
        [
          keys_container([
            key(~disabled=!is_pointing, ","),
            key(~disabled=!is_exp_pointing, "!"),
            // key(~disabled=!is_exp_pointing, "?"),
            key(~disabled=!is_exp_pointing, "Space"),
          ]),
          construct_shape_row(
            ~disabled=!is_exp_pointing,
            [
              disabled_if_not_pointing("pair"),
              disabled_if_not_exp_pointing("factorial"),
              // disabled_if_not_exp_pointing("cond"),
              disabled_if_not_exp_pointing("application"),
            ],
          ),
        ],
        [
          keys(~disabled=!is_pointing, ["(", ")"]),
          construct_shape_row(
            ~disabled=!is_pointing,
            [disabled_if_not_pointing("parentheses")],
          ),
        ],
        [
          keys_container([
            key(~disabled=!is_exp_pointing, "?"),
            key(~disabled=!is_exp_pointing, ":"),
          ]),
          construct_shape_row([disabled_if_not_exp_pointing("ifzero")]),
        ],
        [
          keys(~disabled=!is_exp_pointing, ["\\", "="]),
          construct_shape_row(
            ~disabled=!is_exp_pointing,
            [
              disabled_if_not_exp_pointing("lambda"),
              disabled_if_not_exp_pointing("let"),
            ],
          ),
        ],
      ],
    ),
  );
};

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
      {color, origin, mode, just_failed, delete_actions}: Profile.t,
    ) => {
  let (ss_before, ss_after) =
    switch (mode) {
    | Pointing
    | Selecting(_) => ([], [])
    | Restructuring({
        backpack: (d, _, _),
        view: (selection, rest),
        at_restructurable_selection: _,
      }) =>
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
    | Some({reason: Unrecognized, prior_attempts}) when prior_attempts >= 0 => [
        text("unrecognized input for current sort"),
      ]
    | Some({reason: Failure(f), prior_attempts}) when prior_attempts >= 0 =>
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
          text("cannot pick up selection with caps of different sort"),
        ]
      | Cant_put_down_selection(sort, frame_sort) => [
          text("cannot put down "),
          // br([]),
          text("a" ++ (sort == Pat ? " " : "n ")),
          sort_label(sort),
          text("-capped selection"),
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
        [Attr.id("action-table"), Attr.classes([Color.to_string(color)])],
        List.concat([
          move_row,
          buffer_row,
          select_row(
            ~disabled=
              switch (mode) {
              | Restructuring(_) => true
              | _ => false
              },
          ),
          buffer_row,
          mark_row(
            ~disabled=
              switch (mode) {
              | Pointing
              | Selecting(_, []) => Some(`Both)
              | Selecting(_, selection) =>
                switch (Selection.is_restructurable(selection)) {
                | None => Some(`Both)
                | Some(_) => Some(`Down)
                }
              | Restructuring({at_restructurable_selection, _}) =>
                at_restructurable_selection ? None : Some(`Up)
              },
          ),
          buffer_row,
          delete_row(delete_actions),
          buffer_row,
          construct_rows(color, mode),
        ]),
      ),
      Node.div(
        [
          Attr.id("backpack-pre"),
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
