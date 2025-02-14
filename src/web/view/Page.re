open Virtual_dom.Vdom;
open Node;

// let logo = (~font) => {
//   let piece = (step, color: Sort.t, shape: PieceDec.piece_shape, s): Measured.t =>
//     Measured.annot(Piece({color, shape, step}), Text(s));
//   let l =
//     Measured.(
//       spaces(
//         Selected,
//         [
//           piece(0, Exp, ((Convex, 0), (Convex, 0)), "t"),
//           piece(1, Pat, ((Concave, 0), (Convex, 0)), "y"),
//           piece(2, Typ, ((Concave, 0), (Concave, 0)), "l"),
//           piece(3, Selected, ((Convex, 0), (Concave, 1)), "r"),
//         ],
//       )
//     );
//   Code.view_of_layout(
//     ~id="logo",
//     ~text_id="logo-text",
//     ~font,
//     DecPaths.mk(~logo_pieces=[0, 1, 2, 3], ()),
//     l,
//   );
// };

// let undo = (~inject, ~disabled) => {
//   let clss = disabled ? ["disabled"] : [];
//   let mousedown = _ => disabled ? Event.Many([]) : inject(Update.Undo);
//   span(
//     ~attrs=
//       Attr.[
//         id("undo"),
//         classes(["history-button", ...clss]),
//         on_mousedown(mousedown),
//       ],
//     [Icons.undo],
//   );
// };

// let redo = (~inject, ~disabled) => {
//   let clss = disabled ? ["disabled"] : [];
//   let mousedown = _ => disabled ? Event.Many([]) : inject(Update.Redo);
//   span(
//     ~attrs=
//       Attr.[
//         id("redo"),
//         classes(["history-button", ...clss]),
//         on_mousedown(mousedown),
//       ],
//     [Icons.redo],
//   );
// };

// let copy_log_to_clipboard = _ => {
//   Log.append_json_updates_log();
//   JsUtil.copy_to_clipboard(Log.get_json_update_log_string());
//   Event.Ignore;
// };

// let left_panel_view = (~inject, history) =>
//   div(
//     [Attr.id("history-button-container")],
//     [
//       undo(~inject, ~disabled=!History.can_undo(history)),
//       redo(~inject, ~disabled=!History.can_redo(history)),
//       div(
//         [
//           Attr.class_("topbar-icon"),
//           Attr.on_mousedown(copy_log_to_clipboard),
//         ],
//         [Icons.export],
//       ),
//     ],
//   );

// let center_panel_view = (~inject, cur_idx) => {
//   let next_ed = (cur_idx + 1) mod LocalStorage.num_editors;
//   let prev_ed = Stds.IntUtil.modulo(cur_idx - 1, LocalStorage.num_editors);
//   let incr_ed = _ => {
//     Log.append_json_updates_log();
//     inject(Update.SwitchEditor(next_ed));
//   };
//   let decr_ed = _ => {
//     Log.append_json_updates_log();
//     inject(Update.SwitchEditor(prev_ed));
//   };
//   let toggle_captions = _ => inject(Update.Set(Captions));
//   let s = Printf.sprintf("%d / %d", cur_idx + 1, LocalStorage.num_editors);
//   div(
//     [Attr.id("editor-id")],
//     [
//       div(
//         [Attr.class_("topbar-icon"), Attr.on_mousedown(decr_ed)],
//         [Icons.back],
//       ),
//       div([Attr.on_mousedown(toggle_captions)], [text(s)]),
//       div(
//         [Attr.class_("topbar-icon"), Attr.on_mousedown(incr_ed)],
//         [Icons.forward],
//       ),
//     ],
//   );
// };

// let link_icon = (str, url, icon) =>
//   div(
//     ~attrs=[Attr.id(str)],
//     [a(Attr.[href(url), create("target", "_blank")], [icon])],
//   );

// let right_panel_view = (~inject) =>
//   div(
//     [Attr.id("about-button-container")],
//     [
//       div(
//         [
//           Attr.class_("topbar-icon"),
//           Attr.on_mousedown(_ => inject(Update.Set(WhitespaceIcons))),
//         ],
//         [Icons.eye],
//       ),
//       div(
//         [
//           Attr.class_("topbar-icon"),
//           Attr.on_mousedown(_ => inject(Update.LoadDefault)),
//         ],
//         [Icons.trash],
//       ),
//       link_icon("github", "https://github.com/hazelgrove/tylr", Icons.github),
//       link_icon(
//         "help",
//         "https://twitter.com/dm_0ney/status/1414742742530498566?s=20",
//         Icons.circle_question,
//       ),
//     ],
//   );

// let top_bar_view = (~inject, model: Model.t) =>
//   div(
//     [Attr.id("top-bar")],
//     [
//       left_panel_view(~inject, model.history),
//       center_panel_view(~inject, Model.current_editor(model)),
//       right_panel_view(~inject),
//     ],
//   );

let editor_view = (model: Model.t) =>
  div(
    ~attrs=[Attr.id("code-container")],
    [Code.view(~font=model.font, ~zipper=model.zipper)],
  );

// let editor_caption_view = (model: Model.t) =>
//   div(
//     [Attr.class_("editor-caption")],
//     model.settings.captions
//       ? [
//         text(
//           List.nth(
//             LocalStorage.editor_captions,
//             Model.current_editor(model),
//           ),
//         ),
//       ]
//       : [],
//   );

let on_key = (~inject, ~model) => {
  let prevent_default_tab = evt =>
    Util.Key.key_tag(evt) == "Tab" ? [Effect.Prevent_default] : [];
  [
    Attr.on_keypress(_ => Effect.Prevent_default),
    Attr.on_keyup(evt =>
      Effect.Many(
        prevent_default_tab(evt)
        @ List.map(
            inject,
            Update.handle_key_event(Util.Key.mk(KeyUp, evt), ~model),
          ),
      )
    ),
    Attr.on_keydown(evt =>
      Effect.Many(
        prevent_default_tab(evt)
        @ List.map(
            inject,
            Update.handle_key_event(Util.Key.mk(KeyDown, evt), ~model),
          ),
      )
    ),
  ];
};

let copy = (cur: Tylr_core.Zipper.Cursor.Base.t('tok)) =>
  switch (Tylr_core.Zipper.selection_str(cur)) {
  | None => ()
  | Some(str) => Util.Dom.copy(str)
  };

let get_goal = (~font: Model.Font.t, ~target_id, e): Tylr_core.Loc.t => {
  let rect = Util.Dom.get_elem_by_id(target_id)##getBoundingClientRect;
  let goal_x = float_of_int(e##.clientX);
  let goal_y = float_of_int(e##.clientY);
  // print_endline(
  //   "goal: row/col "
  //   ++ string_of_float(goal_x)
  //   ++ ", "
  //   ++ string_of_float(goal_y),
  // );
  // print_endline(
  //   "code-container: row/col "
  //   ++ string_of_float(rect##.top)
  //   ++ ", "
  //   ++ string_of_float(rect##.left),
  // );
  {
    row: Float.to_int((goal_y -. rect##.top) /. font.row_height),
    col: Float.(to_int(round((goal_x -. rect##.left) /. font.col_width))),
  };
};

let view = (~inject, model: Model.t) => {
  div(
    ~attrs=
      Attr.[
        id("page"),
        // necessary to make cell focusable
        // tabindex(0),
        on_blur(_ => {
          //Util.Dom.get_elem_by_id("page")##focus;
          Util.Dom.focus_clipboard_shim();
          Effect.Prevent_default;
        }),
        Attr.on_focus(_ => {
          Util.Dom.focus_clipboard_shim();
          Effect.Ignore;
        }),
        Attr.on_copy(_ => {
          copy(model.zipper.cur);
          Effect.Ignore;
        }),
        Attr.on_cut(_ => {
          copy(model.zipper.cur);
          inject(Update.PerformAction(Delete(L)));
        }),
        Attr.on_paste(evt => {
          Js_of_ocaml.Dom.preventDefault(evt);
          inject(Update.PerformAction(Insert(Util.Dom.paste(evt))));
        }),
        Attr.on_mousedown(evt => {
          Util.Dom.focus_clipboard_shim();
          let loc =
            get_goal(~font=model.font, ~target_id="code-container", evt);
          // print_endline(
          //   "jumping to: "
          //   ++ string_of_int(loc.row)
          //   ++ ", "
          //   ++ string_of_int(loc.col),
          // );
          inject(PerformAction(Move(Jump(loc))));
        }),
        ...on_key(~inject, ~model),
      ],
    [
      FontSpecimen.view("font-specimen"),
      Util.Dom.clipboard_shim,
      // FontSpecimen.view("logo-font-specimen"),
      Dec.Filters.all,
      // top_bar_view(~inject, model),
      // editor_caption_view(model),
      editor_view(model),
      History.view(model),
    ],
  );
};
