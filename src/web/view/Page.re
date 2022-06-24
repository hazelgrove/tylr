open Virtual_dom.Vdom;
open Node;

// let logo = (~font_metrics) => {
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
//     ~font_metrics,
//     DecPaths.mk(~logo_pieces=[0, 1, 2, 3], ()),
//     l,
//   );
// };

let undo = (~inject, ~disabled) => {
  let clss = disabled ? ["disabled"] : [];
  let mousedown = _ => disabled ? Event.Many([]) : inject(Update.Undo);
  span(
    Attr.[
      id("undo"),
      classes(["history-button", ...clss]),
      on_mousedown(mousedown),
    ],
    [Icons.undo],
  );
};

let redo = (~inject, ~disabled) => {
  let clss = disabled ? ["disabled"] : [];
  let mousedown = _ => disabled ? Event.Many([]) : inject(Update.Redo);
  span(
    Attr.[
      id("redo"),
      classes(["history-button", ...clss]),
      on_mousedown(mousedown),
    ],
    [Icons.redo],
  );
};
let write_to_clipboard = (_string: string) => {
  open Js_of_ocaml;
  //note: to use execommand would need to introduce a textarea
  /*let _ =
    Dom_html.document##execCommand(
      Js.string("copy"),
      Js.bool(true),
      Js.Opt.return(Js.string("testtest")),
    );*/
  // note: using unsafe as js_of_ocaml doesn't have clipboard bindings
  print_endline(LocalStorage.get_action_log());
  // note: uses backticks to allow for newlines in the string
  let q =
    Printf.sprintf(
      "window.navigator.clipboard.writeText(`%s`);",
      LocalStorage.get_action_log(),
    );
  let _ = Js.Unsafe.js_expr(q);
  ();
};
let left_panel_view = (~inject, history) =>
  div(
    [Attr.id("history-button-container")],
    [
      undo(~inject, ~disabled=!ActionHistory.can_undo(history)),
      redo(~inject, ~disabled=!ActionHistory.can_redo(history)),
      div(
        [
          Attr.class_("topbar-icon"),
          Attr.on_mousedown(_ => inject(Update.Set(WhitespaceIcons))),
        ],
        [Icons.eye],
      ),
      div(
        [
          Attr.class_("topbar-icon"),
          Attr.on_mousedown(_ => {
            write_to_clipboard("Sd");
            inject(Update.Set(WhitespaceIcons));
          }),
        ],
        [Icons.export],
      ),
    ],
  );

let center_panel_view = (~inject, cur_idx) => {
  let next_ed = (cur_idx + 1) mod LocalStorage.num_editors;
  let prev_ed = Util.IntUtil.modulo(cur_idx - 1, LocalStorage.num_editors);
  let incr_ed = _ => inject(Update.SwitchEditor(next_ed));
  let decr_ed = _ => inject(Update.SwitchEditor(prev_ed));
  let toggle_captions = _ => inject(Update.Set(Captions));
  let s = Printf.sprintf("%d / %d", cur_idx + 1, LocalStorage.num_editors);
  div(
    [Attr.id("editor-id")],
    [
      div(
        [Attr.class_("topbar-icon"), Attr.on_mousedown(decr_ed)],
        [Icons.back],
      ),
      div([Attr.on_mousedown(toggle_captions)], [text(s)]),
      div(
        [Attr.class_("topbar-icon"), Attr.on_mousedown(incr_ed)],
        [Icons.forward],
      ),
    ],
  );
};

let link_icon = (str, url, icon) =>
  div(
    [Attr.id(str)],
    [a(Attr.[href(url), create("target", "_blank")], [icon])],
  );

let right_panel_view =
  div(
    [Attr.id("about-button-container")],
    [
      link_icon("github", "https://github.com/hazelgrove/tylr", Icons.github),
      link_icon(
        "help",
        "https://twitter.com/dm_0ney/status/1414742742530498566?s=20",
        Icons.circle_question,
      ),
    ],
  );

let top_bar_view = (~inject, model: Model.t) =>
  div(
    [Attr.id("top-bar")],
    [
      left_panel_view(~inject, model.history),
      center_panel_view(~inject, Model.current_editor(model)),
      //logo(~font_metrics=logo_font_metrics),
      right_panel_view,
    ],
  );

let editor_view =
    ({font_metrics, show_backpack_targets, settings, _} as model: Model.t) =>
  div(
    [Attr.id("code-container")],
    [
      Code.view(
        ~font_metrics,
        ~show_backpack_targets,
        ~zipper=Model.get_zipper(model),
        ~settings,
      ),
    ],
  );

let editor_caption_view = (model: Model.t) =>
  div(
    [Attr.class_("editor-caption")],
    model.settings.captions
      ? [
        text(
          List.nth(
            LocalStorage.editor_captions,
            Model.current_editor(model),
          ),
        ),
      ]
      : [],
  );

let view = (~inject, ~handlers, model: Model.t) => {
  div(
    Attr.[
      id("page"),
      // necessary to make cell focusable
      create("tabindex", "0"),
      on_blur(_ => {
        JsUtil.get_elem_by_id("page")##focus;
        Event.Many([]);
      }),
      ...handlers(~inject, ~model),
    ],
    [
      FontSpecimen.view("font-specimen"),
      FontSpecimen.view("logo-font-specimen"),
      DecUtil.filters,
      top_bar_view(~inject, model),
      editor_caption_view(model),
      editor_view(model),
    ],
  );
};
