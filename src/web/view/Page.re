open Virtual_dom.Vdom;
open Node;

// let logo = (~font_metrics) => {
//   let piece = (step, color: Color.t, shape: SelemDec.piece_shape, s): Measured.t =>
//     Measured.annot(Piece({color, shape, step}), Text(s));
//   let l =
//     Measured.(
//       spaces(
//         // HACK
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

let filters =
  NodeUtil.svg(
    Attr.[id("filters")],
    [
      SelemDec.raised_shadow_filter(~color=Exp),
      SelemDec.shadow_filter(~color=Exp),
      SelemDec.raised_shadow_filter(~color=Pat),
      SelemDec.shadow_filter(~color=Pat),
      SelemDec.raised_shadow_filter(~color=Typ),
      SelemDec.shadow_filter(~color=Typ),
      SelemDec.raised_shadow_filter(~color=Selected),
      SelemDec.shadow_filter(~color=Selected),
      CaretPosDec.blur_filter,
    ],
  );

let icon_size = 20.;

let undo = (~inject, ~disabled) => {
  let clss = disabled ? ["disabled"] : [];
  let mousedown = _ => disabled ? Event.Many([]) : inject(Update.Undo);
  span(
    Attr.[
      id("undo"),
      title("undo"),
      classes(["history-button", ...clss]),
      on_mousedown(mousedown),
    ],
    [Icons.undo(icon_size, icon_size)],
  );
};

let redo = (~inject, ~disabled) => {
  let clss = disabled ? ["disabled"] : [];
  let mousedown = _ => disabled ? Event.Many([]) : inject(Update.Redo);
  span(
    Attr.[
      id("redo"),
      title("redo"),
      create(
        "style",
        Printf.sprintf("width: %fpx; height: %fpx;", icon_size, icon_size),
      ),
      classes(["history-button", ...clss]),
      on_mousedown(mousedown),
    ],
    [Icons.redo(icon_size, icon_size)],
  );
};

let history_panel_view = (~inject, history) =>
  div(
    [Attr.id("history-button-container")],
    [
      undo(~inject, ~disabled=!ActionHistory.can_undo(history)),
      redo(~inject, ~disabled=!ActionHistory.can_redo(history)),
    ],
  );

let editor_panel_view = (~inject, cur_idx) => {
  let rotate_ed = _ =>
    inject(Update.SwitchEditor((cur_idx + 1) mod LocalStorage.num_editors));
  let s =
    Printf.sprintf("editor %d of %d", cur_idx, LocalStorage.num_editors);
  div([Attr.id("editor-id"), Attr.on_mousedown(rotate_ed)], [text(s)]);
};

let link_icon = (str, url, icon) =>
  div(
    Attr.[id(str), title(str)],
    [
      a(
        Attr.[href(url), create("target", "_blank")],
        [icon(icon_size, icon_size)],
      ),
    ],
  );
let about_panel_view =
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
      history_panel_view(~inject, model.history),
      editor_panel_view(~inject, Update.current_editor(model)),
      //logo(~font_metrics=logo_font_metrics),
      about_panel_view,
    ],
  );

let editor_view = ({font_metrics, history, _} as model: Model.t) =>
  div(
    [Attr.id("code-container")],
    [
      Code.view(
        ~font_metrics,
        ~just_failed=history.just_failed,
        ~zipper=Model.get_zipper(model),
      ),
    ],
  );

let view = (~inject, model: Model.t) => {
  //print_endline("Page.view");
  let zipper = Model.get_zipper(model);
  div(
    Attr.[
      id("page"),
      // necessary to make cell focusable
      create("tabindex", "0"),
      on_blur(_ => {
        JsUtil.get_elem_by_id("page")##focus;
        Event.Many([]);
      }),
      ...Keyboard.handlers(~inject, ~zipper, ~double_tap=model.double_tap),
    ],
    [
      FontSpecimen.view("font-specimen"),
      FontSpecimen.view("logo-font-specimen"),
      filters,
      top_bar_view(~inject, model),
      editor_view(model),
    ],
  );
};
