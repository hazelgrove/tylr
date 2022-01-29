open Virtual_dom.Vdom;

// let logo = (~font_metrics) => {
//   let piece = (step, color: Color.t, shape: Layout.piece_shape, s): Layout.t =>
//     Layout.annot(Piece({color, shape, step}), Text(s));
//   let l =
//     Layout.(
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

let size = 20.;
let undo = (~inject, ~disabled) => {
  let clss = disabled ? ["disabled"] : [];
  let mousedown = _ => disabled ? Event.Many([]) : inject(Update.Undo);
  Node.span(
    Attr.[
      id("undo"),
      title("undo"),
      create(
        "style",
        Printf.sprintf("width: %fpx; height: %fpx;", size, size),
      ),
      classes(["history-button", ...clss]),
      on_mousedown(mousedown),
    ],
    [Icons.undo(size, size)],
  );
};
let redo = (~inject, ~disabled) => {
  let clss = disabled ? ["disabled"] : [];
  let mousedown = _ => disabled ? Event.Many([]) : inject(Update.Redo);
  Node.span(
    Attr.[
      id("redo"),
      title("redo"),
      create(
        "style",
        Printf.sprintf("width: %fpx; height: %fpx;", size, size),
      ),
      classes(["history-button", ...clss]),
      on_mousedown(mousedown),
    ],
    [Icons.redo(size, size)],
  );
};

let help_size = 20.;

let view = (~inject, model: Model.t) => {
  print_endline("Page.view");
  let Model.{
        font_metrics,
        logo_font_metrics: _,
        zipper,
        history,
        show_neighbor_tiles,
      } = model;
  // let dpaths = DecPaths.of_zipper(zipper);
  let l = Layout.mk_zipper(zipper);
  Node.div(
    Attr.[
      id("page"),
      // necessary to make cell focusable
      create("tabindex", "0"),
      on_blur(_ => {
        JsUtil.get_elem_by_id("page")##focus;
        Event.Many([]);
      }),
      ...Keyboard.handlers(~inject, ~zipper),
    ],
    Node.[
      FontSpecimen.view("font-specimen"),
      FontSpecimen.view("logo-font-specimen"),
      filters,
      div(
        [Attr.id("top-bar")],
        [
          div(
            [Attr.id("history-button-container")],
            [
              undo(~inject, ~disabled=!ActionHistory.can_undo(model.history)),
              redo(
                ~inject,
                ~disabled=!ActionHistory.can_redo(model.history),
              ),
            ],
          ),
          // logo(~font_metrics=logo_font_metrics),
          div(
            [Attr.id("about-button-container")],
            [
              div(
                Attr.[
                  id("github"),
                  title("GitHub"),
                  create(
                    "style",
                    Printf.sprintf(
                      "width: %fpx; height: %fpx;",
                      help_size,
                      help_size,
                    ),
                  ),
                ],
                [
                  a(
                    Attr.[
                      href("https://github.com/hazelgrove/tylr"),
                      create("target", "_blank"),
                    ],
                    [Icons.github(help_size, help_size)],
                    // [Node.text("?")],
                  ),
                ],
              ),
              div(
                Attr.[
                  id("help"),
                  title("help"),
                  create(
                    "style",
                    Printf.sprintf(
                      "width: %fpx; height: %fpx;",
                      help_size,
                      help_size,
                    ),
                  ),
                ],
                [
                  a(
                    Attr.[
                      href(
                        "https://twitter.com/dm_0ney/status/1414742742530498566?s=20",
                      ),
                      create("target", "_blank"),
                    ],
                    [Icons.circle_question(help_size, help_size)],
                    // [Node.text("?")],
                  ),
                ],
              ),
            ],
          ),
        ],
      ),
      div(
        [Attr.id("code-container")],
        [
          BarDec.view(~font_metrics),
          Code.view_of_layout(
            ~id="under-the-rail",
            ~text_id="under-the-rail-text",
            ~font_metrics,
            ~zipper,
            ~filler=Model.filler(model),
            ~just_failed=history.just_failed,
            ~show_neighbor_tiles,
            DecPaths.empty, // dpaths,
            l,
          ),
        ],
      ),
    ],
  );
};
