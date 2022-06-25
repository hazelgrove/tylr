open Js_of_ocaml;
open Incr_dom;
open Web;

module App = {
  module Model = Model;
  module Action = Update;
  module State = State;

  let observe_font_specimen = (id, update) =>
    ResizeObserver.observe(
      ~node=JsUtil.get_elem_by_id(id),
      ~f=
        (entries, _) => {
          let specimen = Js.to_array(entries)[0];
          let rect = specimen##.contentRect;
          update(
            Web.FontMetrics.{
              row_height: rect##.bottom -. rect##.top,
              col_width: rect##.right -. rect##.left,
            },
          );
        },
      (),
    );

  let on_startup = (~schedule_action, _) => {
    let _ =
      observe_font_specimen("font-specimen", fm =>
        schedule_action(Web.Update.SetFontMetrics(fm))
      );
    // let _ =
    //   observe_font_specimen("logo-font-specimen", fm =>
    //     schedule_action(Web.Update.SetLogoFontMetrics(fm))
    //   );
    Os.is_mac :=
      Dom_html.window##.navigator##.platform##toUpperCase##indexOf(
        Js.string("MAC"),
      )
      >= 0;
    Async_kernel.Deferred.return();
  };

  let restart_caret_animation = () =>
    // necessary to trigger reflow
    // <https://css-tricks.com/restart-css-animation/>
    try({
      let caret_elem = JsUtil.get_elem_by_id("caret");
      caret_elem##.classList##remove(Js.string("blink"));
      let _ = caret_elem##getBoundingClientRect;
      caret_elem##.classList##add(Js.string("blink"));
    }) {
    | _ => ()
    };

  let create = (model: Incr.t(Web.Model.t), ~old_model as _, ~inject) => {
    open Incr.Let_syntax;
    let%map model = model;
    Component.create(
      ~apply_action=
        (action, state, ~schedule_action) => {
          restart_caret_animation();
          try(Web.Update.apply(model, action, state, ~schedule_action)) {
          | exc =>
            Printf.printf(
              "ERROR: exception in update: %s\n",
              Printexc.to_string(exc),
            );
            model;
          };
        },
      // ~on_display=
      //   (_, ~schedule_action as _) => {print_endline("on_display")},
      model,
      Web.Page.view(~inject, model),
    );
  };
};

let initial_model: Model.t =
  Update.apply(Model.blank, LoadInit, (), ~schedule_action=());

Incr_dom.Start_app.start(
  (module App),
  ~debug=false,
  ~bind_to_element_with_id="container",
  ~initial_model,
);
