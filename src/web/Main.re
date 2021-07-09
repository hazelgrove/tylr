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
    let _ =
      observe_font_specimen("logo-font-specimen", fm =>
        schedule_action(Web.Update.SetLogoFontMetrics(fm))
      );
    Os.is_mac :=
      Dom_html.window##.navigator##.platform##toUpperCase##indexOf(
        Js.string("MAC"),
      )
      >= 0;
    Async_kernel.Deferred.return();
  };

  let create = (model: Incr.t(Web.Model.t), ~old_model as _, ~inject) => {
    open Incr.Let_syntax;
    let%map model = model;
    Component.create(
      ~apply_action=Web.Update.apply(model),
      // ~on_display=
      //   (_, ~schedule_action as _) => {print_endline("on_display")},
      model,
      Web.Page.view(~inject, model),
    );
  };
};

Incr_dom.Start_app.start(
  (module App),
  ~debug=false,
  ~bind_to_element_with_id="container",
  ~initial_model=Web.Model.init(),
);
