open Js_of_ocaml;
open Incr_dom;

module App = {
  module Model = Wb.Model;
  module Action = Wb.Update;
  module State = Wb.State;

  let observe_font_specimen = (id, update) =>
    ResizeObserver.observe(
      ~node=JsUtil.get_elem_by_id(id),
      ~f=
        (entries, _) => {
          let specimen = Js_of_ocaml.Js.to_array(entries)[0];
          let rect = specimen##.contentRect;
          update(
            Wb.FontMetrics.{
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
        schedule_action(Wb.Update.SetFontMetrics(fm))
      );
    let _ =
      observe_font_specimen("logo-font-specimen", fm =>
        schedule_action(Wb.Update.SetLogoFontMetrics(fm))
      );
    // let _ =
    //   observe_font_specimen("type-font-specimen", fm =>
    //     schedule_action(Wb.Update.SetTypeFontMetrics(fm))
    //   );

    // preserve editor focus across window focus/blur
    Dom_html.window##.onfocus :=
      Dom_html.handler(_ => {
        Page.focus_code();
        Js._true;
      });
    Wb.Page.focus_code();

    Async_kernel.Deferred.return();
  };

  let create = (model: Incr.t(Wb.Model.t), ~old_model as _, ~inject) => {
    open Incr.Let_syntax;
    let%map model = model;
    Component.create(
      ~apply_action=Wb.Update.apply(model),
      model,
      Wb.Page.view(~inject, model),
    );
  };
};

Incr_dom.Start_app.start(
  (module App),
  ~debug=false,
  ~bind_to_element_with_id="container",
  ~initial_model=Wb.Model.init(),
);
