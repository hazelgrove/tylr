open Js_of_ocaml;
open Incr_dom;

module App = {
  module Model = Model;
  module Action = Update;
  module State = State;

  let on_startup = (~schedule_action, _) => {
    let _ =
      ResizeObserver.observe(
        ~node=JsUtil.get_elem_by_id("font-specimen"),
        ~f=
          (entries, _) => {
            let specimen = Js_of_ocaml.Js.to_array(entries)[0];
            let rect = specimen##.contentRect;
            schedule_action(
              Update.SetFontMetrics({
                row_height: rect##.bottom -. rect##.top,
                col_width: rect##.right -. rect##.left,
              }),
            );
          },
        (),
      );

    // preserve editor focus across window focus/blur
    Dom_html.window##.onfocus :=
      Dom_html.handler(_ => {
        Page.focus_code();
        Js._true;
      });
    Page.focus_code();

    Async_kernel.Deferred.return();
  };

  let create = (model: Incr.t(Model.t), ~old_model as _, ~inject) => {
    open Incr.Let_syntax;
    let%map model = model;
    Component.create(
      ~apply_action=Update.apply(model),
      model,
      Page.view(~inject, model),
    );
  };
};

Incr_dom.Start_app.start(
  (module App),
  ~debug=false,
  ~bind_to_element_with_id="container",
  ~initial_model=Model.init(),
);
