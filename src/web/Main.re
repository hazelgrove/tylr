open Incr_dom;

module App = {
  module Model = Model;
  module Action = Update;
  module State = State;

  let on_startup = (~schedule_action as _, _) => {
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
