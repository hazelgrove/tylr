open Sexplib.Std;

[@deriving sexp]
type t = unit;

let apply = (model: Model.t, _: t, _: State.t, ~schedule_action as _) => model;
