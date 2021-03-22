// open Util;

[@deriving sexp]
type t = Action_make.t;

// TODO perform type signatures are starting to get complicated...
// should probably make edit state index-based to support better
// separation of edit mode from tree/tiles
let rec perform =
        (~delete_whole_selection=false, a: t, zipper: EditState.t)
        : option(EditState.t) =>
  switch (zipper) {
  | Typ(zipper) =>
    Action_typ.perform(
      ~perform_edit_state=perform,
      ~delete_whole_selection,
      a,
      zipper,
    )
  | Pat(zipper) =>
    Action_pat.perform(
      ~perform_edit_state=perform,
      ~delete_whole_selection,
      a,
      zipper,
    )
  | Exp(zipper) =>
    Action_exp.perform(
      ~perform_edit_state=perform,
      ~delete_whole_selection,
      a,
      zipper,
    )
  };
