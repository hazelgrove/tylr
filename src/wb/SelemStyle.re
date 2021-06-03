open Sexplib.Std;

[@deriving sexp]
type t =
  | Logo
  | Root
  | Filtered
  | Revealed({show_children: bool})
  | Selected;

let show_children =
  fun
  | Root
  | Filtered
  | Selected => true
  | Logo => false
  | Revealed({show_children}) => show_children;

let stretched =
  fun
  | Root
  | Filtered
  | Selected
  | Revealed(_) => false
  | Logo => true;

let highlighted =
  fun
  | Filtered
  | Revealed(_) => false
  | Selected
  | Logo
  | Root => true;
