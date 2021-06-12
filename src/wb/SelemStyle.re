open Sexplib.Std;

[@deriving sexp]
type t =
  | Logo
  | Root
  | Filtered
  | Revealed({show_children: bool})
  | Selected;

let to_string =
  fun
  | Logo => "Logo"
  | Root => "Root"
  | Filtered => "Filtered"
  | Revealed(_) => "Revealed"
  | Selected => "Selected";

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

let filtered =
  fun
  | Filtered => true
  | _ => false;
