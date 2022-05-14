[@deriving show]
type t =
  | Logo
  | Root
  | Revealed({show_children: bool})
  | Selected;

let to_string =
  fun
  | Logo => "Logo"
  | Root => "Root"
  | Revealed(_) => "Revealed"
  | Selected => "Selected";

let show_children =
  fun
  | Root => true
  | Selected
  | Logo => false
  | Revealed({show_children}) => show_children;

let stretched =
  fun
  | Root
  | Selected
  | Revealed(_) => false
  | Logo => true;

let highlighted =
  fun
  | Revealed(_)
  | Selected => false
  | Logo
  | Root => true;

let selected =
  fun
  | Selected => true
  | _ => false;
