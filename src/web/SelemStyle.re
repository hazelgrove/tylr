[@deriving show]
type t =
  | Logo
  | Root
  | Selected;

let stretched =
  fun
  | Root
  | Selected => false
  | Logo => true;

let highlighted =
  fun
  | Selected => false
  | Logo
  | Root => true;

let selected =
  fun
  | Selected => true
  | _ => false;
