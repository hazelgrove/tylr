// open Util;

[@deriving show]
type t =
  | Any
  | Pat
  | Exp;

let root = Exp;

let consistent = (s, s') =>
  switch (s, s') {
  | (Any, _)
  | (_, Any) => true
  | _ => s == s'
  };

let to_string =
  fun
  | Any => "Any"
  | Pat => "Pat"
  | Exp => "Exp";

let to_proper_string =
  fun
  | Any => "any"
  | Pat => "pattern"
  | Exp => "expression";
