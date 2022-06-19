[@deriving show]
type t =
  | Any
  | Pat
  | Exp;

let root = Exp;

let all = [Any, Pat, Exp];

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
