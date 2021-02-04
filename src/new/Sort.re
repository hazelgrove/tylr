[@deriving sexp]
type t =
  | Typ
  | Pat
  | Exp;

let to_string =
  fun
  | Typ => "Typ"
  | Pat => "Pat"
  | Exp => "Exp";

let compare = (s, s') =>
  switch (s, s') {
  | (Exp, Exp) => 0
  | (Exp, _) => 1
  | (_, Exp) => (-1)
  | (Pat, Pat) => 0
  | (Pat, _) => 1
  | (_, Pat) => (-1)
  | (Typ, Typ) => 0
  };
