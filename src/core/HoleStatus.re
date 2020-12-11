[@deriving sexp]
type t =
  | NotInHole
  | InHole;

let mk = (check: bool) => check ? NotInHole : InHole;
