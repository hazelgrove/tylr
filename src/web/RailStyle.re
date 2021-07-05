open Sexplib.Std;

[@deriving sexp]
type t = {
  atomic: bool,
  color: Color.t,
};
