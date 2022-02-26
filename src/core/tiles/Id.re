open Sexplib.Std;

module T = {
  [@deriving sexp]
  type t = int;
  let compare = Int.compare;
};
include T;

module Map = {
  include Map.Make(T);
};
