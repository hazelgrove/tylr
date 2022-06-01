open Sexplib.Std;

// make an enum
[@deriving (show, sexp)]
type t = string;

module Index = {
  type t = int;
};
