module T = {
  [@deriving show]
  type t = int;
  let compare = Int.compare;
};
include T;

module Map = Map.Make(T);
