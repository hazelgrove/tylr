[@deriving (show({with_path: false}), sexp, yojson, ord)]
type t =
  | Exp
  | Stat
  | Module
  | Typ;

let root = Module;
let all = [Exp, Typ, Module, Stat];

let to_str =
  fun
  | Typ => "Typ"
  | Module => "Item"
  | Stat => "Stat"
  | Exp => "Exp";

let of_str =
  fun
  | "Typ" => Typ
  | "Exp" => Exp
  | "Stat" => Stat
  | "Module" => Module
  | _ => raise(Invalid_argument("Sort.of_string: unrecognized sort"));

module Ord = {
  type nonrec t = t;
  let compare = compare;
};
module Map = Map.Make(Ord);
module Set = Set.Make(Ord);
