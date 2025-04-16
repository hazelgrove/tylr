[@deriving (show({with_path: false}), sexp, yojson, ord)]
type t =
  | Exp
  | Pat
  | Typ;

let root = Exp;
let all = [Exp, Pat, Typ];

let to_str =
  fun
  | Typ => "Typ"
  | Pat => "Pat"
  | Exp => "Exp";

let of_str =
  fun
  | "Typ" => Typ
  | "Pat" => Pat
  | "Exp" => Exp
  | _ => raise(Invalid_argument("Sort.of_string: unrecognized sort"));

module Ord = {
  type nonrec t = t;
  let compare = compare;
};

module Map = Map.Make(Ord);
module Set = Set.Make(Ord);
