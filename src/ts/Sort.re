[@deriving (show({with_path: false}), sexp, yojson, ord)]
type t =
  | Exp
  | Stat
  | Pat;

let root = Exp;
let all = [Exp, Pat, Stat];

let to_str =
  fun
  | Pat => "Pat"
  | Stat => "Stat"
  | Exp => "Exp";

let of_str =
  fun
  | "Pat" => Pat
  | "Exp" => Exp
  | "Stat" => Stat
  | _ => raise(Invalid_argument("Sort.of_string: unrecognized sort"));

module Ord = {
  type nonrec t = t;
  let compare = compare;
};
module Map = Map.Make(Ord);
module Set = Set.Make(Ord);
