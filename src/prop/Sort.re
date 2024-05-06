[@deriving (show({with_path: false}), sexp, yojson, ord)]
type t =
  | Stat;

let root = Stat;
let all = [Stat];

let to_str =
  fun
  | Stat => "Stat";

let of_str =
  fun
  | "Stat" => Stat
  | _ => raise(Invalid_argument("Sort.of_string: unrecognized sort"));

module Ord = {
  type nonrec t = t;
  let compare = compare;
};
module Map = Map.Make(Ord);
module Set = Set.Make(Ord);
