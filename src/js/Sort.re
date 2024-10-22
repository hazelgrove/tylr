[@deriving (show({with_path: false}), sexp, yojson, ord)]
type t =
  | Prog
  | Stat
  | Exp
  | Pat;

let root = Prog;
let all = [Prog, Stat, Exp, Pat];

let to_str =
  fun
  | Pat => "Pat"
  | Stat => "Stat"
  | Exp => "Exp"
  | Prog => "Prog";

let of_str =
  fun
  | "Pat" => Pat
  | "Exp" => Exp
  | "Stat" => Stat
  | "Prog" => Prog
  | _ => raise(Invalid_argument("Sort.of_string: unrecognized sort"));

module Ord = {
  type nonrec t = t;
  let compare = compare;
};
module Map = Map.Make(Ord);
module Set = Set.Make(Ord);