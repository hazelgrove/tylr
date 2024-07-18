[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Exp
  | Pat
  | Typ;

let root = Exp;
let all = [Exp, Pat, Typ];

let compare = (l: t, r: t) => {
  // bug if these Option.get calls fail
  let l = Option.get(Stds.Lists.find_index((==)(l), all));
  let r = Option.get(Stds.Lists.find_index((==)(r), all));
  Int.compare(l, r);
};

// let to_int =
//   fun
//   | Exp => 0
//   | Pat => 1
//   | Typ => 2;
// let of_int =
//   fun
//   | 0 => Exp
//   | 1 => Pat
//   | 2 => Typ
//   | _ => raise(Invalid_argument(""));
// let compare = (s1, s2) => Int.compare(to_int(s1), to_int(s2));
// let lca = (s1, s2) => of_int(Int.min(to_int(s1), to_int(s2)));

let to_str =
  fun
  | Typ => "Typ"
  | Pat => "Pat"
  // | Rul => "Rul"
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
