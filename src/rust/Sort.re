[@deriving (show({with_path: false}), sexp, yojson, ord)]
type t =
  | Exp
  | Pat
  | Stat
   | Item
  | Typ;

let root = Item;
let all = [Exp, Pat, Typ, Item, Stat];


let to_str =
  fun
  | Typ => "Typ"
  | Pat => "Pat"
  // | Rul => "Rul"
  | Item => "Item"
  | Stat => "Stat"
  | Exp => "Exp";

let of_str =
  fun
  | "Typ" => Typ
  | "Pat" => Pat
  | "Exp" => Exp
  | "Stat" => Stat
  | "Item" => Item
  | _ => raise(Invalid_argument("Sort.of_string: unrecognized sort"));

module Ord = {
  type nonrec t = t;
  let compare = compare;
};
module Map = Map.Make(Ord);
module Set = Set.Make(Ord);
