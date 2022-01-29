open Sexplib.Std;

[@deriving sexp]
type t = list(Grout.t);

let empty = [];
let size = List.length;

let adjust = (_: Nibs.t, _: t): t => failwith("Grouts.adjust");

module Affix = {
  type nonrec t = t;
  let size = size;
};

module Frame = {
  type t = (Affix.t, Affix.t);
  let size = ((prefix, suffix)) => Affix.size(prefix) + Affix.size(suffix);
};
