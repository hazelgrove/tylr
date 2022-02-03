[@deriving sexp]
type t = {
  id_gen: IdGen.t,
  selection: Selection.t,
  backpack: Backpack.t,
  siblings: Siblings.t,
  ancestors: Ancestors.t,
};
