open Util;

[@deriving sexp]
type t = {
  id_gen: IdGen.t,
  selection: Selection.t,
  backpack: Backpack.t,
  relatives: Relatives.t,
};

let unselect = (z: t): t => {
  let relatives =
    z.relatives
    |> Relatives.prepend(
         Direction.toggle(z.selection.focus),
         z.selection.content,
       )
    |> Relatives.reassemble;
  let selection = Selection.clear(z.selection);
  {...z, selection, relatives};
};

/**
 * TODO connect selection to relatives
 */
let put_selection = (selection, z) => {...z, selection};

let insert_selection = (z: t): t => {
  let relatives = Relatives.insert(z.selection, z.relatives);
  let selection = Selection.clear(z.selection);
  {...z, selection, relatives};
};

// clear zipper selection, connect siblings,
// and return original selection contents
let remove_selection = (z: t): (Tiles.t, t) => {
  let relatives = Relatives.remove(z.selection, z.relatives);
  let selection = Selection.clear(z.selection);
  (z.selection.content, {...z, selection, relatives});
};
