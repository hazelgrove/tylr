open Util;

module Backpack = {
  [@deriving sexp]
  type t = (Selection.t, ListFrame.t(Selection.t));

  let len = ((selection, ssframe)) =>
    List.length(ListFrame.to_list(~subject=[selection], ssframe));
};

// TODO reorganize types/modules
[@deriving sexp]
type frame_elem =
  | Selem(Selem.t)
  | Selec(Selection.t);
[@deriving sexp]
type frame = ListFrame.t(frame_elem);

let is_tile =
  fun
  | Selem(selem) => Selem.is_tile(selem)
  | _ => false;

let len_elem =
  fun
  | Selem(_) => 1
  | Selec(selection) => List.length(selection);
let len = elems => elems |> List.map(len_elem) |> List.fold_left((+), 0);

let tip = d =>
  fun
  | Selem(selem) => Selem.tip(d, selem)
  | Selec(selection) =>
    Selection.tip(d, selection)
    |> OptUtil.get_or_fail(
         "expected nonempty selection in restructuring frame",
       );

let mk_frame = (sframe: Selection.frame) =>
  TupleUtil.map2(List.map(s => Selem(s)), sframe);
let get_sframe = (~filter_selections=false, frame: frame) =>
  frame
  |> TupleUtil.map2(
       ListUtil.flat_map(
         fun
         | Selec(selection) => filter_selections ? [] : selection
         | Selem(selem) => [selem],
       ),
     );

[@deriving sexp]
type t = (Backpack.t, frame);
