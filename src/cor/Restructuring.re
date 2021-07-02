open Sexplib.Std;
open Util;

module Backpack = {
  [@deriving sexp]
  type t = (Direction.t, Selection.t, list(Selection.t));
  // let len = ((selection, ssframe)) =>
  //   List.length(ListFrame.to_list(~subject=[selection], ssframe));

  let pick_up_selection =
      (d: Direction.t, selection', (d_backpack, selection, rest): t) => {
    let selections = {
      let ss = [selection, ...rest];
      d == d_backpack ? ss : List.rev(ss);
    };
    (Direction.toggle(d), selection', selections);
  };
};

// TODO create Relem module
[@deriving sexp]
type frame_elem =
  | Tile(Tile.t)
  | Selection(Selection.t);
[@deriving sexp]
type frame = ListFrame.t(frame_elem);

let is_tile =
  fun
  | Tile(_) => true
  | Selection(_) => false;

let len_elem =
  fun
  | Tile(_) => 1
  | Selection(selection) => List.length(selection);
let len = elems => elems |> List.map(len_elem) |> List.fold_left((+), 0);

let tip = d =>
  fun
  | Tile(selem) => Tile.tip(d, selem)
  | Selection(selection) =>
    Selection.tip(d, selection)
    |> OptUtil.get_or_fail(
         "expected nonempty selection in restructuring frame",
       );

let mk_relems =
  List.map(
    fun
    | Selem.Tile(tile) => Tile(tile)
    | Shard(_) as selem => Selection([selem]),
  );
let mk_frame = TupleUtil.map2(mk_relems);
let get_sframe = (~filter_selections=false, frame: frame) =>
  frame
  |> TupleUtil.map2(
       ListUtil.flat_map(
         fun
         | Selection(selection) => filter_selections ? [] : selection
         | Tile(tile) => [Tile(tile)],
       ),
     );

[@deriving sexp]
type t = (Backpack.t, frame);
