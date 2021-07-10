open Sexplib.Std;
open Util;
open OptUtil.Syntax;

module Backpack = {
  // TODO make private
  [@deriving sexp]
  type t = (Direction.t, Selection.t, list(Selection.t));
  // let len = ((selection, ssframe)) =>
  //   List.length(ListFrame.to_list(~subject=[selection], ssframe));

  let bound_sort = ((_, selection, _): t) => {
    let (_, sort) =
      Selection.tip(Left, selection)
      |> OptUtil.get_or_fail(
           "backpack expected to carry only nonempty selections",
         );
    sort;
  };

  let pick_up_selection =
      (d: Direction.t, selection', (d_backpack, selection, rest): t)
      : option(t) => {
    let* (sort_l, sort_r) = Selection.tip_sorts(selection');
    if (sort_l != sort_r) {
      None;
    } else {
      let selections = {
        let ss = [selection, ...rest];
        d == d_backpack ? List.rev(ss) : ss;
      };
      Some((Direction.toggle(d), selection', selections));
    };
  };

  let of_shard = (shard: Shard.t): option(t) =>
    switch (shard) {
    | Pat(Paren_l) => Some((Right, [Shard(Pat(Paren_r))], []))
    | Pat(Paren_r) => Some((Left, [Shard(Pat(Paren_l))], []))
    | Exp(Paren_l) => Some((Right, [Shard(Exp(Paren_r))], []))
    | Exp(Paren_r) => Some((Left, [Shard(Exp(Paren_l))], []))
    | Exp(Cond_que) => Some((Right, [Shard(Exp(Cond_col))], []))
    | Exp(Cond_col) => Some((Left, [Shard(Exp(Cond_que))], []))
    | _ => None
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
let get_sframe = (~filter_selections=false, (prefix, suffix): frame) => {
  let prefix =
    prefix
    |> ListUtil.flat_map(
         fun
         | Selection(selection) =>
           filter_selections ? [] : List.rev(selection)
         | Tile(tile) => [Tile(tile)],
       );
  let suffix =
    suffix
    |> ListUtil.flat_map(
         fun
         | Selection(selection) => filter_selections ? [] : selection
         | Tile(tile) => [Tile(tile)],
       );
  (prefix, suffix);
};

[@deriving sexp]
type t = (Backpack.t, frame);
