open Util;

// assuming single backpack, shards may appear in selection, backpack, or siblings
[@deriving show]
type t = {
  // id_gen: IdGen.t,
  selection: Selection.t,
  backpack: Backpack.t,
  relatives: Relatives.t,
};

module Action = {
  [@deriving (show, sexp)]
  type t =
    | Move(Direction.t)
    | Select(Direction.t)
    | Destruct
    // `Construct(d, lbl)` constructs `lbl` starting from `d` side
    | Construct(Direction.t, Tile.Label.t)
    | Pick_up
    | Put_down;

  module Failure = {
    [@deriving (show, sexp)]
    type t =
      | Cant_move
      | Nothing_to_put_down;
  };

  module Result = {
    include Result;
    type t('success) = Result.t('success, Failure.t);
  };
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

let update_selection = (sel: Selection.t, z: t): (Selection.t, t) => {
  let old_sel = z.selection;
  let z = unselect({...z, selection: sel});
  let sort_rank = Relatives.sort_rank(z.relatives);
  let grout_rank = Relatives.grout_rank(z.relatives);
  let molds =
    Relatives.mold(z.relatives)
    |> List.sort((molds, molds') => {
         let (s, s') = (sort_rank(molds), sort_rank(molds'));
         let (g, g') = (grout_rank(molds), grout_rank(molds'));
         if (s < s') {
           (-1);
         } else if (s > s') {
           1;
         } else {
           Int.compare(g, g');
         };
       })
    |> List.hd;
  let relatives = Relatives.regrout(molds, z.relatives);
  (old_sel, {...z, /* molds */ relatives});
};

let put_selection = (sel, z) => snd(update_selection(sel, z));

let grow_selection = (z: t): option(t) => {
  open OptUtil.Syntax;
  let+ (p, relatives) =
    Relatives.pop(~balanced=false, z.selection.focus, z.relatives);
  let selection = Selection.push(p, z.selection);
  {...z, selection, relatives};
};

// toggles focus and grows if selection is empty
let shrink_selection = (z: t): option(t) => {
  switch (Selection.pop(z.selection)) {
  | None =>
    let selection = Selection.toggle_focus(z.selection);
    grow_selection({...z, selection});
  | Some((p, selection)) =>
    let relatives = Relatives.push(selection.focus, p, z.relatives);
    Some({...z, selection, relatives});
  };
};

// balanced specifies whether shifted piece must be balanced
let shift_piece = (~balanced: bool, from: Direction.t, z: t): option(t) => {
  open OptUtil.Syntax;
  let+ (p, relatives) = Relatives.pop(~balanced, from, z.relatives);
  let relatives = Relatives.push(Direction.toggle(from), p, relatives);
  {...z, relatives};
};

let move = (d: Direction.t, z: t): option(t) =>
  if (Selection.is_empty(z.selection)) {
    let balanced = !Backpack.is_balanced(z.backpack);
    let from = Direction.toggle(d);
    shift_piece(~balanced, from, z);
  } else {
    // TODO restore logic attempting to move d
    Some(unselect(z));
  };

let select = (d: Direction.t, z: t): option(t) =>
  d == z.selection.focus ? grow_selection(z) : shrink_selection(z);

let destruct = (z: t): t => {
  let (selected, z) = update_selection(Selection.empty, z);
  let (to_pick_up, to_remove) =
    Segment.shards(selected)
    |> List.partition(shard =>
         Siblings.contains_matching(shard, z.relatives.siblings)
       );
  let backpack =
    z.backpack
    |> Backpack.remove_matching(to_remove)
    |> Backpack.push_s(
         to_pick_up
         |> List.map(Segment.of_shard)
         |> List.map(Selection.mk(z.selection.focus)),
       );
  {...z, backpack};
};

let pick_up = (z: t): t => {
  let (selected, z) = update_selection(Selection.empty, z);
  let selections =
    selected
    |> Segment.split_by_grout
    |> Aba.get_a
    |> List.filter((!=)(Segment.empty))
    |> List.map(Selection.mk(z.selection.focus));
  let backpack =
    Backpack.push_s(
      (z.selection.focus == Left ? Fun.id : List.rev)(selections),
      z.backpack,
    );
  {...z, backpack};
};

let put_down = (z: t): option(t) => {
  open OptUtil.Syntax;
  let z = destruct(z);
  let+ (popped, backpack) = Backpack.pop(z.backpack);
  {...z, backpack} |> put_selection(popped) |> unselect;
};

let construct = (from: Direction.t, label: Tile.Label.t, z: t): t => {
  let z = destruct(z);
  let mold = Relatives.default_mold(label, z.relatives);
  let selections =
    Shard.mk_s(label, mold)
    |> List.map(Segment.of_shard)
    |> List.map(Selection.mk(from))
    |> ListUtil.rev_if(from == Right);
  let backpack = Backpack.push_s(selections, z.backpack);
  Option.get(put_down({...z, backpack}));
};

let perform = (a: Action.t, z: t): Action.Result.t(t) =>
  switch (a) {
  | Move(d) => Result.of_option(~error=Action.Failure.Cant_move, move(d, z))
  | Select(d) =>
    Result.of_option(~error=Action.Failure.Cant_move, select(d, z))
  | Destruct => Ok(destruct(z))
  | Construct(from, label) => Ok(construct(from, label, z))
  | Pick_up => Ok(pick_up(z))
  | Put_down =>
    Result.of_option(~error=Action.Failure.Nothing_to_put_down, put_down(z))
  };
