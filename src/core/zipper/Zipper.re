open Util;

[@deriving sexp]
type t = {
  id_gen: IdGen.t,
  selection: Selection.t,
  backpack: Backpack.t,
  relatives: Relatives.t,
};

module Action = {
  [@deriving sexp]
  type t =
    | Move(Direction.t)
    | Select(Direction.t)
    | Destruct
    // `Construct(d, form)` constructs `form` starting from `d` side
    | Construct(Direction.t, Label.t)
    | Pick_up
    | Put_down;

  module Failure = {
    [@deriving sexp]
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

/**
 * TODO connect to relatives
 */
let update_selection = (selection, z) => (
  failwith("todo update"),
  {...z, selection},
);

// let insert_selection = (z: t): t => {
//   let relatives = Relatives.insert(z.selection, z.relatives);
//   let selection = Selection.clear(z.selection);
//   {...z, selection, relatives};
// };

// // clear zipper selection, connect siblings,
// // and return original selection contents
// let remove_selection = (z: t): (Tiles.t, t) => {
//   let relatives = Relatives.remove(z.selection, z.relatives);
//   let selection = Selection.clear(z.selection);
//   (z.selection.content, {...z, selection, relatives});
// };

let grow_selection = (z: t): option(t) => {
  open OptUtil.Syntax;
  let+ (p, relatives) = Relatives.pop(z.selection.focus, z.relatives);
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

let shift_relative = (from: Direction.t, z: t): option(t) => {
  open OptUtil.Syntax;
  let+ (p, relatives) = Relatives.pop(from, z.relatives);
  let relatives = Relatives.push(Direction.toggle(from), p, relatives);
  {...z, relatives};
};

let shift_balanced_relative = (from: Direction.t, z: t): option(t) => {
  open OptUtil.Syntax;
  let+ (tile, relatives) = Relatives.pop_balanced(from, z.relatives);
  let relatives =
    Relatives.push_tile(Direction.toggle(from), tile, relatives);
  {...z, relatives};
};

let move = (d: Direction.t, z: t): option(t) =>
  if (!Selection.is_empty(z.selection)) {
    // TODO restore logic attempting to move d
    Some(unselect(z));
  } else if (Backpack.is_balanced(z.backpack)) {
    shift_relative(d, z);
  } else {
    shift_balanced_relative(d, z);
  };

let select = (d: Direction.t, z: t): option(t) =>
  d == z.selection.focus ? grow_selection(z) : shrink_selection(z);

let destruct = (z: t): t => {
  let (selected, z) = update_selection(Selection.empty, z);
  let (to_pick_up, to_remove) =
    Tiles.shards(selected)
    |> List.partition(shard =>
         Siblings.contains_matching(shard, z.relatives.siblings)
       );
  let backpack =
    z.backpack
    |> Backpack.remove_matching(to_remove)
    |> Backpack.push_s(
         to_pick_up
         |> List.map(Tiles.of_shard)
         |> List.map(Selection.mk(z.selection.focus)),
       );
  {...z, backpack};
};

let construct = (from: Direction.t, label: Label.t, z: t): t => {
  let (id, id_gen) = IdGen.next(z.id_gen);
  let mold = Relatives.default_mold(label, z.relatives);
  let selections =
    Shard.s_of_tile(id, mold, label)
    |> List.map(Tiles.of_shard)
    |> List.map(Selection.mk(from));
  let (inserted, picked_up) =
    switch (from) {
    | Left => ListUtil.split_first(selections)
    | Right =>
      let (ss, s) = ListUtil.split_last(selections);
      (s, ss);
    };
  let backpack = Backpack.push_s(picked_up, z.backpack);
  {...z, id_gen, backpack}
  |> destruct
  |> update_selection(inserted)
  |> snd
  |> unselect;
};

let pick_up = (z: t): t => {
  let (selected, z) = update_selection(Selection.empty, z);
  let selections =
    selected
    |> Tiles.split_by_grout
    |> Aba.get_a
    |> List.filter((!=)(Tiles.empty))
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
  let+ (popped, backpack) = Backpack.pop(z.backpack);
  {...z, backpack} |> destruct |> update_selection(popped) |> snd |> unselect;
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
