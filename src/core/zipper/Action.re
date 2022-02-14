open Util;
open Zipper;

[@deriving sexp]
type t =
  | Move(Direction.t)
  | Select(Direction.t)
  | Remove
  // `Insert(d, form)` constructs `form` starting from `d` side
  | Insert(Direction.t, Label.t)
  | Pick_up
  | Put_down;

module Failure = {
  [@deriving sexp]
  type t =
    | Cant_move
    | Nothing_to_put_down
    | Cant_overwrite_imbalanced_selection;
};

module Result = {
  include Result;
  type t('success) = Result.t('success, Failure.t);
};

// clear zipper selection, connect siblings,
// and return original selection contents
let clear_selection = (z: Zipper.t): (Tiles.t, Zipper.t) => {
  let selection = Selection.clear(z.selection);
  let relatives = Relatives.connect(z.relatives);
  (z.selection.content, {...z, selection, relatives});
};

// clear zipper selection if balanced (otherwise
// raise Failure.Cant_overwrite_imbalanced_selection);
// replace with input tiles (s.t. cursor ends up on
// same side of tiles as original selection) and connect
// to siblings; and
// return original selection contents
let overwrite_selection =
    (tiles: Tiles.t, z: Zipper.t): Result.t((Tiles.t, Zipper.t)) =>
  if (!Selection.is_balanced(z.selection)) {
    Error(Failure.Cant_overwrite_imbalanced_selection);
  } else {
    let (cleared, z) = clear_selection(z);
    let relatives =
      z.relatives
      |> Relatives.prepend(
           ~connect=true,
           Direction.toggle(z.selection.focus),
           tiles,
         )
      |> Relatives.reassemble;
    Ok((cleared, {...z, relatives}));
  };

// unselect current selection and move caret to
// desired end of former selection if possible
// (not possible if doing so would violate ordering
// between shards up and shards down)
let unselect = (d: Direction.t, z: Zipper.t): option(Zipper.t) =>
  if (d == z.selection.focus
      && !Selection.is_balanced(z.selection)
      && !Backpack.is_balanced(z.backpack)) {
    None;
  } else {
    let relatives =
      z.relatives
      |> Relatives.prepend(d, z.selection.content)
      |> Relatives.reassemble;
    let selection = Selection.clear(z.selection);
    Some({...z, selection, relatives});
  };

let move = (d: Direction.t, z: Zipper.t): option(Zipper.t) =>
  if (!Selection.is_empty(z.selection)) {
    // todo: make unselect not reassemble
    unselect(d, z);
  } else if (Backpack.is_balanced(z.backpack)) {
    open OptUtil.Syntax;
    let+ (piece, relatives) = Relatives.split_piece(d, z.relatives);
    let relatives =
      relatives
      |> Relatives.cons_piece(Direction.toggle(d), piece)
      |> Relatives.reassemble;
    {...z, relatives};
  } else {
    open OptUtil.Syntax;
    let* (tile, relatives) = Relatives.split_tile(d, z.relatives);
    switch (tile) {
    | Intact(_) =>
      let relatives = Relatives.cons(Direction.toggle(d), tile, z.relatives);
      Some({...z, relatives});
    | Pieces(pieces) =>
      let (piece, siblings) = Aba.split_end(Direction.toggle(d), pieces);
      switch (piece) {
      | Shard(_) => None
      | Grout(_) =>
        let siblings =
          siblings
          |> TupleUtil.map2(
               Baba.map_to_list(Fun.id, p => [Tile.of_piece(p)]),
             )
          |> TupleUtil.map2(List.concat);
        let relatives =
          relatives
          |> Relatives.cat(siblings)
          |> Relatives.cons_piece(Direction.toggle(d), piece)
          |> Relatives.reassemble;
        Some({...z, relatives});
      };
    };
  };

let select = (d: Direction.t, z: Zipper.t): option(Zipper.t) => {
  open OptUtil.Syntax;
  let grow = (selection: Selection.t) => {
    let+ (piece, relatives) =
      Relatives.split_piece(selection.focus, z.relatives);
    let selection =
      selection
      |> Selection.push(Tile.of_piece(piece))
      |> Selection.map_content(Tiles.reassemble);
    {...z, selection, relatives};
  };
  if (d == z.selection.focus) {
    grow(z.selection);
  } else {
    switch (Selection.split_piece(z.selection)) {
    | None => grow(Selection.toggle_focus(z.selection))
    | Some((piece, selection)) =>
      let relatives =
        z.relatives
        |> Relatives.cons_piece(selection.focus, piece)
        |> Relatives.reassemble;
      Some({...z, relatives});
    };
  };
};

let remove = (z: Zipper.t): Zipper.t => {
  let (selected, z) = clear_selection(z);
  let (to_pick_up, to_remove) =
    Tiles.shards(selected)
    |> List.partition(shard =>
         Siblings.contains_matching(shard, z.relatives.siblings)
       )
    |> PairUtil.map_fst(List.map(Tiles.of_shard));
  let backpack =
    z.backpack
    |> Backpack.remove(to_remove)
    |> Backpack.pick_up(z.selection.focus, to_pick_up);
  {...z, backpack};
};

let pick_up = (z: Zipper.t): Zipper.t => {
  let (selected, z) = clear_selection(z);
  let tiles = Aba.get_a(Tiles.split_by_grout(selected));
  let backpack = Backpack.pick_up(z.selection.focus, tiles, z.backpack);
  {...z, backpack};
};

let put_down = (z: Zipper.t): Result.t(Zipper.t) => {
  open Util.Result.Syntax;
  let* (put_down, backpack) =
    z.backpack
    |> Backpack.put_down(z.selection.focus)
    |> Util.Result.of_option(~error=Failure.Nothing_to_put_down);
  let+ (_, z) = overwrite_selection(put_down, z);
  {...z, backpack};
};
let rec put_down_all = (z: Zipper.t): Result.t(Zipper.t) =>
  switch (put_down(z)) {
  | Error(Nothing_to_put_down) => Ok(z)
  | Error(_) as r => r
  | Ok(z) => put_down_all(z)
  };

let insert =
    (d: Direction.t, label: Label.t, z: Zipper.t): Result.t(Zipper.t) => {
  open Util.Result.Syntax;
  let* z =
    d != z.selection.focus && !Backpack.is_balanced(z.backpack)
      ? put_down_all(z) : Ok(z);
  let (id, id_gen) = IdGen.next(z.id_gen);
  let mold = Relatives.default_mold(label, z.relatives);
  let to_pick_up =
    Shard.s_of_tile(id, mold, label)
    |> List.map(shard => Tiles.of_shard(shard));
  let selection = {...z.selection, focus: d};
  let backpack = Backpack.pick_up(d, to_pick_up, z.backpack);
  put_down({...z, id_gen, selection, backpack});
};

let perform = (a: t, zipper: Zipper.t): Result.t(Zipper.t) =>
  switch (a) {
  | Move(d) => Result.of_option(~error=Failure.Cant_move, move(d, zipper))
  | Select(d) =>
    Result.of_option(~error=Failure.Cant_move, select(d, zipper))
  | Remove => Ok(remove(zipper))
  | Insert(d, form) => insert(d, form, zipper)
  | Pick_up => Ok(pick_up(zipper))
  | Put_down => put_down(zipper)
  };
