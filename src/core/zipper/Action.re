open Util;
open Zipper;

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

// clear zipper selection if balanced (otherwise
// raise Failure.Cant_overwrite_imbalanced_selection);
// replace with input tiles (s.t. cursor ends up on
// same side of tiles as original selection) and connect
// to siblings; and
// return original selection contents
// let overwrite_selection =
//     (tiles: Tiles.t, z: Zipper.t): Result.t((Tiles.t, Zipper.t)) =>
//   if (!Selection.is_balanced(z.selection)) {
//     Error(Failure.Cant_overwrite_imbalanced_selection);
//   } else {
//     let (cleared, z) = clear_selection(z);
//     let relatives =
//       z.relatives
//       |> Relatives.prepend(
//            ~connect=true,
//            Direction.toggle(z.selection.focus),
//            tiles,
//          )
//       |> Relatives.reassemble;
//     Ok((cleared, {...z, relatives}));
//   };

let move = (d: Direction.t, z: Zipper.t): option(Zipper.t) =>
  if (!Selection.is_empty(z.selection)) {
    // unselect current selection and move caret to
    // desired end of former selection if possible
    // (not possible if doing so would violate ordering
    // between shards up and shards down)
    d == z.selection.focus
    && !Selection.is_balanced(z.selection)
    && !Backpack.is_balanced(z.backpack)
      ? None
      : Some(
          z
          |> Zipper.put_selection({
               ...z.selection,
               focus: Direction.toggle(d),
             })
          |> Zipper.unselect,
        );
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
      |> Selection.map(Tiles.reassemble);
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

let destruct = (z: Zipper.t): Zipper.t => {
  let (selected, z) = Zipper.remove_selection(z);
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

let pick_up = (z: Zipper.t): Zipper.t => {
  let (selected, z) = Zipper.remove_selection(z);
  let selections =
    selected
    |> Tiles.split_by_grout
    |> Aba.get_a
    |> List.map(Selection.mk(z.selection.focus));
  let backpack =
    Backpack.push_s(
      (z.selection.focus == Left ? Fun.id : List.rev)(selections),
      z.backpack,
    );
  {...z, backpack};
};

let put_down = (z: Zipper.t): Result.t(Zipper.t) => {
  open Util.Result.Syntax;
  let+ (popped, backpack) =
    Result.of_option(
      ~error=Failure.Nothing_to_put_down,
      Backpack.pop(z.backpack),
    );
  destruct({...z, backpack})
  |> Zipper.put_selection(popped)
  |> Zipper.insert_selection;
};
// let rec put_down_all = (z: Zipper.t): Result.t(Zipper.t) =>
//   switch (put_down(z)) {
//   | Error(Nothing_to_put_down) => Ok(z)
//   | Error(_) as r => r
//   | Ok(z) => put_down_all(z)
//   };

let construct =
    (side: Direction.t, label: Label.t, z: Zipper.t): Result.t(Zipper.t) => {
  let (id, id_gen) = IdGen.next(z.id_gen);
  let mold = Relatives.default_mold(label, z.relatives);
  let to_pick_up =
    Shard.s_of_tile(id, mold, label)
    |> List.map(Tiles.of_shard)
    |> List.map(Selection.mk(side));
  let backpack = Backpack.push_s(to_pick_up, z.backpack);
  put_down({...z, id_gen, backpack});
};

let perform = (a: t, zipper: Zipper.t): Result.t(Zipper.t) =>
  switch (a) {
  | Move(d) => Result.of_option(~error=Failure.Cant_move, move(d, zipper))
  | Select(d) =>
    Result.of_option(~error=Failure.Cant_move, select(d, zipper))
  | Destruct => Ok(destruct(zipper))
  | Construct(side, form) => construct(side, form, zipper)
  | Pick_up => Ok(pick_up(zipper))
  | Put_down => put_down(zipper)
  };
