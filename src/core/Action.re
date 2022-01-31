open Util;
open Zipper;

[@deriving sexp]
type t =
  | Move(Direction.t)
  | Select(Direction.t)
  | Remove
  // `Insert(d, form)` constructs `form` starting from `d` side
  | Insert(Direction.t, Tile.Label.t)
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

let clear_selection = (z: Zipper.t): (Segment.t, Zipper.t) => {
  let selection = Selection.clear(z.selection);
  let (_, siblings) =
    Segment.connect(z.siblings, Ancestors.sort(z.ancestors));
  (z.selection.content, {...z, selection, siblings});
};

let overwrite_selection =
    (segment: Segment.t, z: Zipper.t): Result.t((Segment.t, Zipper.t)) =>
  if (!Selection.is_balanced(z.selection)) {
    Error(Failure.Cant_overwrite_imbalanced_selection);
  } else {
    let (cleared, z) = clear_selection(z);
    let siblings = {
      let s = Ancestors.sort(z.ancestors);
      let (segment, siblings) =
        Segment.connect(~insert=segment, z.siblings, s);
      Siblings.prepend(z.selection.focus, segment, siblings);
    };
    let (siblings, ancestors) =
      Parser.reassemble_relatives(siblings, z.ancestors);
    Ok((cleared, {...z, siblings, ancestors}));
  };

let split_piece =
    (d: Direction.t, siblings, ancestors)
    : option((Segment.Piece.t, Siblings.t, Ancestors.t)) => {
  switch (Siblings.split_hd(d, siblings)) {
  | Some((p, siblings)) => Some((p, siblings, ancestors))
  | None =>
    switch (ancestors) {
    | [] => None
    | [ancestor, ...ancestors] =>
      open OptUtil.Syntax;
      let siblings' = Parser.disassemble_ancestor(ancestor);
      let+ (piece, siblings) =
        Siblings.(split_hd(d, concat([siblings, siblings'])));
      (piece, siblings, ancestors);
    }
  };
};

// unselect current selection and move caret to
// desired end of former selection if possible
// (not possible if doing so would violate ordering
// between shards up and shards down)
let unselect = (d: Direction.t, zipper: Zipper.t): option(Zipper.t) => {
  let {selection, backpack, siblings, ancestors, id_gen: _} = zipper;
  if (d == selection.focus
      && !Selection.is_balanced(selection)
      && !Backpack.is_balanced(backpack)) {
    None;
  } else {
    let siblings = Siblings.prepend(d, selection.content, siblings);
    let (siblings, ancestors) =
      Parser.reassemble_relatives(siblings, ancestors);
    Some({
      ...zipper,
      ancestors,
      siblings,
      selection: Selection.clear(selection),
    });
  };
};

let move = (d: Direction.t, z: Zipper.t): option(Zipper.t) =>
  if (!Selection.is_empty(z.selection)) {
    unselect(d, z);
  } else if (Backpack.is_balanced(z.backpack)) {
    open OptUtil.Syntax;
    let+ (p, siblings, ancestors) = split_piece(d, z.siblings, z.ancestors);
    let siblings = Siblings.cons(Direction.toggle(d), p, siblings);
    let (siblings, ancestors) =
      Parser.reassemble_relatives(siblings, ancestors);
    {...z, siblings, ancestors};
  } else {
    switch (Siblings.split_hd(d, z.siblings)) {
    | Some((Tile(_) as tile, siblings)) =>
      let siblings = Siblings.cons(Direction.toggle(d), tile, siblings);
      Some({...z, siblings});
    | _ => None
    };
  };

let select = (d: Direction.t, z: Zipper.t): option(Zipper.t) => {
  open OptUtil.Syntax;
  let grow = selection => {
    let+ (p, siblings, ancestors) = split_piece(d, z.siblings, z.ancestors);
    let selection =
      selection
      |> Selection.push(p)
      |> Selection.map_content(Parser.reassemble_segment);
    {...z, selection, siblings, ancestors};
  };
  if (d == z.selection.focus) {
    grow(z.selection);
  } else {
    switch (Selection.pop(z.selection)) {
    | None => grow(Selection.toggle_focus(z.selection))
    | Some((p, selection)) =>
      let siblings = Siblings.cons(z.selection.focus, p, z.siblings);
      let (siblings, ancestors) =
        Parser.reassemble_relatives(siblings, z.ancestors);
      Some({...z, selection, siblings, ancestors});
    };
  };
};

let remove = (z: Zipper.t): Zipper.t => {
  let (selected, z) = clear_selection(z);
  let (picked_up, removed) =
    Segment.shards(selected)
    |> List.partition(shard => Siblings.contains_matching(shard, z.siblings));
  let segments =
    picked_up |> List.map(shard => (Tiles.empty, [(shard, Tiles.empty)]));
  let backpack =
    removed
    |> List.fold_left(
         (backpack, shard) =>
           List.map(Segment.remove_matching(shard), backpack),
         z.backpack,
       )
    |> Backpack.pick_up(z.selection.focus, segments);
  {...z, backpack};
};

let pick_up = (z: Zipper.t): Zipper.t => {
  let (selected, z) = clear_selection(z);
  let segments = Segment.split(selected);
  let backpack = Backpack.pick_up(z.selection.focus, segments, z.backpack);
  {...z, backpack};
};

let put_down = (z: Zipper.t): Result.t(Zipper.t) => {
  open Util.Result.Syntax;
  let focus = z.selection.focus;
  let* (put_down, backpack) =
    Util.Result.of_option(
      ~error=Failure.Nothing_to_put_down,
      Backpack.put_down(focus, z.backpack),
    );
  let+ (_, z) = overwrite_selection(put_down, z);
  {...z, backpack};
};
let rec put_down_all = (z: Zipper.t): Zipper.t =>
  switch (put_down(z)) {
  | Error(_) => z
  | Ok(z) => put_down_all(z)
  };

let insert =
    (d: Direction.t, label: Tile.Label.t, z: Zipper.t): Result.t(Zipper.t) => {
  let z =
    d != z.selection.focus && !Backpack.is_balanced(z.backpack)
      ? put_down_all(z) : z;
  let (id, id_gen) = IdGen.next(z.id_gen);
  let tile = (id, label);
  let mold =
    Tile.default_mold(
      label,
      Ancestors.sort(z.ancestors),
      Siblings.sort(z.siblings),
    );
  let segments =
    label
    |> List.mapi((index, _) => {
         let shard = Shard.{tile, index, nibs: Tile.nibs(~index, mold)};
         Segment.of_pieces([Shard(shard)]);
       });
  put_down({
    ...z,
    id_gen,
    backpack: Backpack.pick_up(d, segments, z.backpack),
    selection: {
      ...z.selection,
      focus: d,
    },
  });
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
