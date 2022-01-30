open Util;
open Zipper;

[@deriving sexp]
type t =
  | Move(Direction.t)
  | Select(Direction.t)
  | Remove
  | Insert(Direction.t, Tile.Form.t)
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
  type t('success) = Result.t('success, Failure.t);
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
  let (picked_up, removed) =
    Segment.shards(z.selection.content)
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
  let (_, siblings) =
    Segment.connect(z.siblings, Ancestors.sort(z.ancestors));
  {...z, siblings, backpack, selection: Selection.clear(z.selection)};
};

let pick_up = (z: Zipper.t): Zipper.t => {
  let (trimmed, grouts) = Selection.trim(z.selection);
  let (_, siblings) =
    Segment.connect(
      Siblings.(concat([of_grouts(grouts), z.siblings])),
      Ancestors.sort(z.ancestors),
    );
  let backpack =
    Backpack.pick_up(z.selection.focus, [trimmed.content], z.backpack);
  {...z, selection: Selection.clear(z.selection), backpack, siblings};
};

let put_down = (z: Zipper.t): Result.t(Zipper.t) => {
  open Util.Result.Syntax;
  let focus = z.selection.focus;
  let* (put_down, backpack) =
    Util.Result.of_option(
      ~error=Failure.Nothing_to_put_down,
      Backpack.put_down(focus, z.backpack),
    );
  if (!Selection.is_balanced(z.selection)) {
    Error(Failure.Cant_overwrite_imbalanced_selection);
  } else {
    let z = remove(z);
    let siblings = {
      let s = Ancestors.sort(z.ancestors);
      let (put_down, siblings) =
        Segment.connect(~insert=put_down, z.siblings, s);
      Siblings.prepend(Direction.toggle(focus), put_down, siblings);
    };
    let (siblings, ancestors) =
      Parser.reassemble_relatives(siblings, z.ancestors);
    Ok({...z, backpack, siblings, ancestors});
  };
};

// let insert = (d: Direction.t, tokens: list(Token.t), (zipper: Zipper.t, id_gen: IdGen.t)) => {
//   let (((_, selection, affixes), _up), _frame) = zipper;
//   let (id, id_gen) = IdGen.next(id_gen);
//   let (insertion, picked_up) =
//     switch (tokens) {
//     | [_] => failwith
//     };

//   switch (tokens) {
//   | [_] =>
//     // make tile with tokens
//     // get its default mold
//     // make segment and connect between current zipper affixes
//     // meanwhile delete all selected tiles and either
//     //   (1) remove selected shards if they're the last to be picked up
//     //   (also remove their matching shards that were picked up), or
//     //   (2) otherwise pick up selected shards
//   | _ =>
//     // make shards of tokens
//     let shards =
//       tokens
//       |> List.mapi((i, token) => )
//     // connect d-end of shard list to current zipper
//     // put remaining shards in backpack (in some order tbd)
//   }
// };
let insert = _ => failwith("todo Action.insert");

// let perform = (a: t, edit_state: EditState.t): result(EditState.t) =>
//   switch (a) {
//   | Move(d) =>
//     Result.of_option(~error=Failure.Cant_move, move(d, edit_state))
//   | Select(d) =>
//     Result.of_option(~error=Failure.Cant_move, select(d, edit_state))
//   | Remove => Ok({...edit_state, zipper: remove(edit_state.zipper)})
//   | Insert(d, tokens) => insert(d, tokens, edit_state)
//   | Pick_up => Ok({...edit_state, zipper: pick_up(zipper)})
//   | Put_down =>
//     Result.of_option(~error=Failure.Empty_backpack, put_down(edit_state))
//   };
