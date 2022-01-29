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
    | Undefined
    | Cant_move
    | Cant_construct(Sort.t, Sort.t)
    | Cant_pick_up_selection
    | Cant_put_down_selection(Sort.t, Sort.t)
    | Cant_construct_in_restructuring;
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
  let backpack = {
    let (backpack, picked_up) =
      z.selection.content
      |> Segment.fold_left_map(
           (backpack, p) =>
             switch (p) {
             | Grout(_)
             | Tile(_) => (backpack, None)
             | Shard(shard) =>
               Siblings.contains_matching(shard, z.siblings)
                 ? (backpack, Some(p))
                 : (
                   List.map(Segment.remove_matching(shard), backpack),
                   None,
                 )
             },
           z.backpack,
         );
    let picked_up = Segment.of_pieces(List.filter_map(Fun.id, picked_up));
    Backpack.pick_up(z.selection.focus, picked_up, backpack);
  };
  let (_, siblings) =
    Segment.connect(z.siblings, Ancestors.sort(z.ancestors));
  {...z, selection: Selection.clear(z.selection), backpack, siblings};
};

// let pick_up = (edit_state: EditState.t): EditState.t => {
//   let Zipper.{subject, frame} = edit_state.zipper;
//   let sort = Zipper.Frame.sort(frame);

// };

// let pick_up = (((down, up), frame): Zipper.t): Zipper.t => {
//   let sort = Zipper.Frame.sort(frame);
//   let (side, selection, affixes) = down;
//   let affixes = Segment.connect(affixes);
//   let down = ([], affixes);
//   switch (Segment.trim(selection)) {
//   | [] => ((down, up), frame)
//   | [_, ..._] as trimmed =>
//     let up = Subject.Up.extend(Direction.toggle(side), trimmed, up);
//     ((down, up), frame);
//   };
// };

// let put_down = (((down, up), frame): Zipper.t): result(Zipper.t) => {
//   let sort = Frame.sort(frame);
//   let (side, selection, affixes) = down;
//   switch (selection) {
//   | [_, ..._] =>
//     // todo revisit
//     // should support pasting over intact selections at least
//     Error(Undefined)
//   | [] =>
//     let up_side = Direction.toggle(side);
//     // todo better error
//     let+ (put_down, up) =
//       Result.of_option(
//         ~error=Failure.Undefined,
//         Subject.Up.pop(up_side, up),
//       );
//     let (prefix, suffix) =
//       Segment.connect(~insert=(up_side, put_down), affixes);
//     let affixes = (
//       Parser.reassemble_segment(Left, prefix),
//       Parser.reassemble_segment(Right, suffix),
//     );
//     let (affixes, frame) = Parser.reassemble_relatives(affixes, frame);
//     ((([], affixes), up), frame);
//   };
// };

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
