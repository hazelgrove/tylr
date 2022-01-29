open Util;

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

// TODO maybe move this
let split_piece =
    (d: Direction.t, affixes: Segment.Frame.t, frame: Zipper.Frame.t)
    : option((Segment.Piece.t, Segment.Frame.t, Zipper.Frame.t)) => {
  switch (Segment.Frame.split_hd(d, affixes)) {
  | Some((piece, affixes)) => Some((piece, affixes, frame))
  | None =>
    open OptUtil.Syntax;
    let* (affixes', frame) = Parser.disassemble_frame(frame);
    let+ (piece, affixes) =
      Segment.Frame.(split_hd(d, concat([affixes, affixes'])));
    (piece, affixes, frame);
  };
};

// unselect current selection and move caret to
// desired end of former selection if possible
// (not possible if doing so would violate ordering
// between shards up and shards down)
let unselect = (d: Direction.t, edit_state: EditState.t): option(EditState.t) => {
  let Zipper.{subject: {focus, selection, affixes}, frame} =
    edit_state.zipper;
  if (d == focus
      && !Segment.is_balanced(selection)
      && !Backpack.is_balanced(edit_state.backpack)) {
    None;
  } else {
    let affixes = Segment.Frame.prepend(d, selection, affixes);
    let (affixes, frame) = Parser.assemble_zipper(affixes, frame);
    Some({
      ...edit_state,
      zipper: {
        frame,
        subject: {
          affixes,
          selection: Segment.empty,
          focus: d,
        },
      },
    });
  };
};

let move = (d: Direction.t, edit_state: EditState.t): option(EditState.t) => {
  let EditState.{backpack, zipper, id_gen: _} = edit_state;
  let Zipper.{subject, frame} = zipper;
  if (subject.selection != Segment.empty) {
    unselect(d, edit_state);
  } else if (Backpack.is_balanced(backpack)) {
    open OptUtil.Syntax;
    let+ (piece, affixes, frame) = split_piece(d, subject.affixes, frame);
    let affixes = Segment.Frame.cons(Direction.toggle(d), piece, affixes);
    let (affixes, frame) = Parser.assemble_zipper(affixes, frame);
    let subject = {...subject, affixes};
    {
      ...edit_state,
      zipper: {
        frame,
        subject,
      },
    };
  } else {
    switch (Segment.Frame.split_hd(d, subject.affixes)) {
    | Some((Tile(_) as tile, affixes)) =>
      let affixes = Segment.Frame.cons(Direction.toggle(d), tile, affixes);
      let subject = {...subject, affixes};
      Some({
        ...edit_state,
        zipper: {
          ...zipper,
          subject,
        },
      });
    | _ => None
    };
  };
};

let select = (d: Direction.t, edit_state: EditState.t): option(EditState.t) => {
  open OptUtil.Syntax;
  let Zipper.{subject, frame} = edit_state.zipper;
  let+ zipper =
    if (d == subject.focus) {
      let+ (piece, affixes, frame) = split_piece(d, subject.affixes, frame);
      let selection =
        subject.selection
        |> Segment.cons_snoc(subject.focus, piece)
        |> Parser.assemble_segment;
      let subject = {...subject, affixes, selection};
      Zipper.{subject, frame};
    } else {
      let+ (piece, selection) = Segment.split(d, subject.selection);
      let affixes = Segment.Frame.cons(subject.focus, piece, subject.affixes);
      let (affixes, frame) = Parser.assemble_zipper(affixes, frame);
      let subject = {...subject, affixes, selection};
      Zipper.{subject, frame};
    };
  {...edit_state, zipper};
};

let remove = (edit_state: EditState.t): EditState.t => {
  let EditState.{backpack, zipper, id_gen: _} = edit_state;
  let Zipper.Subject.{focus, selection, affixes} = zipper.subject;
  let backpack = {
    let (backpack, picked_up) =
      selection
      |> Segment.fold_left_map(
           (backpack, piece) =>
             switch (piece) {
             | Grout(_)
             | Tile(_) => (backpack, None)
             | Shard(shard) =>
               Segment.Frame.contains_matching(shard, affixes)
                 ? (backpack, Some(piece))
                 : (
                   List.map(Segment.remove_matching(shard), backpack),
                   None,
                 )
             },
           backpack,
         );
    let picked_up = Segment.of_pieces(List.filter_map(Fun.id, picked_up));
    Backpack.extend(Direction.toggle(focus), picked_up, backpack);
  };
  let subject = {...zipper.subject, selection: Segment.empty};
  let zipper = {...zipper, subject};
  {...edit_state, backpack, zipper};
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
//       Parser.assemble_segment(Left, prefix),
//       Parser.assemble_segment(Right, suffix),
//     );
//     let (affixes, frame) = Parser.assemble_zipper(affixes, frame);
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
