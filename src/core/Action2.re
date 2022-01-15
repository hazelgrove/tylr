open Util;

type t =
  | Move(Direction.t)
  | Select(Direction.t)
  | Remove
  | Insert(Direction.t, Tile.t)
  | Pick_up
  | Put_down;

type result('success) = Result.t('success, Failure.t);

let rec move_balanced =
    (d: Direction.t, affixes: Segment.Frame.t, frame: Zipper.Frame.t)
    : option((Segment.Frame.t, Zipper.Frame.t)) =>
  switch (front_affix(d, affixes)) {
  | ([], []) =>
    // [Move<d>Frame]
    open OptUtil.Syntax;
    let* (affixes', frame') = Parser.disassemble_frame(frame);
    move_balanced(d, ListFrame.append(affixes, affixes'), frame);
  | ([], [(shard, tiles), ...tl]) as front =>
    // [Move<d>Atomic]
    let back =
      Parser.assemble_segment(
        Direction.toggle(d),
        Segment.cons_shard(shard, back_affix(d, affixes)),
      );
    let affixes = mk_affixes(d, front, back);
    Some(Parser.assemble_zipper(affixes, frame));
  | ([tile, ...tiles], tl) =>
    let front = (tiles, tl);
    let disassembled = Parser.disassemble_tile(d, tile);
    let back = Segment.concat([disassembled, back_affix(d, affixes)]);
    move_balanced(d, (front, back), frame);
  };

let move_imbalanced =
    (d: Direction.t, affixes, frame)
    : option((Segment.frame, Frame.t)) =>
  switch (front_affix(d, affixes)) {
  | ([tile, ...tiles], tl) =>
    let front = (tiles, tl);
    let back = Segment.cons_tile(tile, back_affix(d, affixes));
    Some(((front, back), frame));
  | _ => None
  };

// return to empty selection
// move caret to desired end of selection if possible
let unselect = (d: Direction.t, ((down, up), frame)): Zipper.t => {
  let (selection, (prefix, suffix)) = down;
  let (orientation, _) = up;
  let affixes = {
    let d =
      Segment.is_balanced(selection) || Subject2.Up.is_balanced(up)
      ? d : orientation;
    switch (d) {
    | Left =>
      let suffix = Segment.concat([selection, suffix]);
      (prefix, Parser.assemble_segment(Right, suffix));
    | Right =>
      let prefix = Segment.(concat([rev(selection), prefix]));
      (Parser.assemble_segment(Left, prefix), suffix);
    };
  };
  let (affixes, frame) = Parser.parse_zipper(affixes, frame);
  ((([], affixes), up), frame);
};

let move = (d: Direction.t, ((down, up), frame): Zipper.t): result(Zipper.t) => {
  let (selection, affixes) = down;
  switch (selection) {
  | [_, ..._] => Ok(unselect(d, zipper))
  | [] =>
    switch (front_affix(d, affixes)) {
    | [Shard(_), ..._] => Error(Cant_move)
    | _ =>
      let move =
        Subject2.Up.is_balanced(up) ? move_balanced : move_imbalanced;
      Result.of_option(
        ~error=Failure.Cant_move,
        move(d, affixes, frame)
      );
    }
  };
};

let rec select = (d: Direction.t, ((down, up), frame): Zipper.t): result(Zipper.t) => {
  let (selection, affixes) = down;
  let (orientation, segments) = up;
  let front = front_affix(d, affixes);
  let back = back_affix(d, affixes);
  if (Direction.toggle(d) == orientation) {
    switch (front) {
    | [] =>
      let* (affixes', frame') =
        Result.of_option(
          ~error=Failure.Cant_move,
          Parser.disassemble_frame(frame),
        );
      let subj = ((selection, ListFrame.append(affixes, affixes')), up);
      select(d, (subj, frame));
    | [piece, ...front] =>
      switch (Parser.disassemble_piece(d, piece)) {
      | [] =>
        let selection =
          switch (d) {
          | Left => [piece, ...selection]
          | Right => selection @ [piece]
          };
        let down = (
          Parser.assemble_segment(Right, selection),
          mk_affixes(d, front, back),
        );
        let up = (Direction.toggle(d), segments);
        Ok(((down, up), frame));
      | [_, ..._] as disassembled =>
        let affixes = mk_affixes(d, disassembled @ front, back);
        let subj = ((selection, affixes), up);
        select(d, (subj, frame));
      }
    };
  } else {
    let split_selection =
      switch (d) {
      | Left => ListUtil.split_first_opt(selection)
      | Right =>
        ListUtil.split_last_opt(selection)
        |> Option.map(((selection, piece)) => (piece, selection))
      };
    switch (split_selection) {
    | None =>
      let up = (Direction.toggle(d), segments);
      select(d, ((down, up), frame));
    | Some((piece, selection)) =>
      switch (Parser.disassemble_piece(d, piece)) {
      | [] =>
        let front =
          Parser.parse_selection()
      }
    };
  };
};

let remove = (((down, up) as subj, frame): Zipper.t): Zipper.t => {
  // for each piece in selection
  //   check if there are any matching pieces down
  //   if so, pick up that piece
  //   otherwise, remove it and any matching pieces up
  let (d, selection, (prefix, suffix) as affixes) = down;
  let (up, picked_up) =
    selection
    |> ListUtil.fold_left_map(
      (up, piece) => {
        let id = Piece.id(piece);
        let has_matching_piece = List.exists(p => Piece.id(p) == id);
        if (has_matching_piece(prefix) || has_matching_piece(suffix)) {
          (up, Some(piece))
        } else {
          (List.map(Segment.remove_id(id), up), None)
        };
      },
      up,
    );
  let picked_up = List.filter_map(Fun.id, picked_up);
  let down = (d, [], affixes);
  let up = Subject.Up.extend(Direction.toggle(d), picked_up, up);
  ((down, up), frame);
};

let pick_up = (((down, up), frame): Zipper.t): Zipper.t => {
  let sort = Zipper.Frame.sort(frame);
  let (side, selection, affixes) = down;
  let affixes = Segment.connect(affixes);
  let down = ([], affixes);
  switch (Segment.trim(selection)) {
  | [] => ((down, up), frame)
  | [_, ..._] as trimmed =>
    let up = Subject.Up.extend(Direction.toggle(side), trimmed, up);
    ((down, up), frame);
  };
};

let put_down = (((down, up), frame): Zipper.t): result(Zipper.t) => {
  let sort = Frame.sort(frame);
  let (side, selection, affixes) = down;
  switch (selection) {
  | [_, ..._] =>
    // todo revisit
    // should support pasting over intact selections at least
    Error(Undefined)
  | [] =>
    let up_side = Direction.toggle(side);
    // todo better error
    let+ (put_down, up) =
      Result.of_option(~error=Failure.Undefined, Subject.Up.pop(up_side, up));
    let (prefix, suffix) = Segment.connect(~insert=(up_side, put_down), affixes);
    let affixes = (Parser.assemble_segment(Left, prefix), Parser.assemble_segment(Right, suffix));
    let (affixes, frame) = Parser.assemble_zipper(affixes, frame);
    ((([], affixes), up), frame);
  }
};

let insert = (d: Direction.t, tokens: list(Token.t), (zipper: Zipper.t, id_gen: IdGen.t)) => {
  let (((_, selection, affixes), _up), _frame) = zipper;
  let (id, id_gen) = IdGen.next(id_gen);
  let (insertion, picked_up) =
    switch (tokens) {
    | [_] =>
    };

  switch (tokens) {
  | [_] =>
    // make tile with tokens
    // get its default mold
    // make segment and connect between current zipper affixes
    // meanwhile delete all selected tiles and either
    //   (1) remove selected shards if they're the last to be picked up
    //   (also remove their matching shards that were picked up), or
    //   (2) otherwise pick up selected shards
  | _ =>
    // make shards of tokens
    let shards =
      tokens
      |> List.mapi((i, token) => )
    // connect d-end of shard list to current zipper
    // put remaining shards in backpack (in some order tbd)
  }
};

let perform =
    (a: t, (zipper: Zipper.t, id_gen: IdGen.t))
    : (result((Zipper.t, IdGen.t)) as 'r) =>
  switch (a) {
  | Move(d) =>
    let+ zipper = move(d, zipper);
    (zipper, id_gen);
  | Select(d) =>
    let+ zipper = select(d, zipper);
    (zipper, id_gen);
  | Remove =>
    let zipper = remove(zipper);
    Ok((zipper, id_gen));
  | Insert(d, tokens) => insert(d, tokens, (zipper, id_gen))
  | Pick_up =>
    let zipper = pick_up(zipper);
    Ok((zipper, id_gen));
  | Put_down =>
    let+ zipper = put_down(zipper);
    (zipper, id_gen);
  };
