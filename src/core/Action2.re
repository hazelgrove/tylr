type t =
  | Move(Direction.t)
  | Select(Direction.t)
  | Remove
  | Insert(Direction.t, Tile.t)
  | Pick_up
  | Put_down;

type result('success) = Result.t('success, Failure.t);

let rec move_balanced =
    (d: Direction.t, affixes, frame)
    : option((Segment.frame, Frame.t)) =>
  switch (front_affix(d, affixes)) {
  | [] =>
    // [Move<d>Frame]
    open Util.OptUtil.Syntax;
    let* (affixes', frame') = Parser.disassemble_frame(frame);
    move_balanced(d, ListFrame.append(affixes, affixes'), frame);
  | [piece, ...front] =>
    let back = back_affix(d, affixes);
    switch (Parser.disassemble_piece(d, piece)) {
    | [] =>
      // [Move<d>Atomic]
      let front = front_affix(d, affixes);
      let back =
        Parser.parse_selection(
          Direction.toggle(d),
          [piece, ...back_affix(d, affixes)],
        );
      let affixes = mk_affixes(d, front, back);
      Some(Parser.parse_zipper(affixes, frame));
    | [hd, ...tl] =>
      // [Move<d>Disassembles]
      let affixes = mk_affixes(d, tl @ front, back);
      move_balanced(d, hd, affixes, frame);
    }
  };

let move_imbalanced =
    (d: Direction.t, affixes, frame)
    : option((Segment.frame, Frame.t)) =>
  switch (front_affix(d, affixes)) {
  | [Tile(_) as tile, ...front] =>
    let back = back_affix(d, affixes);
    let affixes = mk_affixes(d, front, [tile, ...back]);
    Some((affixes, frame));
  | _ => None
  };

// return to empty selection
// move caret to desired end of selection if possible
let unselect = (d: Direction.t, ((down, up), frame)): Zipper.t => {
  let (selection, affixes) = down;
  let (orientation, _) = up;
  let affixes = {
    let d =
      Segment.is_balanced(selection) || Subject2.Up.is_balanced(up)
      ? d : orientation;
    switch (d) {
    | Left => (prefix, Parser.parse_selection(Right, selection @ suffix))
    | Right =>
      let selection = List.rev(selection);
      (Parser.parse_selection(Left, selection @ prefix));
    };
  };
  let (affixes, frame) = Parser.parse_zipper(affixes, frame);
  let subj = (([], affixes), up);
  (subj, frame);
};

let move = (d: Direction.t, ((down, up), frame): Zipper.t): result(Zipper.t) => {
  let (selection, affixes) = down;
  switch (selection) {
  | [_, ..._] => Ok(unselect(d, zipper))
  | [] =>
    switch (front_affix(d, affixes)) {
    | [Shard(_), ..._] => Error(Cant_move)
    | _ when !Segment.is_restructurable(Subject2.Up.total_segment(up)) =>
      Error(Cant_move)
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
          Parser.parse_selection(selection),
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

let pick_up = (((down, up), frame): Zipper.t, id_gen): (Zipper.t, IdGen.t) => {
  let sort = Frame.sort(frame);
  let (side, selection, affixes) = down;
  let ((_, affixes), id_gen) = Parser.connect(id_gen, sort, [], affixes);
  let down = ([], affixes);
  switch (Segment.trim_end_holes(selection)) {
  | [] => (((down, up), frame), id_gen)
  | [_, ..._] as trimmed =>
    let up = Subject.Up.extend(Direction.toggle(side), trimmed, up);
    (((down, up), frame), id_gen);
  };
};

let put_down = (((down, up), frame): Zipper.t, id_gen): result((Zipper.t, IdGen.t)) => {
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
    let ((put_down, affixes), id_gen) = Parser.connect(id_gen, sort, put_down, affixes);
    let (down, frame) = {
      let (prefix, suffix) = affixes;
      let affixes =
        switch (up_side) {
        | Left =>
          (prefix, Parser.assemble_segment(Right, put_down @ suffix))
        | Right =>
          (Parser.assemble_segment(Left, List.rev(put_down) @ prefix), suffix)
        };
      let (affixes, frame) = Parser.assemble_zipper(affixes, frame);
      (([], affixes), frame);
    };
    ((down, up), frame);
  }
}

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
  | Insert(d, tile) => insert(d, tile, zipper)
  | Pick_up => Ok(pick_up(zipper))
  | Put_down => put_down(zipper)
  };
