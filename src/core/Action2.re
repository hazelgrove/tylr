open Util;

type t =
  | Move(Direction.t)
  | Select(Direction.t)
  | Remove
  | Insert(Direction.t, Tile.t)
  | Pick_up
  | Put_down;

type result('success) = Result.t('success, Failure.t);

let enter_segment = (d: Direction.t, segment: Segment.t): option((Shard.t, Segment.t)) =>
  switch (Segment.orient(d, segment)) {
  | ([], []) => None
  | ([], [(shard, tiles), ...tl]) =>
    Some((shard, Segment.unorient(d, (tiles, tl))))
  | ([tile, ...tiles], tl) =>
    let (tile_hd, tile_tl) = Parser.disassemble_tile(d, tile);
    let segment = Segment.concat([Aba.snoc(tile_tl, []), (tiles, tl)]);
    Some((tile_hd, Segment.unorient(d, segment)));
  };

let enter_zipper =
    (d: Direction.t, (affixes: Segment.Frame.t, frame: Zipper.Frame.t))
    : option((Shard.t, (Segment.Frame.t, Zipper.Frame.t))) => {
  let (front, back) = Segment.Frame.orient(d, affixes);
  // TODO revisit direction arg
  switch (enter_segment(Right, front)) {
  | None =>
    open OptUtil.Syntax;
    let* (affixes', frame) = Parser.disassemble_frame(frame);
    let (front', back') = Segment.Frame.orient(d, affixes);
    switch (front') {
    | ([], [(shard, tiles), ...tl]) =>
      let affixes' = Segment.Frame.unorient(d, ((tiles, tl), back'));
      Some((shard, (Segment.Frame.concat([affixes', affixes]), frame)));
    | ([], [])
    | ([_, ..._], _) => failwith("expected head shard")
    };
  | Some((shard, front)) =>
    let affixes = Segment.Frame.unorient(d, (front, back));
    Some((shard, (affixes, frame)));
  };

// unselect current selection and move caret to
// desired end of former selection if possible
// (not possible if doing so would violate ordering
// between shards up and shards down)
let unselect = (d: Direction.t, ((down, up), frame): Zipper.t): option(Zipper.t) =>
  if (
    d == down.focus
    && !Segment.is_balanced(selection)
    && !Zipper.Subject.Up.is_balanced(up)
  ) {
    None
  } else {
    let affixes =
      switch (d) {
      | Left => (prefix, Segment.concat([selection, suffix]))
      | Right => (Segment.(concat([rev(selection), prefix])), suffix)
      };
    let (affixes, frame) = Parser.parse_zipper(affixes, frame);
    Some(((([], affixes), up), frame));
  };

let move = (d: Direction.t, ((down, up), frame): Zipper.t): option(Zipper.t) =>
  switch (down.selection) {
  | (_, [_, ..._])
  | ([_, ..._], _) => unselect(d, zipper)
  | ([], []) =>
    if (Zipper.Subject.Up.is_balanced(up)) {
      open OptUtil.Syntax;
      let+ (shard, (affixes, frame)) = enter_zipper(d, down.affixes, frame);
      let affixes = Segment.Frame.grow(Direction.toggle(d), affixes);
      let (affixes, frame) = Parser.assemble_zipper(affixes, frame);
      let down = {...down, affixes};
      ((down, up), frame);
    } else {
      let (front, back) = Segment.Frame.orient(d, down.affixes);
      switch (front) {
      | ([tile, ...tiles], tl) =>
        let front = (tiles, tl);
        let back = Segment.cons_tile(tile, back);
        let affixes = Segment.Frame.unorient(d, (front, back))
        Some((affixes, frame));
      | _ => None
      };
    }
  };

let select = (d: Direction.t, ((down, up), frame): Zipper.t): option(Zipper.t) =>
  OptUtil.Syntax.(
    if (d == down.focus) {
      let+ (shard, (affixes, frame)) = enter_zipper(d, (down.affixes, frame));
      let selection =
        Parser.assemble_segment(Right, Segment.grow(down.focus, shard, down.selection));
      let down = {...down, affixes, selection};
      ((down, up), frame);
    } else {
      let+ (shard, selection) = enter_segment(d, down.selection);
      let affixes = Segment.Frame.grow(down.focus, shard, down.affixes);
      let (affixes, frame) = Parser.assemble_zipper(affixes, frame);
      let down = {...down, affixes, selection};
      ((down, up), frame);
    }
  );

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
    let+ zipper = Result.of_option(~error=Failure.Cant_move, move(d, zipper));
    (zipper, id_gen);
  | Select(d) =>
    let+ zipper = Result.of_option(~error=Failure.Cant_move, select(d, zipper));
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
