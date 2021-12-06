open Util;
open OptUtil.Syntax;

let disassemble_piece = (d: Direction.t, piece: Piece.t): Segment.t => {
  let disassembled: Segment.t =
    switch (piece) {
    | Piece.Shard(Pat(_)) => []
    | Shard(Exp((id, shard))) =>
      switch (shard) {
      | Lam_lam_open(p) =>
        [Piece.Shard(Exp((id, Lam_lam))), ...Segment.of_tiles_pat(p)]
        @ [Shard(Exp((id, Lam_open)))]
      | Let_let_eq(p) =>
        [Piece.Shard(Exp((id, Let_let))), ...Segment.of_tiles_pat(p)]
        @ [Shard(Exp((id, Let_eq)))]
      | _ => []
      }
    | Tile(Pat((id, tile))) =>
      switch (tile) {
      | OpHole
      | Var(_)
      | BinHole
      | Prod => []
      | Paren(body) =>
        [Piece.Shard(Pat((id, Paren_l))), ...Segment.of_tiles_pat(body)]
        @ [Shard(Pat((id, Paren_r)))]
      }
    | Tile(Exp((id, tile))) =>
      switch (tile) {
      | OpHole
      | Num(_)
      | Var(_)
      | Fact
      | BinHole
      | Plus
      | Minus
      | Times
      | Div
      | Prod => []
      | Paren(body) =>
        [Piece.Shard(Exp((id, Paren_l))), ...Segment.of_tiles_exp(body)]
        @ [Shard(Exp((id, Paren_r)))]
      | Ap(arg) =>
        [Piece.Shard(Exp((id, Ap_l))), ...Segment.of_tiles_exp(arg)]
        @ [Shard(Exp((id, Ap_r)))]
      | Lam(p, body) =>
        [
          Piece.Shard(Exp((id, Lam_lam_open(p)))),
          ...Segment.of_tiles_exp(body),
        ]
        @ [Shard(Exp((id, Lam_close)))]
      | Let(p, def) =>
        [
          Piece.Shard(Exp((id, Let_let_eq(p)))),
          ...Segment.of_tiles_exp(def),
        ]
        @ [Shard(Exp((id, Let_in)))]
      | Cond(then_) =>
        [Piece.Shard(Exp((id, Cond_que))), ...Segment.of_tiles_exp(then_)]
        @ [Shard(Exp((id, Cond_col)))]
      }
    };
  switch (d) {
  | Left => List.rev(disassembled)
  | Right => disassembled
  };
};

let split_by_matching_shards =
    (shard: Shard.t, segment: Segment.t)
    : (AltList.t(Shard.t, Segment.t), Segment.t) => {
  let id = Shard.id(shard);
  let (tl, rest) =
    AltList.split(
      fun
      | Piece.Shard((Pat((id', _)) | Exp((id', _))) as shard) when id == id' =>
        Some(shard)
      | _ => None,
      segment,
    );
  ((shard, tl), rest);
};

let flatten = l =>
  l |> AltList.odd_to_list(t => [Piece.Shard(t)], Fun.id) |> List.flatten;

let rec assemble_segment = (d: Direction.t, segment: Segment.t): Segment.t =>
  switch (segment) {
  | [] => []
  | [Tile(_) as tile, ...segment] => [tile, ...assemble_segment(d, segment)]
  | [Shard(shard), ...segment] =>
    let (split, rest) = split_by_matching_shards(shard, segment);
    let prefix =
      switch (assemble_piece(d, split)) {
      | None => flatten(split)
      | Some(piece) => [piece]
      };
    prefix @ assemble_segment(d, rest);
  }
// assumes input shards have matching ids
and assemble_piece =
    (d: Direction.t, ts: AltList.t(Shard.t, Segment.t)): option(Piece.t) => {
  let child = ts => d == Left ? List.rev(ts) : ts;
  switch (d, ts) {
  | (Left, (Pat((id, Paren_r)), [(body, Pat((_, Paren_l)))]))
  | (Right, (Pat((id, Paren_l)), [(body, Pat((_, Paren_r)))])) =>
    let+ body = Segment.get_tiles_pat(child(body));
    Piece.Tile(Pat((id, Paren(body))));
  | (Left, (Exp((id, Paren_r)), [(body, Exp((_, Paren_l)))]))
  | (Right, (Exp((id, Paren_l)), [(body, Exp((_, Paren_r)))])) =>
    let+ body = Segment.get_tiles_exp(child(body));
    Piece.Tile(Exp((id, Paren(body))));
  | (Left, (Exp((id, Ap_r)), [(arg, Exp((_, Ap_l)))]))
  | (Right, (Exp((id, Ap_l)), [(arg, Exp((_, Ap_r)))])) =>
    let+ arg = Segment.get_tiles_exp(child(arg));
    Piece.Tile(Exp((id, Ap(arg))));
  | (Left, (Exp((id, Lam_open)), [(p, Exp((_, Lam_lam)))]))
  | (Right, (Exp((id, Lam_lam)), [(p, Exp((_, Lam_open)))])) =>
    let+ p = Segment.get_tiles_pat(child(p));
    Piece.Shard(Exp((id, Lam_lam_open(p))));
  | (
      Left,
      (
        Exp((id, Lam_close)),
        [(body, Exp((_, Lam_open))), (p, Exp((_, Lam_lam)))],
      ),
    )
  | (
      Right,
      (
        Exp((id, Lam_lam)),
        [(p, Exp((_, Lam_open))), (body, Exp((_, Lam_close)))],
      ),
    ) =>
    let+ p = Segment.get_tiles_pat(p)
    and+ body = Segment.get_tiles_exp(child(body));
    Piece.Tile(Exp((id, Lam(p, body))));
  | (Left, (Exp((id, Lam_close)), [(body, Exp((_, Lam_lam_open(p))))]))
  | (Right, (Exp((id, Lam_lam_open(p))), [(body, Exp((_, Lam_close)))])) =>
    let+ body = Segment.get_tiles_exp(child(body));
    Piece.Tile(Exp((id, Lam(p, body))));
  | (Left, (Exp((id, Let_eq)), [(p, Exp((_, Let_let)))]))
  | (Right, (Exp((id, Let_let)), [(p, Exp((_, Let_eq)))])) =>
    let+ p = Segment.get_tiles_pat(child(p));
    Piece.Shard(Exp((id, Let_let_eq(p))));
  | (Left, (Exp((id, Let_in)), [(def, Exp((_, Let_let_eq(p))))]))
  | (Right, (Exp((id, Let_let_eq(p))), [(def, Exp((_, Let_in)))])) =>
    let+ def = Segment.get_tiles_exp(child(def));
    Piece.Tile(Exp((id, Let(p, def))));
  | (Left, (Exp((id, Cond_col)), [(then_, Exp((_, Cond_que)))]))
  | (Right, (Exp((id, Cond_que)), [(then_, Exp((_, Cond_col)))])) =>
    let+ then_ = Segment.get_tiles_exp(child(then_));
    Piece.Tile(Exp((id, Cond(then_))));
  | _ => None
  };
};

let disassemble_frame: Frame.t => option((Segment.frame, Frame.t)) =
  fun
  | Pat((id, frame)) =>
    switch (frame) {
    | Paren_body(((prefix, suffix), frame)) =>
      let prefix = [
        Piece.Shard(Pat((id, Paren_l))),
        ...Segment.of_tiles_pat(prefix),
      ];
      let suffix = [
        Piece.Shard(Pat((id, Paren_r))),
        ...Segment.of_tiles_pat(suffix),
      ];
      Some(((prefix, suffix), Pat(frame)));
    | Lam_pat(body, ((prefix, suffix), frame)) =>
      let prefix = [
        Piece.Shard(Exp((id, Lam_lam))),
        ...Segment.of_tiles_exp(prefix),
      ];
      let suffix =
        [Piece.Shard(Exp((id, Lam_open))), ...Segment.of_tiles_exp(body)]
        @ [
          Piece.Shard(Exp((id, Lam_close))),
          ...Segment.of_tiles_exp(suffix),
        ];
      Some(((prefix, suffix), Exp(frame)));
    | Let_pat(def, ((prefix, suffix), frame)) =>
      let prefix = [
        Piece.Shard(Exp((id, Let_let))),
        ...Segment.of_tiles_exp(prefix),
      ];
      let suffix =
        [Piece.Shard(Exp((id, Let_eq))), ...Segment.of_tiles_exp(def)]
        @ [Shard(Exp((id, Let_in))), ...Segment.of_tiles_exp(suffix)];
      Some(((prefix, suffix), Exp(frame)));
    }
  | Exp((id, frame)) =>
    switch (frame) {
    | Root => None
    | Paren_body(((prefix, suffix), frame)) =>
      let prefix = [
        Piece.Shard(Exp((id, Paren_l))),
        ...Segment.of_tiles_exp(prefix),
      ];
      let suffix = [
        Piece.Shard(Exp((id, Paren_r))),
        ...Segment.of_tiles_exp(suffix),
      ];
      Some(((prefix, suffix), Exp(frame)));
    | Ap_arg(((prefix, suffix), frame)) =>
      let prefix = [
        Piece.Shard(Exp((id, Ap_l))),
        ...Segment.of_tiles_exp(prefix),
      ];
      let suffix = [
        Piece.Shard(Exp((id, Ap_r))),
        ...Segment.of_tiles_exp(suffix),
      ];
      Some(((prefix, suffix), Exp(frame)));
    | Lam_body(p, ((prefix, suffix), frame)) =>
      let prefix = [
        Piece.Shard(Exp((id, Lam_lam_open(p)))),
        ...Segment.of_tiles_exp(prefix),
      ];
      let suffix = [
        Piece.Shard(Exp((id, Lam_close))),
        ...Segment.of_tiles_exp(suffix),
      ];
      Some(((prefix, suffix), Exp(frame)));
    | Let_def(p, ((prefix, suffix), frame)) =>
      let prefix = [
        Piece.Shard(Exp((id, Let_let_eq(p)))),
        ...Segment.of_tiles_exp(prefix),
      ];
      let suffix = [
        Piece.Shard(Exp((id, Let_in))),
        ...Segment.of_tiles_exp(suffix),
      ];
      Some(((prefix, suffix), Exp(frame)));
    | Cond_then(((prefix, suffix), frame)) =>
      let prefix = [
        Piece.Shard(Exp((id, Cond_que))),
        ...Segment.of_tiles_exp(prefix),
      ];
      let suffix = [
        Piece.Shard(Exp((id, Cond_col))),
        ...Segment.of_tiles_exp(suffix),
      ];
      Some(((prefix, suffix), Exp(frame)));
    };

// assumes shards have matching ids
let assemble_frame =
    (
      frame_t: (AltList.t(Shard.t, Tiles.t) as 't, 't),
      (prefix, suffix): Segment.frame,
      frame: Frame.t,
    )
    : option(Frame.t) => {
  let* prefix = Segment.get_tiles(prefix);
  let* suffix = Segment.get_tiles(suffix);
  switch (frame_t, frame) {
  | (((Pat((id, Paren_l)), []), (Pat((_, Paren_r)), [])), Pat(frame)) =>
    let+ prefix = Tiles.get_pat(prefix)
    and+ suffix = Tiles.get_pat(suffix);
    Frame.Pat((id, Paren_body(((prefix, suffix), frame))));
  | (
      (
        (Exp((id, Lam_lam)), []),
        (Exp((_, Lam_open)), [(body, Exp((_, Lam_close)))]),
      ),
      Exp(frame),
    ) =>
    let+ body = Tiles.get_exp(body)
    and+ prefix = Tiles.get_exp(prefix)
    and+ suffix = Tiles.get_exp(suffix);
    Frame.Pat((id, Lam_pat(body, ((prefix, suffix), frame))));
  | (
      (
        (Exp((id, Let_let)), []),
        (Exp((_, Let_eq)), [(def, Exp((_, Let_in)))]),
      ),
      Exp(frame),
    ) =>
    let+ def = Tiles.get_exp(def)
    and+ prefix = Tiles.get_exp(prefix)
    and+ suffix = Tiles.get_exp(suffix);
    Frame.Pat((id, Let_pat(def, ((prefix, suffix), frame))));
  | (((Exp((id, Paren_l)), []), (Exp((_, Paren_r)), [])), Exp(frame)) =>
    let+ prefix = Tiles.get_exp(prefix)
    and+ suffix = Tiles.get_exp(suffix);
    Frame.Exp((id, Paren_body(((prefix, suffix), frame))));
  | (((Exp((id, Ap_l)), []), (Exp((_, Ap_r)), [])), Exp(frame)) =>
    let+ prefix = Tiles.get_exp(prefix)
    and+ suffix = Tiles.get_exp(suffix);
    Frame.Exp((id, Ap_arg(((prefix, suffix), frame))));
  | (
      ((Exp((id, Lam_lam_open(p))), []), (Exp((_, Lam_close)), [])),
      Exp(frame),
    ) =>
    let+ prefix = Tiles.get_exp(prefix)
    and+ suffix = Tiles.get_exp(suffix);
    Frame.Exp((id, Lam_body(p, ((prefix, suffix), frame))));
  | (
      ((Exp((id, Let_let_eq(p))), []), (Exp((_, Let_in)), [])),
      Exp(frame),
    ) =>
    let+ prefix = Tiles.get_exp(prefix)
    and+ suffix = Tiles.get_exp(suffix);
    Frame.Exp((id, Let_def(p, ((prefix, suffix), frame))));
  | (((Exp((id, Cond_que)), []), (Exp((_, Cond_col)), [])), Exp(frame)) =>
    let+ prefix = Tiles.get_exp(prefix)
    and+ suffix = Tiles.get_exp(suffix);
    Frame.Exp((id, Cond_then(((prefix, suffix), frame))));
  | _ => None
  };
};

// assumes shards have matching ids
let assemble_frame =
    (
      disassembled: (AltList.t(Shard.t, Segment.t) as 't, 't),
      (prefix, suffix): Segment.frame,
      frame: Frame.t,
    )
    : option(Frame.t) => {
  let* prefix = Segment.get_tiles(prefix);
  let* suffix = Segment.get_tiles(suffix);
  switch (disassembled, frame) {
  | (((Pat((id, Paren_l)), []), (Pat((_, Paren_r)), [])), Pat(frame)) =>
    let+ prefix = Tiles.get_pat(prefix)
    and+ suffix = Tiles.get_pat(suffix);
    Frame.Pat((id, Paren_body(((prefix, suffix), frame))));
  | (
      (
        (Exp((id, Lam_lam)), []),
        (Exp((_, Lam_open)), [(body, Exp((_, Lam_close)))]),
      ),
      Exp(frame),
    ) =>
    let+ body = Segment.get_tiles_exp(body)
    and+ prefix = Tiles.get_exp(prefix)
    and+ suffix = Tiles.get_exp(suffix);
    Frame.Pat((id, Lam_pat(body, ((prefix, suffix), frame))));
  | (
      (
        (Exp((id, Let_let)), []),
        (Exp((_, Let_eq)), [(def, Exp((_, Let_in)))]),
      ),
      Exp(frame),
    ) =>
    let+ def = Segment.get_tiles_exp(def)
    and+ prefix = Tiles.get_exp(prefix)
    and+ suffix = Tiles.get_exp(suffix);
    Frame.Pat((id, Let_pat(def, ((prefix, suffix), frame))));
  | (((Exp((id, Paren_l)), []), (Exp((_, Paren_r)), [])), Exp(frame)) =>
    let+ prefix = Tiles.get_exp(prefix)
    and+ suffix = Tiles.get_exp(suffix);
    Frame.Exp((id, Paren_body(((prefix, suffix), frame))));
  | (((Exp((id, Ap_l)), []), (Exp((_, Ap_r)), [])), Exp(frame)) =>
    let+ prefix = Tiles.get_exp(prefix)
    and+ suffix = Tiles.get_exp(suffix);
    Frame.Exp((id, Ap_arg(((prefix, suffix), frame))));
  | (
      ((Exp((id, Lam_lam_open(p))), []), (Exp((_, Lam_close)), [])),
      Exp(frame),
    ) =>
    let+ prefix = Tiles.get_exp(prefix)
    and+ suffix = Tiles.get_exp(suffix);
    Frame.Exp((id, Lam_body(p, ((prefix, suffix), frame))));
  | (
      ((Exp((id, Let_let_eq(p))), []), (Exp((_, Let_in)), [])),
      Exp(frame),
    ) =>
    let+ prefix = Tiles.get_exp(prefix)
    and+ suffix = Tiles.get_exp(suffix);
    Frame.Exp((id, Let_def(p, ((prefix, suffix), frame))));
  | (((Exp((id, Cond_que)), []), (Exp((_, Cond_col)), [])), Exp(frame)) =>
    let+ prefix = Tiles.get_exp(prefix)
    and+ suffix = Tiles.get_exp(suffix);
    Frame.Exp((id, Cond_then(((prefix, suffix), frame))));
  | _ => None
  };
};

let rec assemble_zipper =
    (affixes: Segment.frame, frame: Frame.t)
    : (Segment.frame, Frame.t) => {
  switch (affixes) {
  | ([], _)
  | (_, []) => (affixes, frame)
  | ([Tile(_) as tile, ...prefix], suffix) =>
    let ((prefix, suffix), frame) =
      assemble_zipper((prefix, suffix), frame);
    (([tile, ...prefix], suffix), frame);
  | (prefix, [Tile(_) as tile, ...suffix]) =>
    let ((prefix, suffix), frame) =
      assemble_zipper((prefix, suffix), frame);
    ((prefix, [tile, ...suffix]), frame);
  | ([Shard(shard_pre), ...prefix], [Shard(shard_suf), ...suffix]) =>
    let (disassembled_pre, prefix) = split_by_matching_shards(shard_pre, prefix);
    let (disassembled_suf, suffix) = split_by_matching_shards(shard_suf, suffix);
    let ((prefix, suffix), frame) = assemble_zipper((prefix, suffix), frame);
    switch (
      assemble_frame((disassembled_pre, disassembled_suf), (prefix, suffix), frame)
    ) {
    | None =>
      let prefix = flatten(disassembled_pre) @ prefix;
      let suffix = flatten(disassembled_suf) @ suffix;
      ((prefix, suffix), frame);
    | Some(frame) => (([], []), frame)
    }
  }
};

// todo: clean up
let connect = (id_gen, sort, selection: Segment.t, (prefix, suffix) as affixes: Segment.frame): ((Segment.t, Segment.frame), IdGen.t) => {
  let expected_tip =
    fun
    | None => (Tip.Convex, sort)
    | Some(affix_tip) => Tip.toggle(affix_tip);
  // left bc prefix is reversed
  let l = expected_tip(Segment.tip(Left, prefix))
  let r = expected_tip(Segment.tip(Left, suffix));

  switch (Segment.tip(Left, selection), Segment.tip(Right, selection)) {
  | (None, _)
  | (_, None) =>
    if (l == Tip.toggle(r)) {
      (([], affixes), id_gen)
    } else {
      let (hole, id_gen) = Tile.mk_hole(id_gen, l);
      (([], (prefix, [Tile(hole), ...suffix])), id_gen);
    };
  | (Some(left_tip), Some(right_tip)) =>
    let (prefix, id_gen) =
      if (l == left_tip) { (prefix, id_gen) } else {
        let (hole, id_gen) = Tile.mk_hole(id_gen, l);
        ([Tile(hole), ...prefix], id_gen)
      };
    let (suffix, id_gen) =
      if (r == right_tip) { (suffix, id_gen) } else {
        let (hole, id_gen) = Tile.mk_hole(id_gen, r);
        ([Tile(hole), ...suffix], id_gen)
      };
    ((selection, (prefix, suffix)), id_gen);
  };
};

type itile = (int, Tile.t);
let associate = (tiles: Tiles.t): Skel.t => {
  let push_output =
      ((i, tile): itile, output_stack: list(Skel.t)): list(Skel.t) =>
    switch (Tile.tip(Left, tile), Tile.tip(Right, tile)) {
    | ((Convex, _), (Convex, _)) => [Op(i), ...output_stack]
    | ((Convex, _), (Concave, _)) =>
      switch (output_stack) {
      | [] => failwith("impossible: pre encountered empty stack")
      | [skel, ...skels] => [Pre(i, skel), ...skels]
      }
    | ((Concave, _), (Convex, _)) =>
      switch (output_stack) {
      | [] => failwith("impossible: post encountered empty stack")
      | [skel, ...skels] => [Post(skel, i), ...skels]
      }
    | ((Concave, _), (Concave, _)) =>
      switch (output_stack) {
      | []
      | [_] =>
        failwith("impossible: bin encountered empty or singleton stack")
      | [skel1, skel2, ...skels] => [Bin(skel2, i, skel1), ...skels]
      }
    };

  let process_operand = (~output_stack, ~shunted_stack, op) => (
    output_stack,
    [op, ...shunted_stack],
  );

  let rec process_preop =
          (
            ~output_stack: list(Skel.t),
            ~shunted_stack: list(itile),
            ipreop: itile,
          ) => {
    switch (shunted_stack) {
    | [] => (output_stack, [ipreop, ...shunted_stack])
    | [(_, tile) as itile, ...itiles] =>
      switch (Tile.tip(Right, tile)) {
      | (Concave, _) => (output_stack, [ipreop, ...shunted_stack])
      | (Convex, _) =>
        process_preop(
          ~output_stack=push_output(itile, output_stack),
          ~shunted_stack=itiles,
          ipreop,
        )
      }
    };
  };

  // assumes postops lose ties with preops and binops
  let rec process_postop =
          (
            ~output_stack: list(Skel.t),
            ~shunted_stack: list(itile),
            (_, post) as ipostop: itile,
          ) =>
    switch (shunted_stack) {
    | [] => (output_stack, [ipostop, ...shunted_stack])
    | [(_, tile) as itile, ...itiles] =>
      switch (Tile.tip(Right, tile)) {
      | (Convex, _) =>
        process_postop(
          ~output_stack=push_output(itile, output_stack),
          ~shunted_stack=itiles,
          ipostop,
        )
      | (Concave, _) =>
        let p_post = Tile.precedence(post);
        let p_tile = Tile.precedence(tile);
        let a_tile = IntMap.find_opt(p_tile, Tile.associativity(tile));
        p_tile < p_post
        || p_tile == p_post
        && a_tile == Some(Associativity.Left)
          ? process_postop(
              ~output_stack=push_output(itile, output_stack),
              ~shunted_stack=itiles,
              ipostop,
            )
          : (output_stack, [ipostop, ...shunted_stack]);
      }
    };

  // currently assumes binops lose ties with preops
  let rec process_binop =
          (
            ~output_stack: list(Skel.t),
            ~shunted_stack: list(itile),
            (_, bin) as ibinop: itile,
          ) =>
    switch (shunted_stack) {
    | [] => (output_stack, [ibinop, ...shunted_stack])
    | [(_, tile) as itile, ...itiles] =>
      switch (Tile.tip(Right, tile)) {
      | (Convex, _) =>
        process_binop(
          ~output_stack=push_output(itile, output_stack),
          ~shunted_stack=itiles,
          ibinop,
        )
      | (Concave, _) =>
        let p_bin = Tile.precedence(bin);
        let p_tile = Tile.precedence(tile);
        let a_tile = IntMap.find_opt(p_tile, Tile.associativity(tile));
        p_tile < p_bin
        || p_tile == p_bin
        && a_tile == Some(Associativity.Left)
          ? process_binop(
              ~output_stack=push_output(itile, output_stack),
              ~shunted_stack=itiles,
              ibinop,
            )
          : (output_stack, [ibinop, ...shunted_stack]);
      }
    };

  let rec go =
          (
            ~output_stack: list(Skel.t)=[],
            ~shunted_stack: list(itile)=[],
            itiles: list(itile),
          )
          : list(Skel.t) => {
    switch (itiles) {
    | [] =>
      shunted_stack
      |> List.fold_left(
           (output_stack, t) => push_output(t, output_stack),
           output_stack,
         )
    | [(_, tile) as itile, ...itiles] =>
      let process =
        switch (Tile.tip(Left, tile), Tile.tip(Right, tile)) {
        | ((Convex, _), (Convex, _)) => process_operand
        | ((Convex, _), (Concave, _)) => process_preop
        | ((Concave, _), (Convex, _)) => process_postop
        | ((Concave, _), (Concave, _)) => process_binop
        };
      let (output_stack, shunted_stack) =
        process(~output_stack, ~shunted_stack, itile);
      go(~output_stack, ~shunted_stack, itiles);
    };
  };

  tiles |> List.mapi((i, tile) => (i, tile)) |> go |> List.hd;
};
