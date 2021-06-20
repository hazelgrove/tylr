open Util;
open OptUtil.Syntax;

let disassemble_selem = (d: Direction.t, selem: Selem.t): Selection.t => {
  let disassembled =
    switch (selem) {
    | Selem.Shard(_) => []
    | Tile(Pat(tile)) =>
      switch (tile) {
      | OpHole
      | Var(_)
      | BinHole
      | Prod => []
      | Paren(body) =>
        [Selem.Shard(Pat(Paren_l)), ...Selection.of_tiles_pat(body)]
        @ [Shard(Pat(Paren_r))]
      }
    | Tile(Exp(tile)) =>
      switch (tile) {
      | OpHole
      | Num(_)
      | Var(_)
      | BinHole
      | Plus
      | Times
      | Prod => []
      | Paren(body) =>
        [Selem.Shard(Exp(Paren_l)), ...Selection.of_tiles_exp(body)]
        @ [Shard(Exp(Paren_r))]
      | Lam(p) =>
        [Selem.Shard(Exp(Lam_lam)), ...Selection.of_tiles_pat(p)]
        @ [Shard(Exp(Lam_dot))]
      | Let(p, def) =>
        [Selem.Shard(Exp(Let_let)), ...Selection.of_tiles_pat(p)]
        @ [Selem.Shard(Exp(Let_eq)), ...Selection.of_tiles_exp(def)]
        @ [Selem.Shard(Exp(Let_in))]
      }
    };
  switch (d) {
  | Left => List.rev(disassembled)
  | Right => disassembled
  };
};

let rec find_token =
        (d: Direction.t, selection: Selection.t)
        : option((Tiles.t, Shard.t, Selection.t)) =>
  switch (selection) {
  | [] => None
  | [Shard(found_token), ...selection] => Some(([], found_token, selection))
  | [Tile(tile), ...selection] =>
    let+ (preceding_tiles, found_token, selection) =
      find_token(d, selection);
    ([tile, ...preceding_tiles], found_token, selection);
  };

let rec find_rest_of_tile =
        (d: Direction.t, curr_token: Shard.t, selection: Selection.t)
        : option((AltList.even(Tiles.t, Shard.t), Selection.t)) =>
  if (Shard.is_end(d, curr_token)) {
    Some(([], selection));
  } else {
    let* (preceding_tiles, found_token, selection) =
      find_token(d, selection);
    if (Shard.is_next(d, curr_token, found_token)) {
      let+ (rest_of_tile, selection) =
        find_rest_of_tile(d, found_token, selection);
      ([(preceding_tiles, found_token), ...rest_of_tile], selection);
    } else {
      None;
    };
  };

let assemble_tile =
    (d: Direction.t, ts: AltList.t(Shard.t, Tiles.t)): option(Tile.t) => {
  let child = ts => d == Left ? List.rev(ts) : ts;
  switch (d, ts) {
  | (Left, (Pat(Paren_r), [(body, Pat(Paren_l))]))
  | (Right, (Pat(Paren_l), [(body, Pat(Paren_r))])) =>
    let+ body = Tiles.get_pat(child(body));
    Tile.Pat(Paren(body));
  | (Left, (Exp(Paren_r), [(body, Exp(Paren_l))]))
  | (Right, (Exp(Paren_l), [(body, Exp(Paren_r))])) =>
    let+ body = Tiles.get_exp(child(body));
    Tile.Exp(Paren(body));
  | (Left, (Exp(Lam_dot), [(p, Exp(Lam_lam))]))
  | (Right, (Exp(Lam_lam), [(p, Exp(Lam_dot))])) =>
    let+ p = Tiles.get_pat(child(p));
    Tile.Exp(Lam(p));
  | (Left, (Exp(Let_in), [(def, Exp(Let_eq)), (p, Exp(Let_let))]))
  | (Right, (Exp(Let_let), [(p, Exp(Let_eq)), (def, Exp(Let_in))])) =>
    let+ p = Tiles.get_pat(child(p))
    and+ def = Tiles.get_exp(child(def));
    Tile.Exp(Let(p, def));
  | _ => None
  };
};

let flatten = l =>
  l
  |> AltList.even_to_list(Selection.of_tiles, t => [Selem.Shard(t)])
  |> List.flatten;

let rec parse_selection =
        (d: Direction.t, selection: Selection.t): Selection.t => {
  switch (find_token(d, selection)) {
  | None => selection
  | Some((tiles, shard, selection)) =>
    switch (find_rest_of_tile(d, shard, selection)) {
    | None => flatten([(tiles, shard)]) @ parse_selection(d, selection)
    | Some((rest_of_tile, selection)) =>
      let parsed =
        switch (assemble_tile(d, (shard, rest_of_tile))) {
        | None => flatten([(tiles, shard), ...rest_of_tile])
        | Some(tile) => Selection.of_tiles(tiles) @ [Tile(tile)]
        };
      parsed @ parse_selection(d, selection);
    }
  };
};

let disassemble_frame: Frame.t => option((Selection.frame, Frame.t)) =
  fun
  | Pat(frame) =>
    switch (frame) {
    | Paren_body(((prefix, suffix), frame)) =>
      let prefix = [
        Selem.Shard(Pat(Paren_l)),
        ...Selection.of_tiles_pat(prefix),
      ];
      let suffix = [
        Selem.Shard(Pat(Paren_r)),
        ...Selection.of_tiles_pat(suffix),
      ];
      Some(((prefix, suffix), Pat(frame)));
    | Lam_pat(((prefix, suffix), frame)) =>
      let prefix = [
        Selem.Shard(Exp(Lam_lam)),
        ...Selection.of_tiles_exp(prefix),
      ];
      let suffix = [
        Selem.Shard(Exp(Lam_dot)),
        ...Selection.of_tiles_exp(suffix),
      ];
      Some(((prefix, suffix), Exp(frame)));
    | Let_pat(def, ((prefix, suffix), frame)) =>
      let prefix = [
        Selem.Shard(Exp(Let_let)),
        ...Selection.of_tiles_exp(prefix),
      ];
      let suffix =
        [Selem.Shard(Exp(Let_eq)), ...Selection.of_tiles_exp(def)]
        @ [Shard(Exp(Let_in)), ...Selection.of_tiles_exp(suffix)];
      Some(((prefix, suffix), Exp(frame)));
    }
  | Exp(frame) =>
    switch (frame) {
    | Root => None
    | Paren_body(((prefix, suffix), frame)) =>
      let prefix = [
        Selem.Shard(Exp(Paren_l)),
        ...Selection.of_tiles_exp(prefix),
      ];
      let suffix = [
        Selem.Shard(Exp(Paren_r)),
        ...Selection.of_tiles_exp(suffix),
      ];
      Some(((prefix, suffix), Exp(frame)));
    | Let_def(p, ((prefix, suffix), frame)) =>
      let prefix =
        [Selem.Shard(Exp(Let_eq)), ...List.rev(Selection.of_tiles_pat(p))]
        @ [Shard(Exp(Let_let)), ...Selection.of_tiles_exp(prefix)];
      let suffix = [
        Selem.Shard(Exp(Let_in)),
        ...Selection.of_tiles_exp(suffix),
      ];
      Some(((prefix, suffix), Exp(frame)));
    };

let assemble_frame =
    (
      frame_t: (AltList.t(Shard.t, Tiles.t) as 't, 't),
      (prefix, suffix): Selection.frame,
      frame: Frame.t,
    )
    : option(Frame.t) => {
  let* prefix = Selection.get_tiles(prefix);
  let* suffix = Selection.get_tiles(suffix);
  switch (frame_t, frame) {
  | (((Pat(Paren_l), []), (Pat(Paren_r), [])), Pat(frame)) =>
    let+ prefix = Tiles.get_pat(prefix)
    and+ suffix = Tiles.get_pat(suffix);
    Frame.Pat(Paren_body(((prefix, suffix), frame)));
  | (((Exp(Lam_lam), []), (Exp(Lam_dot), [])), Exp(frame)) =>
    let+ prefix = Tiles.get_exp(prefix)
    and+ suffix = Tiles.get_exp(suffix);
    Frame.Pat(Lam_pat(((prefix, suffix), frame)));
  | (
      ((Exp(Let_let), []), (Exp(Let_eq), [(def, Exp(Let_in))])),
      Exp(frame),
    ) =>
    let+ def = Tiles.get_exp(def)
    and+ prefix = Tiles.get_exp(prefix)
    and+ suffix = Tiles.get_exp(suffix);
    Frame.Pat(Let_pat(def, ((prefix, suffix), frame)));
  | (((Exp(Paren_l), []), (Exp(Paren_r), [])), Exp(frame)) =>
    let+ prefix = Tiles.get_exp(prefix)
    and+ suffix = Tiles.get_exp(suffix);
    Frame.Exp(Paren_body(((prefix, suffix), frame)));
  | (
      ((Exp(Let_eq), [(p, Exp(Let_let))]), (Exp(Let_in), [])),
      Exp(frame),
    ) =>
    let+ p = Tiles.get_pat(List.rev(p))
    and+ prefix = Tiles.get_exp(prefix)
    and+ suffix = Tiles.get_exp(suffix);
    Frame.Exp(Let_def(p, ((prefix, suffix), frame)));
  | _ => None
  };
};

let rec parse_zipper =
        ((prefix, suffix) as sframe: Selection.frame, frame: Frame.t)
        : (Selection.frame, Frame.t) => {
  switch (find_token(Left, prefix), find_token(Right, suffix)) {
  | (None, _)
  | (_, None) => (sframe, frame)
  | (
      Some((tiles_pre, token_pre, prefix)),
      Some((tiles_suf, token_suf, suffix)),
    ) =>
    switch (
      find_rest_of_tile(Left, token_pre, prefix),
      find_rest_of_tile(Right, token_suf, suffix),
    ) {
    | (None, _)
    | (_, None) =>
      let (sframe', frame') = parse_zipper((prefix, suffix), frame);
      let sframe' =
        ListFrame.append(
          (
            flatten([(tiles_pre, token_pre)]),
            flatten([(tiles_suf, token_suf)]),
          ),
          sframe',
        );
      (sframe', frame');
    | (Some((rest_of_tile_pre, prefix)), Some((rest_of_tile_suf, suffix))) =>
      let ((prefix', suffix'), frame') =
        parse_zipper((prefix, suffix), frame);
      let tile_pre = (token_pre, rest_of_tile_pre);
      let tile_suf = (token_suf, rest_of_tile_suf);
      switch (
        assemble_frame((tile_pre, tile_suf), (prefix', suffix'), frame')
      ) {
      | None =>
        let prefix'' =
          flatten([(tiles_pre, token_pre), ...rest_of_tile_pre]);
        let suffix'' =
          flatten([(tiles_suf, token_suf), ...rest_of_tile_suf]);
        ((prefix'' @ prefix', suffix'' @ suffix'), frame');
      | Some(frame'') =>
        let prefix'' = Selection.of_tiles(tiles_pre);
        let suffix'' = Selection.of_tiles(tiles_suf);
        ((prefix'', suffix''), frame'');
      };
    }
  };
};

let fix_holes =
    (ltip: Tip.t, (prefix, suffix): Selection.frame, rtip: Tip.t)
    : Selection.frame => {
  let rec fix = (ltip: Tip.t, selection: Selection.t): (Selection.t, Tip.t) =>
    switch (selection) {
    | [] => ([], Tip.toggle(ltip))
    | [selem, ...selection] =>
      if (Selem.is_hole(selem)) {
        // skip holes
        fix(ltip, selection);
      } else {
        let (fixed, rtip) =
          fix(Tip.toggle(Selem.tip(Right, selem)), selection);
        let inserted_hole =
          // relies on invariant that reachable selections are sort-consistent
          Selem.tip(Left, selem) == ltip
            ? [] : [Selem.Tile(Tile.mk_hole(ltip))];
        (inserted_hole @ [selem, ...fixed], rtip);
      }
    };
  let (fixed_prefix, rtip_prefix) = fix(ltip, List.rev(prefix));
  let (fixed_suffix, rtip_suffix) = fix(Tip.toggle(rtip_prefix), suffix);
  let inserted_hole =
    rtip_suffix == rtip ? [] : [Selem.Tile(Tile.mk_hole(rtip))];
  (List.rev(fixed_prefix), fixed_suffix @ inserted_hole);
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
        Tile.precedence(tile) <= Tile.precedence(post)
          ? process_postop(
              ~output_stack=push_output(itile, output_stack),
              ~shunted_stack=itiles,
              ipostop,
            )
          : (output_stack, [ipostop, ...shunted_stack])
      }
    };

  // currently assumes all binops are left-associative
  // and binops lose ties with preops
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
        Tile.precedence(tile) <= Tile.precedence(bin)
          ? process_binop(
              ~output_stack=push_output(itile, output_stack),
              ~shunted_stack=itiles,
              ibinop,
            )
          : (output_stack, [ibinop, ...shunted_stack])
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
