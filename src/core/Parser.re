open Util;
open OptUtil.Syntax;

let disassemble_selem = (d: Direction.t, selem: Selem.t): Selection.t => {
  let disassembled: Selection.t =
    switch (selem) {
    | Selem.Shard(Pat(_)) => []
    | Shard(Exp(shard)) =>
      switch (shard) {
      | Let_let_eq(p) =>
        [Selem.Shard(Exp(Let_let)), ...Selection.of_tiles_pat(p)]
        @ [Shard(Exp(Let_eq))]
      | _ => []
      }
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
      | Fact
      | BinHole
      | Plus
      | Minus
      | Times
      | Div
      | Prod => []
      | Paren(body) =>
        [Selem.Shard(Exp(Paren_l)), ...Selection.of_tiles_exp(body)]
        @ [Shard(Exp(Paren_r))]
      | Ap(arg) =>
        [Selem.Shard(Exp(Ap_l)), ...Selection.of_tiles_exp(arg)]
        @ [Shard(Exp(Ap_r))]
      | Lam(p) =>
        [Selem.Shard(Exp(Lam_lam)), ...Selection.of_tiles_pat(p)]
        @ [Shard(Exp(Lam_dot))]
      | Let(p, def) =>
        [Selem.Shard(Exp(Let_let_eq(p))), ...Selection.of_tiles_exp(def)]
        @ [Shard(Exp(Let_in))]
      | Cond(then_) =>
        [Selem.Shard(Exp(Cond_que)), ...Selection.of_tiles_exp(then_)]
        @ [Shard(Exp(Cond_col))]
      }
    };
  switch (d) {
  | Left => List.rev(disassembled)
  | Right => disassembled
  };
};

let rec find_shard =
        (d: Direction.t, selection: Selection.t)
        : option((Tiles.t, Shard.t, Selection.t)) =>
  switch (selection) {
  | [] => None
  | [Shard(found_shard), ...selection] => Some(([], found_shard, selection))
  | [Tile(tile), ...selection] =>
    let+ (preceding_tiles, found_shard, selection) =
      find_shard(d, selection);
    ([tile, ...preceding_tiles], found_shard, selection);
  };

let assemble_selem =
    (d: Direction.t, ts: AltList.t(Shard.t, Tiles.t)): option(Selem.t) => {
  let child = ts => d == Left ? List.rev(ts) : ts;
  switch (d, ts) {
  | (Left, (Pat(Paren_r), [(body, Pat(Paren_l))]))
  | (Right, (Pat(Paren_l), [(body, Pat(Paren_r))])) =>
    let+ body = Tiles.get_pat(child(body));
    Selem.Tile(Pat(Paren(body)));
  | (Left, (Exp(Paren_r), [(body, Exp(Paren_l))]))
  | (Right, (Exp(Paren_l), [(body, Exp(Paren_r))])) =>
    let+ body = Tiles.get_exp(child(body));
    Selem.Tile(Exp(Paren(body)));
  | (Left, (Exp(Ap_r), [(arg, Exp(Ap_l))]))
  | (Right, (Exp(Ap_l), [(arg, Exp(Ap_r))])) =>
    let+ arg = Tiles.get_exp(child(arg));
    Selem.Tile(Exp(Ap(arg)));
  | (Left, (Exp(Lam_dot), [(p, Exp(Lam_lam))]))
  | (Right, (Exp(Lam_lam), [(p, Exp(Lam_dot))])) =>
    let+ p = Tiles.get_pat(child(p));
    Selem.Tile(Exp(Lam(p)));
  | (Left, (Exp(Let_eq), [(p, Exp(Let_let))]))
  | (Right, (Exp(Let_let), [(p, Exp(Let_eq))])) =>
    let+ p = Tiles.get_pat(child(p));
    Selem.Shard(Exp(Let_let_eq(p)));
  | (Left, (Exp(Let_in), [(def, Exp(Let_let_eq(p)))]))
  | (Right, (Exp(Let_let_eq(p)), [(def, Exp(Let_in))])) =>
    let+ def = Tiles.get_exp(child(def));
    Selem.Tile(Exp(Let(p, def)));
  | (Left, (Exp(Cond_col), [(then_, Exp(Cond_que))]))
  | (Right, (Exp(Cond_que), [(then_, Exp(Cond_col))])) =>
    let+ then_ = Tiles.get_exp(child(then_));
    Selem.Tile(Exp(Cond(then_)));
  | _ => None
  };
};

let flatten = l =>
  l
  |> AltList.even_to_list(Selection.of_tiles, t => [Selem.Shard(t)])
  |> List.flatten;

let rec parse_selection =
        (d: Direction.t, selection: Selection.t): Selection.t => {
  switch (find_shard(d, selection)) {
  | None => selection
  | Some((tiles, shard, selection)) =>
    switch (find_rest_of_selem(~strict=false, d, shard, selection)) {
    | None => flatten([(tiles, shard)]) @ parse_selection(d, selection)
    | Some((rest_of_tile, selection)) =>
      switch (assemble_selem(d, (shard, rest_of_tile))) {
      | None =>
        flatten([(tiles, shard), ...rest_of_tile])
        @ parse_selection(d, selection)
      | Some(selem) =>
        Selection.of_tiles(tiles)
        @ parse_selection(d, [selem, ...selection])
      }
    }
  };
}
and find_rest_of_selem =
    (~strict, d: Direction.t, curr_token: Shard.t, selection: Selection.t)
    : option((AltList.even(Tiles.t, Shard.t), Selection.t)) =>
  if (Shard.is_end(~strict, d, curr_token)) {
    Some(([], selection));
  } else {
    let selection = parse_selection(d, selection);
    let* (preceding_tiles, found_shard, selection) =
      find_shard(d, selection);
    if (Shard.is_next(d, curr_token, found_shard)) {
      let+ (rest_of_tile, selection) =
        find_rest_of_selem(~strict, d, found_shard, selection);
      ([(preceding_tiles, found_shard), ...rest_of_tile], selection);
    } else {
      None;
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
    | Ap_arg(((prefix, suffix), frame)) =>
      let prefix = [
        Selem.Shard(Exp(Ap_l)),
        ...Selection.of_tiles_exp(prefix),
      ];
      let suffix = [
        Selem.Shard(Exp(Ap_r)),
        ...Selection.of_tiles_exp(suffix),
      ];
      Some(((prefix, suffix), Exp(frame)));
    | Let_def(p, ((prefix, suffix), frame)) =>
      let prefix = [
        Selem.Shard(Exp(Let_let_eq(p))),
        ...Selection.of_tiles_exp(prefix),
      ];
      let suffix = [
        Selem.Shard(Exp(Let_in)),
        ...Selection.of_tiles_exp(suffix),
      ];
      Some(((prefix, suffix), Exp(frame)));
    | Cond_then(((prefix, suffix), frame)) =>
      let prefix = [
        Selem.Shard(Exp(Cond_que)),
        ...Selection.of_tiles_exp(prefix),
      ];
      let suffix = [
        Selem.Shard(Exp(Cond_col)),
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
  print_endline("0");
  let* prefix = Selection.get_tiles(prefix);
  let* suffix = Selection.get_tiles(suffix);
  print_endline("1");
  switch (frame_t, frame) {
  | (((Pat(Paren_l), []), (Pat(Paren_r), [])), Pat(frame)) =>
    let+ prefix = Tiles.get_pat(prefix)
    and+ suffix = Tiles.get_pat(suffix);
    Frame.Pat(Paren_body(((prefix, suffix), frame)));
  | (((Exp(Lam_lam), []), (Exp(Lam_dot), [])), Exp(frame)) =>
    print_endline("yo");
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
  | (((Exp(Ap_l), []), (Exp(Ap_r), [])), Exp(frame)) =>
    let+ prefix = Tiles.get_exp(prefix)
    and+ suffix = Tiles.get_exp(suffix);
    Frame.Exp(Ap_arg(((prefix, suffix), frame)));
  | (((Exp(Let_let_eq(p)), []), (Exp(Let_in), [])), Exp(frame)) =>
    let+ prefix = Tiles.get_exp(prefix)
    and+ suffix = Tiles.get_exp(suffix);
    Frame.Exp(Let_def(p, ((prefix, suffix), frame)));
  | (((Exp(Cond_que), []), (Exp(Cond_col), [])), Exp(frame)) =>
    let+ prefix = Tiles.get_exp(prefix)
    and+ suffix = Tiles.get_exp(suffix);
    Frame.Exp(Cond_then(((prefix, suffix), frame)));
  | _ => None
  };
};

let unmatched_shards =
    (d: Direction.t, selection: Selection.t): list(Shard.t) => {
  let rec go = (~stack=[], selection: Selection.t) =>
    switch (selection) {
    | [] => stack
    | [Tile(_), ...selection] => go(~stack, selection)
    | [Shard(shard), ...selection] =>
      let cons_unless_end = (hd, tl) =>
        Shard.is_end(~strict=true, d, hd) ? tl : [hd, ...tl];
      switch (stack) {
      | [shard', ...tl] when Shard.is_next(d, shard', shard) =>
        go(~stack=cons_unless_end(shard, tl), selection)
      | _ => go(~stack=cons_unless_end(shard, stack), selection)
      };
    };
  go(d == Left ? List.rev(selection) : selection);
};

/**
 * `split_matching_shards(d, selection, affix)` splits the `d`-side
 * `affix` into a pair of selections, the first of which minimally
 * contains all shards in `affix` matching those in `selection`,
 * the second of which carries the rest of `affix`.
 */
let split_matching_shards =
    (d: Direction.t, selection: Selection.t, affix: Selection.t)
    : (Selection.t, Selection.t) => {
  let rec go = (~stack, affix: Selection.t) =>
    switch (stack) {
    | [] => ([], affix)
    | [shard, ...stack'] =>
      switch (affix) {
      | [] => ([], [])
      | [Tile(_) as tile, ...affix] =>
        let (matching, affix) = go(~stack, affix);
        ([tile, ...matching], affix);
      | [Shard(shard'), ...affix'] =>
        if (Shard.is_next(d, shard, shard')) {
          let stack =
            Shard.is_end(~strict=true, d, shard')
              ? stack' : [shard', ...stack'];
          let (matching, affix) = go(~stack, affix');
          ([Shard(shard'), ...matching], affix);
        } else {
          ([], affix);
        }
      }
    };
  go(~stack=unmatched_shards(d, selection), affix);
};

let rec parse_zipper =
        ((prefix, suffix) as sframe: Selection.frame, frame: Frame.t)
        : (Selection.frame, Frame.t) => {
  switch (find_shard(Left, prefix), find_shard(Right, suffix)) {
  | (None, _)
  | (_, None) => (sframe, frame)
  | (
      Some((tiles_pre, token_pre, prefix)),
      Some((tiles_suf, token_suf, suffix)),
    ) =>
    switch (
      find_rest_of_selem(~strict=true, Left, token_pre, prefix),
      find_rest_of_selem(~strict=true, Right, token_suf, suffix),
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

let round_up =
    (
      ~frame_sort: Sort.t,
      selection: Selection.t,
      (prefix, suffix): Selection.frame,
    )
    : (Selection.t, Selection.frame) => {
  let split_rounded = d =>
    ListUtil.take_while(
      fun
      | Selem.Tile(tile) => Tile.sort(tile) != frame_sort
      | Shard(shard) =>
        snd(Shard.tip(Direction.toggle(d), shard)) != frame_sort,
    );
  let (selection_pre, prefix) = split_rounded(Left, prefix);
  let (selection_suf, suffix) = split_rounded(Right, suffix);
  let new_selection =
    parse_selection(
      Right,
      ListFrame.to_list(~subject=selection, (selection_pre, selection_suf)),
    );
  (new_selection, (prefix, suffix));
};

let is_backpack_whole =
    (sort, (d, selection, rest): Restructuring.Backpack.t) => {
  let selections = [selection, ...rest];
  let selections = d == Left ? List.rev(selections) : selections;
  let total_selection = List.concat(selections);
  Selection.is_whole(sort, parse_selection(Right, total_selection));
};
let is_backpack_whole_any = backpack =>
  is_backpack_whole(Pat, backpack) || is_backpack_whole(Exp, backpack);

let rec fix_holes_relems =
        (ltip: Tip.t, affix: list(Restructuring.frame_elem))
        : (list(Restructuring.frame_elem), Tip.t) =>
  switch (affix) {
  | [] => ([], Tip.toggle(ltip))
  | [relem, ...affix] =>
    switch (relem) {
    | Tile(tile) when Tile.is_hole(tile) =>
      // skip holes
      fix_holes_relems(ltip, affix)
    | _ =>
      let (fixed, rtip) =
        fix_holes_relems(
          Tip.toggle(Restructuring.tip(Right, relem)),
          affix,
        );
      let inserted_hole =
        // relies on invariant that reachable selections are sort-consistent
        Restructuring.tip(Left, relem) == ltip
          ? [] : [Restructuring.Tile(Tile.mk_hole(ltip))];
      (inserted_hole @ [relem, ...fixed], rtip);
    }
  };

/**
 * rushed impl for maintaining selection
 * upon emptying backpack
 */
let fix_holes_selection = (ltip, selection, (prefix, suffix), rtip) => {
  let (fixed_prefix, rtip_prefix) =
    fix_holes_relems(ltip, List.rev(prefix));
  let (prefix_hd, rtip_prefix) = {
    // put hole on prefix instead of selection
    let ltip_selection = Tip.toggle(rtip_prefix);
    ltip_selection == Selem.tip(Left, List.hd(selection))
      ? ([], rtip_prefix)
      : (
        Restructuring.mk_relems([Selem.Tile(Tile.mk_hole(ltip_selection))]),
        ltip_selection,
      );
  };
  let (fixed_selection, rtip_selection) =
    fix_holes_relems(
      Tip.toggle(rtip_prefix),
      Restructuring.mk_relems(selection),
    );
  let (fixed_suffix, rtip_suffix) =
    fix_holes_relems(Tip.toggle(rtip_selection), suffix);
  let suffix_end =
    rtip_suffix == rtip ? [] : [Restructuring.Tile(Tile.mk_hole(rtip))];
  (
    fixed_selection,
    (List.rev(fixed_prefix @ prefix_hd), fixed_suffix @ suffix_end),
  );
};

let fix_holes_rframe =
    (ltip: Tip.t, (prefix, suffix): Restructuring.frame, rtip: Tip.t)
    : Restructuring.frame => {
  let (fixed_prefix, rtip_prefix) =
    fix_holes_relems(ltip, List.rev(prefix));
  let (fixed_suffix, rtip_suffix) =
    fix_holes_relems(Tip.toggle(rtip_prefix), suffix);
  let inserted_hole =
    rtip_suffix == rtip ? [] : [Restructuring.Tile(Tile.mk_hole(rtip))];
  (List.rev(fixed_prefix), fixed_suffix @ inserted_hole);
};
let fix_holes = (ltip, sframe, rtip) => {
  let rframe = fix_holes_rframe(ltip, Restructuring.mk_frame(sframe), rtip);
  Restructuring.get_sframe(rframe);
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
