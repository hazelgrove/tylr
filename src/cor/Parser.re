open Util;
open OptUtil.Syntax;

let disassemble_selem: Selem.t => Selection.t =
  fun
  | Token(_) => []
  | Tile(Pat(tile)) =>
    switch (tile) {
    | OpHole
    | Var(_)
    | BinHole
    | Prod => []
    | Paren(body) =>
      [Selem.Token(Pat(Paren_l)), ...Selection.of_tiles_pat(body)]
      @ [Token(Pat(Paren_r))]
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
      [Selem.Token(Exp(Paren_l)), ...Selection.of_tiles_exp(body)]
      @ [Token(Exp(Paren_r))]
    | Lam(p) =>
      [Selem.Token(Exp(Lam_lam)), ...Selection.of_tiles_pat(p)]
      @ [Token(Exp(Lam_dot))]
    | Let(p, def) =>
      [Selem.Token(Exp(Let_let)), ...Selection.of_tiles_pat(p)]
      @ [Selem.Token(Exp(Let_eq)), ...Selection.of_tiles_exp(def)]
      @ [Selem.Token(Exp(Let_in))]
    };

let rec find_token =
        (d: Direction.t, selection: Selection.t)
        : option((Tiles.t, Token.t, Selection.t)) =>
  switch (selection) {
  | [] => None
  | [Token(found_token), ...selection] => Some(([], found_token, selection))
  | [Tile(tile), ...selection] =>
    let+ (preceding_tiles, found_token, selection) =
      find_token(d, selection);
    ([tile, ...preceding_tiles], found_token, selection);
  };

let rec find_rest_of_tile =
        (d: Direction.t, curr_token: Token.t, selection: Selection.t)
        : option((AltList.even(Tiles.t, Token.t), Selection.t)) =>
  if (Token.is_end(d, curr_token)) {
    Some(([], selection));
  } else {
    let* (preceding_tiles, found_token, selection) =
      find_token(d, selection);
    if (Token.is_next(d, curr_token, found_token)) {
      let+ (rest_of_tile, selection) =
        find_rest_of_tile(d, found_token, selection);
      ([(preceding_tiles, found_token), ...rest_of_tile], selection);
    } else {
      None;
    };
  };

// TODO direction
let assemble_tile: AltList.t(Token.t, Tiles.t) => option(Tile.t) =
  fun
  | (Pat(Paren_l), [(body, Pat(Paren_r))]) => {
      let+ body = Tiles.get_pat(body);
      Tile.Pat(Paren(body));
    }
  | (Exp(Paren_l), [(body, Exp(Paren_r))]) => {
      let+ body = Tiles.get_exp(body);
      Tile.Exp(Paren(body));
    }
  | (Exp(Lam_lam), [(p, Exp(Lam_dot))]) => {
      let+ p = Tiles.get_pat(p);
      Tile.Exp(Lam(p));
    }
  | (Exp(Let_let), [(p, Exp(Let_eq)), (def, Exp(Let_in))]) => {
      let+ p = Tiles.get_pat(p)
      and+ def = Tiles.get_exp(def);
      Tile.Exp(Let(p, def));
    }
  | _ => None;

let flatten = l =>
  l
  |> AltList.even_to_list(Selection.of_tiles, t => [Selem.Token(t)])
  |> List.flatten;

let rec parse_selection =
        (d: Direction.t, selection: Selection.t): Selection.t => {
  switch (find_token(d, selection)) {
  | None => selection
  | Some((tiles, token, selection)) =>
    switch (find_rest_of_tile(d, token, selection)) {
    | None => flatten([(tiles, token)]) @ parse_selection(d, selection)
    | Some((rest_of_tile, selection)) =>
      let parsed =
        switch (assemble_tile((token, rest_of_tile))) {
        | None => flatten([(tiles, token), ...rest_of_tile])
        | Some(tile) => Selection.of_tiles(tiles) @ [Tile(tile)]
        };
      parsed @ parse_selection(d, selection);
    }
  };
};

let disassemble_frame: Frame.t => (Selection.frame, Frame.t) =
  fun
  | Pat(frame) =>
    switch (frame) {
    | Paren_body(((prefix, suffix), frame)) =>
      let prefix = [
        Selem.Token(Pat(Paren_l)),
        ...Selection.of_tiles_pat(prefix),
      ];
      let suffix = [
        Selem.Token(Pat(Paren_r)),
        ...Selection.of_tiles_pat(suffix),
      ];
      ((prefix, suffix), Pat(frame));
    | Lam_pat(((prefix, suffix), frame)) =>
      let prefix = [
        Selem.Token(Exp(Lam_lam)),
        ...Selection.of_tiles_exp(prefix),
      ];
      let suffix = [
        Selem.Token(Exp(Lam_dot)),
        ...Selection.of_tiles_exp(suffix),
      ];
      ((prefix, suffix), Exp(frame));
    | Let_pat(def, ((prefix, suffix), frame)) =>
      let prefix = [
        Selem.Token(Exp(Let_let)),
        ...Selection.of_tiles_exp(prefix),
      ];
      let suffix =
        [Selem.Token(Exp(Let_eq)), ...Selection.of_tiles_exp(def)]
        @ [Token(Exp(Let_in)), ...Selection.of_tiles_exp(suffix)];
      ((prefix, suffix), Exp(frame));
    }
  | Exp(frame) =>
    switch (frame) {
    | Paren_body(((prefix, suffix), frame)) =>
      let prefix = [
        Selem.Token(Exp(Paren_l)),
        ...Selection.of_tiles_exp(prefix),
      ];
      let suffix = [
        Selem.Token(Exp(Paren_r)),
        ...Selection.of_tiles_exp(suffix),
      ];
      ((prefix, suffix), Exp(frame));
    | Let_def(p, ((prefix, suffix), frame)) =>
      let prefix =
        [Selem.Token(Exp(Let_eq)), ...Selection.of_tiles_pat(p)]
        @ [Token(Exp(Let_let)), ...Selection.of_tiles_exp(prefix)];
      let suffix = [
        Selem.Token(Exp(Let_in)),
        ...Selection.of_tiles_exp(suffix),
      ];
      ((prefix, suffix), Exp(frame));
    };

let assemble_frame =
    (
      frame_t: (AltList.t(Token.t, Tiles.t) as 't, 't),
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
    let+ p = Tiles.get_pat(p)
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
