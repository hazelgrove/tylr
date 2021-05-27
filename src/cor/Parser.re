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
  | [Token(found_token), ...selection] =>
    let (_, tile_sort) = Token.tip(Direction.toggle(d), found_token);
    let preceding_tiles = Tiles.nil(tile_sort);
    Some((preceding_tiles, found_token, selection));
  | [Tile(tile), ...selection] =>
    let* (preceding_tiles, found_token, selection) =
      find_token(d, selection);
    let+ preceding_tiles = Tiles.cons(tile, preceding_tiles);
    (preceding_tiles, found_token, selection);
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
  | (Pat(Paren_l), [(Pat(body), Pat(Paren_r))]) =>
    Some(Pat(Paren(body)))
  | (Exp(Paren_l), [(Exp(body), Exp(Paren_r))]) =>
    Some(Exp(Paren(body)))
  | (Exp(Lam_lam), [(Pat(p), Exp(Lam_dot))]) => Some(Exp(Lam(p)))
  | (Exp(Let_let), [(Pat(p), Exp(Let_eq)), (Exp(def), Exp(Let_in))]) =>
    Some(Exp(Let(p, def)))
  | _ => None;

let rec parse_selection =
        (d: Direction.t, selection: Selection.t): Selection.t => {
  let flatten = l =>
    l
    |> AltList.even_to_list(Selection.of_tiles, t => [Selem.Token(t)])
    |> List.flatten;
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
      frame_s: (Tiles.t, Tiles.t),
      frame: Frame.t,
    )
    : option(Frame.t) =>
  switch (frame_t, frame_s, frame) {
  | (
      ((Pat(Paren_l), []), (Pat(Paren_r), [])),
      (Pat(prefix), Pat(suffix)),
      Pat(frame),
    ) =>
    Some(Pat(Paren_body(((prefix, suffix), frame))))
  | (
      ((Exp(Lam_lam), []), (Exp(Lam_dot), [])),
      (Exp(prefix), Exp(suffix)),
      Exp(frame),
    ) =>
    Some(Pat(Lam_pat(((prefix, suffix), frame))))
  | (
      ((Exp(Let_let), []), (Exp(Let_eq), [(Exp(def), Exp(Let_in))])),
      (Exp(prefix), Exp(suffix)),
      Exp(frame),
    ) =>
    Some(Pat(Let_pat(def, ((prefix, suffix), frame))))
  | (
      ((Exp(Paren_l), []), (Exp(Paren_r), [])),
      (Exp(prefix), Exp(suffix)),
      Exp(frame),
    ) =>
    Some(Exp(Paren_body(((prefix, suffix), frame))))
  | (
      ((Exp(Let_eq), [(Pat(p), Exp(Let_let))]), (Exp(Let_in), [])),
      (Exp(prefix), Exp(suffix)),
      Exp(frame),
    ) =>
    Some(Exp(Let_def(p, ((prefix, suffix), frame))))
  | _ => None
  };

// let rec parse_zipper = (
//   (prefix, suffix): Selection.frame,
//   frame: Frame.t,
// )
// : (Selection.frame, Frame.t) => {
//   let* (tiles_pre, token_pre, prefix) = find_token(Left, prefix);
//   let* (tiles_suf, token_suf, suffix) = find_token(Right, suffix);

//   // let rec go = (
//   //   d: Direction.t,
//   //   affix: Selection.t,
//   // )
//   // : option(list(AltList.even(Tiles.t, Token.t)))
// }
let parse_zipper = _ => failwith("todo");

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
