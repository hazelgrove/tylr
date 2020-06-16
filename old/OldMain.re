module HExp = {
  type t =
    | Hole
    | Num(int)
    | Paren(t)
    | If(t, t, t)
    | BinOp(t, op, t)
  and op =
    | Plus
    | Times
    | Eq;
};

module UHTile = {
  type t =
    | Hole
    | Num(int)
    | Paren(HExp.t)
    | IfThenElse(HExp.t, HExp.t)
    | Plus
    | Times
    | Eq;

  // selection categorization
  // - no unmatched delimiters: operand
  // - at least one unmatched open delimiter xor at least one unmatched close delimiter: unary
  // - otherwise: binary

  // `(1 + _[) + (]2 3 + 4) + 5`

  /*
  1 +
  [(
    a + b
    let x = 1 in
    (]x + 1 + 2)
    c + d
  )
  + 5
  */

  let of_op: HExp.op => t =
    fun
    | Plus => Plus
    | Times => Times
    | Eq => Eq;

  let rec tiles_of_hexp: HExp.t => list(t) =
    fun
    | Hole => [Hole]
    | Num(n) => [Num(n)]
    | Paren(e) => [Paren(e)]
    | IfThenElse(e1, e2, e3) => [IfThenElse(e1, e2), ...tiles_of_hexp(e3)]
    | BinOp(e1, op, e2) => tiles_of_hexp(e1) @ [of_op(op), ...tiles_of_hexp(e2)];

  // tiles1 - tiles2 (assuming "non-negative difference")
  // [a, b, c] \ [a, b] = [c]
  let diff = (tiles1: list(t), tiles2: list(t)): option(list(t)) =>
    switch (tiles1, tiles2) {
    | ([], []) => Some([])
    | ([], [_, ..._]) => None
    | ([_, ..._], []) => Some(tiles)
    | ([tile1, ...tiles1], [tile2, ...tiles2]) =>
      tile1 == tile2
      ? diff(tiles1, tiles2)
      : None
    };
};

/*
module ZExp = {
  // top down
  type t = {
    frames: list(frame),
    subject,
    cursor: side,
  }
  and frame =
    | ParenZ_body
    | IfZ_cond(HExp.t, HExp.t)
    | IfZ_then_clause(HExp.t, HExp.t)
    | IfZ_else_clause(HExp.t, HExp.t)
    | BinOpZ_left(op, HExp.t)
    | BinOpZ_right(HExp.t, op)
  and subject =
    | HoleZ
    | NumZ(int)
    | ParenZ_open(HExp.t)
    | ParenZ_close(HExp.t)
    | IfZ_if(HExp.t, HExp.t, HExp.t)
    | IfZ_then(HExp.t, HExp.t, HExp.t)
    | IfZ_else(HExp.t, HExp.t, HExp.t)
    | BinOpZ_op(HExp.t, HExp.op, HExp.t)
  and side =
    | Before
    | After;

  type framed('a) = (list(frame), 'a);
  // TODO define t in terms of framed

  let rec erase =
    fun
    | {subject, frames, _} =>
      erase_frames(erase_subject(subject), List.rev(frames))
  and erase_subject: t => HExp.t =
    fun
    | HoleZ => Hole
    | NumZ(n) => Num(n)
    | ParenZ_open(e)
    | ParenZ_close(e) => Paren(e)
    | IfZ_if(e1, e2, e3)
    | IfZ_then(e1, e2, e3)
    | IfZ_else(e1, e2, e3) => If(e1, e2, e3)
    | BinOpZ_op(e1, op, e2) => BinOp(e1, op, e2)
  and erase_frames = (subject: HExp.t, rev_frames: list(frame)): HExp.t =>
    switch (rev_frames) {
    | [] => subject
    | [frame, ...rev_frames] =>
      let new_subject: HExp.t =
        switch (frame) {
        | ParenZ_body => Paren(subject)
        | IfZ_cond(e2, e3) => If(subject, e2, e3)
        | IfZ_then_clause(e1, e3) => If(e1, subject, e3)
        | IfZ_else_clause(e1, e2) => If(e1, e2, subject)
        | BinOpZ_left(op, e2) => BinOp(subject, op, e2)
        | BinOpZ_right(e1, op) => BinOp(e1, op, subject)
        };
      erase_frames(new_subject, rev_frames);
    };

  let is_bidelimiting_frame =
    fun
    | ParenZ_body
    | IfZ_cond(_)
    | IfZ_then_clause(_) => true
    | IfZ_else_clause(_)
    | BinOpZ_left(_)
    | BinOpZ_right(_) => false;

  let split: ZExp.t => (list(frame), (list(UHTile.t), list(UHTile.t))) =
    fun
    | {frames: [], subject, side} =>
      switch (subject, side) {
      | (ParenZ_open(e), After) => ([ParenZ_body], ([], UHTile.tiles_of_hexp(e)))
      | (ParenZ_close(e), Before) => ([ParenZ_body], (UHTile.tiles_of_hexp(e), []))
      | (IfZ_if(e1, e2, e3), After) => ([IfZ_cond(e2, e3)], ([], UHTile.tiles_of_hexp(e1)))
      | (IfZ_then(e1, e2, e3), Before) => ([IfZ_cond(e2, e3)], (UHTile.tiles_of_hexp(e1), []))
      | (IfZ_then(e1, e2, e3), After) => ([IfZ_then_clause(e1, e3)], ([], UHTile.tiles_of_hexp(e2)))
      | (IfZ_else(e1, e2, e3), Before) => ([IfZ_then_clause(e1, e3)], (UHTile.tiles_of_hexp(e2), []))
      | _ =>
        let tiles = UHTile.tiles_of_hexp(ZExp.erase(ze));
        switch (side) {
        | Before => ([], ([], tiles))
        | After => ([], (tiles, []))
        }
      }
    | {frames: [f, ...fs], _} as ze => {
        let (frames, split_tiles) = split({...ze, frames: fs});
        ([f, ...frames], split_tiles)
      };

  // least common ancestor
  // assumes same erasure
  let lca = (ze1, ze2): (HExp.t, list(frame)) => {
    let ((_, subject1), frames1) = ze1;
    let ((_, subject2), frames2) = ze2;

    let rec go = (rev_frames1, rev_frames2) =>
      switch (rev_frames1, rev_frames2) {
      | ([], _) => (erase_subject(subject1), frames1)
      | (_, []) => (erase_subject(subject2), frames2)
      | ([frame1, ...rev_frames1], [frame2, ...rev_frames2]) =>
        switch (frame1, frame2) {
        | _ when frame1 == frame2 => go(rev_frames1, rev_frames2)
        |
        }
      };

  }

};

module ZExp = {
  type t = (t', side)
  and t' =
    | HoleZ
    | NumZ(int)
    | ParenZ_open(Exp.t)
    | ParenZ_body(t)
    | ParenZ_close(Exp.t)
    | IfZ_if(Exp.t, Exp.t, Exp.t)
    | IfZ_cond(t, Exp.t, Exp.t)
    | IfZ_then(Exp.t, Exp.t, Exp.t)
    | IfZ_then_clause(Exp.t, t, Exp.t)
    | IfZ_else(Exp.t, Exp.t, Exp.t)
    | IfZ_else_clause(Exp.t, Exp.t, t)
    | BinOpZ_left(t, op, Exp.t)
    | BinOpZ_op(Exp.t, op, Exp.t)
    | BinOpZ_right(Exp.t, op, t);

  let rec erase = ((ze', _)) => erase'(ze')
  and erase': t' => HExp.t =
    fun
    | HoleZ => Hole
    | NumZ(n) => Num(n)
    | ParenZ_open(e)
    | ParenZ_close(e) => Paren(e)
    | ParenZ_body(ze) => Paren(erase(ze))
    | IfZ_if(e1, e2, e3)
    | IfZ_then(e1, e2, e3)
    | IfZ_else(e1, e2, e3) => If(e1, e2, e3)
    | IfZ_cond(ze1, e2, e3) => If(erase(ze1), e2, e3)
    | IfZ_then_clause(e1, ze2, e3) => If(e1, erase(ze2), e3)
    | IfZ_else_clause(e1, e2, ze3) => If(e1, e2, erase(ze3))
    | BinOpZ_left(ze1, op, e2) => BinOp(erase(ze1), op, e2)
    | BinOpZ_op(e1, op, e2) => BinOp(e1, op, e2)
    | BinOpZ_right(e1, op, ze2) => BinOp(e1, op, erase(ze2));
};

let rec split = (ze: ZExp.t): option((list(UHTile.t), list(UHTile.t))) =>
  switch (ze) {
  | (HoleZ | NumZ(_), side)
  | (ParenZ_open(e), Before as side)
  | (ParenZ_close(e), After as side)
  | (IfZ_if, Before as side) =>
    let tiles = UHTile.tiles_of_hexp(ZExp.erase(ze));
    switch (side) {
    | Before => Some(([], tiles))
    | After => Some((tiles, []))
    };

  | (IfZ_else(e1, e2, e3), After) =>
    Some(([IfThenElse(e1, e2)], UHTile.tiles_of_hexp(e3)))
  | (IfZ_else_clause(e1, e2, ze3), _) =>
    split(ze3)
    |> Option.map(((tiles1, tiles2)) => ([IfThenElse(e1, e2), ...tiles1], tiles2))

  | (BinOpZ_left(ze1, op, e2), _) =>
    split(ze1)
    |> Option.map(((tiles1, tiles2)) =>
      (tiles1, tiles2 @ [UHTile.of_op(op), ...UHTile.tiles_of_hexp(e2)])
    )
  | (BinOpZ_op(e1, op, e2), side) =>
    let tiles1 = UHTile.tiles_of_hexp(e1);
    let op_tile = UHTile.of_op(op);
    let tiles2 = UHTile.tiles_of_hexp(e2);
    switch (side) {
    | Before => Some((tiles1, [op_tile, ...tiles2]))
    | After => Some((tiles1 @ [op_tile], tiles2))
    }
  | (BinOpZ_right(e1, op, ze2), _) =>
    split(ze2)
    |> Option.map(((tiles1, tiles2)) =>
      (UHTile.tiles_of_hexp(e1) @ [UHTile.of_op(op), ...tiles1], tiles2)
    )

  | (ParenZ_open(_), After)
  | (ParenZ_close(_), Before)
  | (IfZ_if(_), After)
  | (IfZ_cond(_), _)
  | (IfZ_then(_), _)
  | (IfZ_then_clause(_), _)
  | (IfZ_else(_), Before) => None
  };
*/

type selection = {
  anchor: ZExp.t,
  focus: ZExp.t,
};

// 1 + ( 2 + (3) |) + 4  -->  1 + (_) + 4 , 2 + (3)|
let rec split_next_bidelimiting_frame =
  fun
  | {frames: [], subject, side} as ze =>
    switch (subject, side) {
    | (ParenZ_open(e), After) => ([ParenZ_body], ZExp.place_before(e))
    | (ParenZ_close(e), Before) => ([ParenZ_body], ZExp.place_after(e))
    | (IfZ_if(e1, e2, e3), After) => ([IfZ_cond(e2, e3)], ZExp.place_before(e1))
    | (IfZ_then(e1, e2, e3), Before) => ([IfZ_cond(e2, e3)], ZExp.place_after(e1))
    | (IfZ_then(e1, e2, e3), After) => ([IfZ_then_clause(e1, e3)], ZExp.place_before(e2))
    | (IfZ_else(e1, e2, e3), Before) => ([IfZ_then_clause(e1, e3)], ZExp.place_after(e2))
    | _ => ([], ze)
    }
  | {frames: [f, ...fs], _} as ze =>
    if (ZExp.is_bidelimiting_frame(f)) {
      ([f], {...ze, frames: fs})
    } else {
      let (wallpaper, ze) = split_next_bidelimiting_frame(ze);
      ([f, ...wallpaper], ze)
    };

let rec split_next_bidelimiting_frames = (ze: ZExp.t): (list(frame), ZExp.t) =>
  switch (split_next_bidelimiting_frame(ze)) {
  | ([], _) => ([], ze)
  | (outer_wallpaper, ze) =>
    let (inner_wallpaper, ze) = split_next_bidelimiting_frames(ze);
    (outer_wallpaper @ inner_wallpaper, ze);
  };

// assumes same erasure
let rec split_common_bidelimiting_frames = (ze1, ze2) =>
  switch (
    split_next_bidelimiting_frame(ze1),
    split_next_bidelimiting_frame(ze2),
  ) {
  | (([], _), _)
  | (_, ([], _)) => ([], (ze1, ze2))
  | ((outer_wallpaper, ze1), (_, ze2)) =>
    let (inner_wallpaper, (ze1, ze2)) =
      split_common_bidelimiting_frames(ze1, ze2);
    (outer_wallpaper @ inner_wallpaper, (ze1, ze2))
  };

/*
let split_common_frames = (ze1, ze2) => {
  let rec go = (rev_common_frames, frames1, frames2) =>
    switch (frames1, frames2) {
    | ([], _)
    | (_, []) => (List.rev(rev_common_frames), (frames1, frames2))
    | ([f1, ...fs1], [f2, ...fs2]) =>
      f1 == f2
      ? go([f1, ...rev_common_frames], fs1, fs2)
      : (List.rev(rev_common_frames), (frames1, frames2))
    };
  let (common_frames, frames1, frames2) =
    go([], ze1.frames, ze2.frames);
  (common_frames, {...ze1, frames: frames1}, {...ze2, frames: frames2});
};
*/


type tiles = list(UHTile.t);
type tile_split = (list(UHTile.t), list(UHTile.t));

type mode =
  | Disjoint(ZExp.framed(tile_split), ZExp.framed(tile_split))
  | OutIn(tile_split, ZExp.framed(tile_split), list(UHTile.t))
  | InOut(list(UHTile.t), ZExp.framed(tile_split), tile_split);

let restructure = ({anchor, focus}: selection, target: ZExp.t): option(ZExp.t) => {
  let (wallpaper, (anchor, focus)) = split_common_bidelimiting_frames(anchor, focus);
  switch (
    ZExp.split(anchor),
    ZExp.split(focus),
    ZExp.split(target),
  ) {
  | (([], _), ([], _), _) =>
    failwith("restructuring mode should only be entered when selection contains unmatched delimiters")
  | (
      ([f, ...fs] as anchor_frames, (anchor_tiles1, anchor_tiles2)),
      ([] as focus_frames, (focus_tiles1, focus_tiles2)),
      (target_frames, (target_tiles1, target_tiles2)),
    ) =>
    if (target_frames == wallpaper @ anchor_frames) {

    } else if (target_frames == wallpaper @ focus_frames) {

    } else {
      None;
    }
  }
}

let restructure = ({anchor, focus}: selection, target: ZExp.t): option(ZExp.t) => {
  let (anchor_biframes, anchor_tiles) = ZExp.split(anchor);
  let (focus_biframes, focus_tiles) = ZExp.split(focus);
  let (target_biframes, target_tiles) = ZExp.split(target);
  if (target_biframes == anchor_biframes) {
    let (target_tiles1, target_tiles2) = target_tiles;
    let (anchor_tiles1, anchor_tiles2) = anchor_tiles;
    switch (UHTile.diff(anchor_tiles1, target_tiles1)) {
    | None => None
    | Some(diff) =>

      // merge target_tiles1 and anchor_tiles2
      // insert diff between focus_tiles
      // restructure
    }
  }
}

let restructure = ({anchor, focus}: selection, target: ZExp.t): option(ZExp.t) => {
  let (common_frames, anchor, focus) = split_common_bidelimiting_frames(anchor, focus);
  switch (
    split_next_bidelimiting_frames(anchor),
    split_next_bidelimiting_frames(focus),
  ) {
  | (([], _), ([], _)) => // can place anywhere
  | (([], _), (inner_wallpaper, inner_ze)) =>
    // take outer tile context of anchor, split by anchor
    // take tile context of inner_ze, split by ze
    // check target is in one of these tile contexts
  | ((inner_wallpaper, inner_ze), ([], _)) =>
    // take outer tile context of focus, split by focus
    // take tile context of inner_ze, split by ze
    // check target is in one of these tile contexts
  | ((inner_wallpaper1, inner_ze1), (inner_wallpaper2, inner_ze2)) =>
    // take tile contexts of both inner zes
    // check target is in one of these tile contexts =
  }
}

/**
 * Major assumptions:
 * - arguments have same erasure
 *
 * Assumptions:
 * - anchor is before focus
 * - focus is before target
 */
let restructure = (anchor: ZExp.t, focus: ZExp.t, target: ZExp.t): option(ZExp.t) => {
  switch (anchor, focus, target) {
  | (ParenZ_body(anchor), ParenZ_body(focus), ParenZ_body(target)) =>
    restructure(anchor, focus, target)
    |> Option.map(ze => ParenZ_body(ze))
  | (IfZ_cond(anchor, e2, e3), IfZ_cond(focus, _, _), IfZ_cond(target, _, _)) =>
    restructure(anchor, focus, target)
    |> Option.map(ze => IfZ_cond(ze, e2, e3))
  | (IfZ_then_clause(e1, anchor, e3), IfZ_then_clause(_, focus, _), IfZ_then_clause(_, target, _)) =>
    restructure(anchor, focus, target)
    |> Option.map(ze => IfZ_then_clause(e1, ze, e3))
  | (IfZ_else_clause(e1, e2, anchor), IfZ_else_clause(_, _, focus), IfZ_else_clause(_, _, target)) =>
    restructure(anchor, focus, target)
    |> Option.map(ze => IfZ_else_clause(e1, e2, ze))
  | (BinOpZ)

  // if e1 [then 2] + 3| + 4 else e3
  | (IfZ_then(_), IfZ_then_clause(e1, ze2, e3), IfZ_then_clause(_, ze2', _))
      when ze2 << ze2' =>
    let (e2_left, e2_mid, e2_right) = partition(ze2, ze2');
    let new_e1 = merge(e1, e2_mid);
    let new_e2 = merge(e2_left, e2_right);
    Some(IfZ_then(new_e1, new_e2, e3));
  }
}