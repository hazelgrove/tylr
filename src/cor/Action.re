open Util;
open OptUtil.Syntax;

[@deriving sexp]
type t =
  | Mark
  | Move(Direction.t)
  | Delete
  | Construct(Tile.t);

let front_affix =
  fun
  | Direction.Left => fst
  | Right => snd;
let back_affix =
  fun
  | Direction.Left => snd
  | Right => fst;
let mk_sframe = (d: Direction.t, front, back) =>
  switch (d) {
  | Left => (front, back)
  | Right => (back, front)
  };

let rec move_pointing =
        (d: Direction.t, sframe: Selection.frame, frame: Frame.t)
        : option((Selection.frame, Frame.t)) => {
  switch (front_affix(d, sframe)) {
  | [] =>
    // [Move<d>Frame]
    let* (sframe', frame') = Parser.disassemble_frame(frame);
    switch (front_affix(d, sframe')) {
    | [] => None
    | [_, ..._] =>
      move_pointing(d, ListFrame.append(sframe, sframe'), frame')
    };
  | [selem, ...toward] =>
    switch (Parser.disassemble_selem(d, selem)) {
    | [] =>
      // [Move<d>Atomic]
      let away =
        Parser.parse_selection(
          Direction.toggle(d),
          [selem, ...back_affix(d, sframe)],
        );
      Some(Parser.parse_zipper(mk_sframe(d, toward, away), frame));
    | [_, ..._] as disassembled =>
      // [Move<d>Disassembles]
      move_pointing(
        d,
        mk_sframe(d, disassembled @ toward, back_affix(d, sframe)),
        frame,
      )
    }
  };
};

let rec move_selecting =
        (
          d: Direction.t,
          caret_side: Direction.t,
          selection: Selection.t,
          sframe: Selection.frame,
          frame: Frame.t,
        )
        : option((Direction.t, Selection.t, Selection.frame, Frame.t)) => {
  let split = selection =>
    switch (caret_side) {
    | Left =>
      switch (selection) {
      | [] => None
      | [selem, ...selection] => Some((selem, selection))
      }
    | Right =>
      let+ (selection, selem) = ListUtil.split_last_opt(selection);
      (selem, selection);
    };
  let grow = (growth, selection) =>
    switch (caret_side) {
    | Left => growth @ selection
    | Right => selection @ growth
    };
  if (d == caret_side) {
    switch (front_affix(caret_side, sframe)) {
    | [] =>
      // [Select<caret_side>Frame]
      let* (sframe', frame') = Parser.disassemble_frame(frame);
      move_selecting(
        d,
        caret_side,
        selection,
        ListFrame.append(sframe, sframe'),
        frame',
      );
    | [selem, ...near] =>
      switch (Parser.disassemble_selem(d, selem)) {
      | [] =>
        // [SelectGrow<caret_side>Atomic]
        let selection' =
          Parser.parse_selection(Right, grow([selem], selection));
        let sframe' =
          mk_sframe(caret_side, near, back_affix(caret_side, sframe));
        Some((caret_side, selection', sframe', frame));
      | [_, ..._] as disassembled =>
        // [SelectGrow<caret_side>Disassembles]
        let sframe' =
          mk_sframe(
            caret_side,
            disassembled @ near,
            back_affix(caret_side, sframe),
          );
        move_selecting(d, caret_side, selection, sframe', frame);
      }
    };
  } else {
    switch (split(selection)) {
    | None => move_selecting(d, d, selection, sframe, frame)
    | Some((selem, selection)) =>
      switch (Parser.disassemble_selem(Right, selem)) {
      | [] =>
        // [SelectShrink<caret_side>Atomic]
        let (sframe', frame') =
          Parser.(
            parse_zipper(
              mk_sframe(
                caret_side,
                parse_selection(
                  caret_side,
                  [selem, ...front_affix(caret_side, sframe)],
                ),
                back_affix(caret_side, sframe),
              ),
              frame,
            )
          );
        Some((caret_side, selection, sframe', frame'));
      | [_, ..._] as disassembled =>
        // [SelectShrink<caret_side>Disassembles]
        let selection' = grow(disassembled, selection);
        move_selecting(d, caret_side, selection', sframe, frame);
      }
    };
  };
};

let move_restructuring =
    (
      d: Direction.t,
      selection: Selection.t,
      sframe: Selection.frame,
      frame: Frame.t,
    )
    : option((Selection.frame, Frame.t)) =>
  if (Selection.is_whole_any(selection)) {
    // [Move<d>RestructuringWhole]
    move_pointing(d, sframe, frame);
  } else {
    // [Move<d>RestructuringNotWhole]
    switch (front_affix(d, sframe)) {
    | []
    | [Shard(_), ..._] => None
    | [Tile(_) as selem, ...front] =>
      let sframe = mk_sframe(d, front, [selem, ...back_affix(d, sframe)]);
      Some((sframe, frame));
    };
  };

let rec disassemble_and_enter =
        (selem: Selem.t, (prefix, suffix): Selection.frame) =>
  switch (Parser.disassemble_selem(Right, selem)) {
  | [] => ([selem, ...prefix], suffix)
  | [hd, ...tl] => disassemble_and_enter(hd, (prefix, tl @ suffix))
  };

let perform = (a: t, (subject, frame): Zipper.t): option(Zipper.t) =>
  switch (subject) {
  | Pointing(sframe) =>
    switch (a) {
    | Mark =>
      // [MarkPointing]
      Some((Selecting(Left, [], sframe), frame))
    | Move(d) =>
      let+ (sframe, frame) = move_pointing(d, sframe, frame);
      (Subject.Pointing(sframe), frame);
    | Delete => None
    | Construct(tile) =>
      // [Construct]
      let sort = Tile.sort(tile);
      if (sort != Frame.sort(frame)) {
        None;
      } else {
        let sframe = disassemble_and_enter(Tile(tile), sframe);
        let (prefix, suffix) = {
          let tip = (Tip.Convex, sort);
          Parser.fix_holes(tip, sframe, tip);
        };
        let sframe = (
          Parser.parse_selection(Left, prefix),
          Parser.parse_selection(Right, suffix),
        );
        let (sframe, frame) = Parser.parse_zipper(sframe, frame);
        Some((Pointing(sframe), frame));
      };
    }
  | Selecting(side, selection, sframe) =>
    switch (a) {
    | Delete
    | Construct(_) => None
    | Mark =>
      // [MarkSelecting]
      let* (_, lsort) = Selection.tip(Left, selection);
      let* (_, rsort) = Selection.tip(Right, selection);
      if (lsort != rsort) {
        None;
      } else {
        let tip = (Tip.Convex, Frame.sort(frame));
        let sframe = Parser.fix_holes(tip, sframe, tip);
        Some((Subject.Restructuring(selection, sframe), frame));
      };
    | Move(d) =>
      let+ (side, selection, sframe, frame) =
        move_selecting(d, side, selection, sframe, frame);
      (Subject.Selecting(side, selection, sframe), frame);
    }
  | Restructuring(selection, (prefix, suffix) as sframe) =>
    switch (a) {
    | Mark =>
      if (Selection.is_partial(selection)
          || Selection.is_whole(Frame.sort(frame), selection)) {
        // [MarkRestructuring]
        let tip = Tip.(Convex, Frame.sort(frame));
        let (prefix, suffix) =
          Parser.fix_holes(tip, (prefix, selection @ suffix), tip);
        let suffix = Parser.parse_selection(Right, suffix);
        let (sframe, frame) = Parser.parse_zipper((prefix, suffix), frame);
        Some((Pointing(sframe), frame));
      } else {
        None;
      }
    | Move(d) =>
      let+ (sframe, frame) = move_restructuring(d, selection, sframe, frame);
      (Subject.Restructuring(selection, sframe), frame);
    | Delete =>
      // [Delete]
      let s = Frame.sort(frame);
      let tip = Tip.(Convex, s);
      let prefix' = Selection.filter_tiles(s, prefix);
      let suffix' = Selection.filter_tiles(s, suffix);
      let fixed = Parser.fix_holes(tip, (prefix', suffix'), tip);
      Some((Pointing(fixed), frame));
    | Construct(_) => None
    }
  };
