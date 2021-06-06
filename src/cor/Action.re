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
          selection: Selection.t,
          (prefix, suffix) as sframe: Selection.frame,
          frame: Frame.t,
        )
        : option((Selection.t, Selection.frame, Frame.t)) => {
  switch (d) {
  | Left =>
    switch (prefix) {
    | [] =>
      // [SelectLeftFrame]
      let* (sframe', frame') = Parser.disassemble_frame(frame);
      move_selecting(
        d,
        selection,
        ListFrame.append(sframe, sframe'),
        frame',
      );
    | [selem, ...prefix] =>
      switch (Parser.disassemble_selem(Left, selem)) {
      | [] =>
        // [SelectGrowLeftAtomic]
        let selection' =
          Parser.parse_selection(Right, [selem, ...selection]);
        Some((selection', (prefix, suffix), frame));
      | [_, ..._] as disassembled =>
        // [SelectGrow<caret_side>Disassembles]
        let sframe' = (disassembled @ prefix, suffix);
        move_selecting(d, selection, sframe', frame);
      }
    }
  | Right =>
    switch (selection) {
    | [] => None
    | [selem, ...selection] =>
      switch (Parser.disassemble_selem(Right, selem)) {
      | [] =>
        // [SelectShrinkLeftAtomic]
        let (sframe', frame') =
          Parser.(
            parse_zipper(
              (parse_selection(Left, [selem, ...prefix]), suffix),
              frame,
            )
          );
        Some((selection, sframe', frame'));
      | [_, ..._] as disassembled =>
        // [SelectShrink<caret_side>Disassembles]
        let selection' = disassembled @ selection;
        move_selecting(d, selection', sframe, frame);
      }
    }
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
    | [Token(_), ..._] => None
    | [Tile(_) as selem, ...front] =>
      let sframe = mk_sframe(d, front, [selem, ...back_affix(d, sframe)]);
      Some((sframe, frame));
    };
  };

let perform = (a: t, (subject, frame): Zipper.t): option(Zipper.t) =>
  switch (subject) {
  | Pointing((prefix, suffix) as sframe) =>
    switch (a) {
    | Mark =>
      // [MarkPointing]
      Some((Selecting([], sframe), frame))
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
        let tip = (Tip.Convex, sort);
        let sframe =
          Parser.fix_holes(tip, (prefix, [Tile(tile), ...suffix]), tip);
        Some((Pointing(sframe), frame));
      };
    }
  | Selecting(selection, sframe) =>
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
      let+ (selection, sframe, frame) =
        move_selecting(d, selection, sframe, frame);
      (Subject.Selecting(selection, sframe), frame);
    }
  | Restructuring(selection, (prefix, suffix) as sframe) =>
    switch (a) {
    | Mark =>
      if (Selection.is_partial(selection)
          || Selection.is_whole(Frame.sort(frame), selection)) {
        // [MarkRestructuring]
        let tip = Tip.(Convex, Frame.sort(frame));
        let (sframe, frame) =
          Parser.(
            parse_zipper(
              Parser.fix_holes(
                tip,
                (prefix, parse_selection(Right, selection @ suffix)),
                tip,
              ),
              frame,
            )
          );
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
