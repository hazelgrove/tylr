open Util;
open OptUtil.Syntax;

[@deriving sexp]
type t =
  | Mark
  | Move(Direction.t)
  | Delete(Direction.t)
  | Construct(Selem.t);

let front_affix =
  fun
  | Direction.Left => fst
  | Right => snd;
let back_affix =
  fun
  | Direction.Left => snd
  | Right => fst;
let mk_frame = (d: Direction.t, front, back) =>
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
      Some(Parser.parse_zipper(mk_frame(d, toward, away), frame));
    | [_, ..._] as disassembled =>
      // [Move<d>Disassembles]
      move_pointing(
        d,
        mk_frame(d, disassembled @ toward, back_affix(d, sframe)),
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
          mk_frame(caret_side, near, back_affix(caret_side, sframe));
        Some((caret_side, selection', sframe', frame));
      | [_, ..._] as disassembled =>
        // [SelectGrow<caret_side>Disassembles]
        let sframe' =
          mk_frame(
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
              mk_frame(
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
    (d: Direction.t, (backpack, rframe): Restructuring.t, frame: Frame.t)
    : option((Restructuring.t, Frame.t)) =>
  if (Parser.is_backpack_whole(backpack)) {
    // [Move<d>RestructuringWhole]
    let+ (sframe, frame) =
      move_pointing(d, Restructuring.get_sframe(rframe), frame);
    ((backpack, Restructuring.mk_frame(sframe)), frame);
  } else {
    // [Move<d>RestructuringNotWhole]
    switch (front_affix(d, rframe)) {
    | [] => None
    | [Tile(tile), ...front] =>
      let rframe =
        mk_frame(d, front, [Tile(tile), ...back_affix(d, rframe)]);
      Some(((backpack, rframe), frame));
    | [Selection(selection'), ...front] =>
      let+ backpack =
        Restructuring.Backpack.pick_up_selection(d, selection', backpack);
      let rframe = mk_frame(d, front, back_affix(d, rframe));
      ((backpack, rframe), frame);
    };
  };

let rec disassemble_and_enter =
        (selem: Selem.t, (prefix, suffix): Selection.frame) =>
  switch (Parser.disassemble_selem(Right, selem)) {
  | [] => ([selem, ...prefix], suffix)
  | [hd, ...tl] => disassemble_and_enter(hd, (prefix, tl @ suffix))
  };

let trim_selection = (selection: Selection.t) => {
  let trim_l =
    fun
    | [] => []
    | [hd, ...tl] as selection => Selem.is_hole(hd) ? tl : selection;
  let trim_r = selection =>
    switch (ListUtil.split_last_opt(selection)) {
    | None => []
    | Some((leading, last)) => Selem.is_hole(last) ? leading : selection
    };
  trim_r(trim_l(selection));
};

let perform = (a: t, (subject, frame): Zipper.t): option(Zipper.t) => {
  let frame_sort = Frame.sort(frame);

  let fix_holes = sframe => {
    let tip = (Tip.Convex, frame_sort);
    Parser.fix_holes(tip, sframe, tip);
  };

  let mark_selecting = (selection, sframe, frame) => {
    // [MarkSelecting]
    let* (_, lsort) = Selection.tip(Left, selection);
    let* (_, rsort) = Selection.tip(Right, selection);
    if (lsort != rsort) {
      None;
    } else {
      let tip = (Tip.Convex, Frame.sort(frame));
      let sframe = Parser.fix_holes(tip, sframe, tip);
      switch (trim_selection(selection)) {
      | [] => Some((Subject.Pointing(sframe), frame))
      | [_, ..._] as trimmed =>
        let rframe = Restructuring.mk_frame(sframe);
        Some((
          Subject.Restructuring(((Right, trimmed, []), rframe)),
          frame,
        ));
      };
    };
  };

  let delete_selecting = (selection, sframe, frame) =>
    if (Selection.is_whole_any(selection)) {
      let tip = (Tip.Convex, Frame.sort(frame));
      let sframe = Parser.fix_holes(tip, sframe, tip);
      Some((Subject.Pointing(sframe), frame));
    } else {
      let (selection, sframe) =
        Parser.round_up(~frame_sort, selection, sframe);
      mark_selecting(selection, sframe, frame);
    };

  switch (subject) {
  | Pointing(sframe) =>
    switch (a) {
    | Mark =>
      // [MarkPointing]
      Some((Selecting(Left, [], sframe), frame))
    | Move(d) =>
      let+ (sframe, frame) = move_pointing(d, sframe, frame);
      (Subject.Pointing(sframe), frame);
    | Delete(d) =>
      let* (_, selection, sframe, frame) =
        move_selecting(d, Left, [], sframe, frame);
      delete_selecting(selection, sframe, frame);
    | Construct(selem) =>
      // [Construct]
      let sort = Selem.sort(selem);
      if (sort != Frame.sort(frame)) {
        None;
      } else {
        switch (selem) {
        | Tile(_) =>
          let sframe = disassemble_and_enter(selem, sframe);
          let (prefix, suffix) = fix_holes(sframe);
          let sframe = (
            Parser.parse_selection(Left, prefix),
            Parser.parse_selection(Right, suffix),
          );
          let (sframe, frame) = Parser.parse_zipper(sframe, frame);
          Some((Pointing(sframe), frame));
        | Shard(shard) =>
          let+ (d, _, _) as backpack =
            Restructuring.Backpack.of_shard(shard);
          let (prefix, suffix) = sframe;
          let sframe =
            switch (d) {
            | Left => (prefix, [selem, ...suffix])
            | Right => ([selem, ...prefix], suffix)
            };
          let rframe = Restructuring.mk_frame(sframe);
          (Subject.Restructuring((backpack, rframe)), frame);
        };
      };
    }
  | Selecting(side, selection, sframe) =>
    switch (a) {
    | Construct(_) => None
    | Delete(_) => delete_selecting(selection, sframe, frame)
    | Mark => mark_selecting(selection, sframe, frame)
    | Move(d) =>
      let+ (side, selection, sframe, frame) =
        move_selecting(d, side, selection, sframe, frame);
      (Subject.Selecting(side, selection, sframe), frame);
    }
  | Restructuring(
      ((d_backpack, selection, rest), (prefix, suffix) as rframe) as restructuring,
    ) =>
    switch (a) {
    | Mark =>
      if (Selection.is_partial(selection)
          || Selection.is_whole(Frame.sort(frame), selection)) {
        // [MarkRestructuring]
        let tip = Tip.(Convex, Frame.sort(frame));
        switch (rest) {
        | [] =>
          let rframe =
            Parser.fix_holes_rframe(
              tip,
              (prefix, [Selection(selection), ...suffix]),
              tip,
            );
          let (prefix, suffix) = Restructuring.get_sframe(rframe);
          let suffix = Parser.parse_selection(Right, suffix);
          let (sframe, frame) =
            Parser.parse_zipper((prefix, suffix), frame);
          Some((Pointing(sframe), frame));
        | [selection', ...rest'] =>
          let rframe =
            switch (d_backpack) {
            | Left => (
                prefix,
                [Restructuring.Selection(selection), ...suffix],
              )
            | Right => ([Selection(selection), ...prefix], suffix)
            };
          let rframe = Parser.fix_holes_rframe(tip, rframe, tip);
          let backpack = (d_backpack, selection', rest');
          Some((Restructuring((backpack, rframe)), frame));
        };
      } else {
        None;
      }
    | Move(d) =>
      let+ ((backpack, rframe), frame) =
        move_restructuring(d, restructuring, frame);
      (Subject.Restructuring((backpack, rframe)), frame);
    | Delete(_) =>
      // [Delete]
      let s = Frame.sort(frame);
      let tip = Tip.(Convex, s);
      let (prefix, suffix) =
        Restructuring.get_sframe(~filter_selections=true, rframe);
      let prefix' = Selection.filter_tiles(s, prefix);
      let suffix' = Selection.filter_tiles(s, suffix);
      let fixed = Parser.fix_holes(tip, (prefix', suffix'), tip);
      Some((Pointing(fixed), frame));
    | Construct(_) => None
    }
  };
};
