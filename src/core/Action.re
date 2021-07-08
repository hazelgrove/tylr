open Util;
open Result.Syntax;

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

let disassemble_frame_or_cannot_move =
    (frame: Frame.t): Result.t((Selection.frame, Frame.t), Failure.t) =>
  Result.of_option(
    ~error=Failure.Cant_move,
    Parser.disassemble_frame(frame),
  );

let rec move_pointing =
        (d: Direction.t, sframe: Selection.frame, frame: Frame.t)
        : Result.t((Selection.frame, Frame.t), Failure.t) => {
  switch (front_affix(d, sframe)) {
  | [] =>
    // [Move<d>Frame]
    let* (sframe', frame') = disassemble_frame_or_cannot_move(frame);
    switch (front_affix(d, sframe')) {
    | [] => Error(Cant_move)
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
      Ok(Parser.parse_zipper(mk_frame(d, toward, away), frame));
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
        : Result.t(
            (Direction.t, Selection.t, Selection.frame, Frame.t),
            Failure.t,
          ) => {
  let split = selection =>
    switch (caret_side) {
    | Left =>
      switch (selection) {
      | [] => None
      | [selem, ...selection] => Some((selem, selection))
      }
    | Right =>
      ListUtil.split_last_opt(selection)
      |> Option.map(((selection, selem)) => (selem, selection))
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
      let* (sframe', frame') = disassemble_frame_or_cannot_move(frame);
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
        Ok((caret_side, selection', sframe', frame));
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
        Ok((caret_side, selection, sframe', frame'));
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
    : Result.t((Restructuring.t, Frame.t), Failure.t) =>
  if (Parser.is_backpack_whole_any(backpack)) {
    // [Move<d>RestructuringWhole]
    let+ (sframe, frame) =
      move_pointing(d, Restructuring.get_sframe(rframe), frame);
    ((backpack, Restructuring.mk_frame(sframe)), frame);
  } else {
    // [Move<d>RestructuringNotWhole]
    switch (front_affix(d, rframe)) {
    | [] => Error(Cant_move)
    | [Tile(tile), ...front] =>
      let rframe =
        mk_frame(d, front, [Tile(tile), ...back_affix(d, rframe)]);
      Ok(((backpack, rframe), frame));
    | [Selection(selection'), ...front] =>
      let frame_sort = Frame.sort(frame);
      let+ backpack =
        Restructuring.Backpack.pick_up_selection(d, selection', backpack)
        |> Result.of_option(~error=Failure.Cant_pick_up_selection);
      let rframe = mk_frame(d, front, back_affix(d, rframe));
      let rframe =
        Parser.fix_holes_rframe(
          (Convex, frame_sort),
          rframe,
          (Convex, frame_sort),
        );
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

let perform =
    (a: t, (subject, frame): Zipper.t)
    : (Result.t(Zipper.t, Failure.t) as 'r) => {
  let frame_sort = Frame.sort(frame);

  let fix_holes = (frame_sort, sframe) => {
    let tip = (Tip.Convex, frame_sort);
    Parser.fix_holes(tip, sframe, tip);
  };

  let mark_selecting = (selection, sframe, frame): 'r => {
    // [MarkSelecting]
    switch (Selection.tip(Left, selection), Selection.tip(Right, selection)) {
    | (None, _)
    | (_, None) => Ok((Pointing(sframe), frame))
    | (Some((_, lsort)), Some((_, rsort))) =>
      if (lsort != rsort) {
        Error(Cant_pick_up_selection);
      } else {
        let sframe = fix_holes(Frame.sort(frame), sframe);
        switch (trim_selection(selection)) {
        | [] => Ok((Pointing(sframe), frame))
        | [_, ..._] as trimmed =>
          let rframe = Restructuring.mk_frame(sframe);
          Ok((Restructuring(((Right, trimmed, []), rframe)), frame));
        };
      }
    };
  };

  let delete_selecting = (d: Direction.t, selection, sframe, frame): 'r => {
    let frame_sort = Frame.sort(frame);
    if (trim_selection(selection) == []) {
      let (prefix, suffix) = sframe;
      let sframe =
        d == Left
          ? (prefix, selection @ suffix)
          : (List.rev(selection) @ prefix, suffix);
      let (sframe, frame) = Parser.parse_zipper(sframe, frame);
      Ok((Pointing(sframe), frame));
    } else if (Selection.is_whole_any(selection)) {
      let sframe = fix_holes(frame_sort, sframe);
      Ok((Pointing(sframe), frame));
    } else {
      let (selection, sframe) =
        Parser.round_up(~frame_sort, selection, sframe);
      mark_selecting(selection, sframe, frame);
    };
  };

  switch (subject) {
  | Pointing(sframe) =>
    switch (a) {
    | Mark =>
      // [MarkPointing]
      Ok((Selecting(Left, [], sframe), frame))
    | Move(d) =>
      let+ (sframe, frame) = move_pointing(d, sframe, frame);
      (Subject.Pointing(sframe), frame);
    | Delete(d) =>
      let* (_, selection, sframe, frame) =
        move_selecting(d, Left, [], sframe, frame);
      delete_selecting(d, selection, sframe, frame);
    | Construct(selem) =>
      // [Construct]
      let sort = Selem.sort(selem);
      if (sort != frame_sort) {
        Error(Cant_construct(sort, frame_sort));
      } else {
        switch (selem) {
        | Tile(_) =>
          let sframe = disassemble_and_enter(selem, sframe);
          let (prefix, suffix) = fix_holes(frame_sort, sframe);
          let sframe = (
            Parser.parse_selection(Left, prefix),
            Parser.parse_selection(Right, suffix),
          );
          let (sframe, frame) = Parser.parse_zipper(sframe, frame);
          Ok((Pointing(sframe), frame));
        | Shard(shard) =>
          switch (Restructuring.Backpack.of_shard(shard)) {
          | None => Error(Undefined)
          | Some((d, _, _) as backpack) =>
            let (prefix, suffix) = sframe;
            let sframe =
              switch (d) {
              | Left => (prefix, [selem, ...suffix])
              | Right => ([selem, ...prefix], suffix)
              };
            let rframe = Restructuring.mk_frame(sframe);
            Ok((Restructuring((backpack, rframe)), frame));
          }
        };
      };
    }
  | Selecting(side, selection, sframe) =>
    switch (a) {
    | Construct(_) => Error(Undefined)
    | Delete(d) => delete_selecting(d, selection, sframe, frame)
    | Mark => mark_selecting(selection, sframe, frame)
    | Move(d) =>
      let+ (side, selection, sframe, frame) =
        move_selecting(d, side, selection, sframe, frame);
      (Subject.Selecting(side, selection, sframe), frame);
    }
  | Restructuring(
      (
        (d_backpack, selection, rest) as backpack,
        (prefix, suffix) as rframe,
      ) as restructuring,
    ) =>
    switch (a) {
    | Mark =>
      if (Parser.is_backpack_whole(frame_sort, backpack)
          || !Parser.is_backpack_whole_any(backpack)) {
        // [MarkRestructuring]
        let fix_holes_rframe = rframe => {
          let tip = (Tip.Convex, frame_sort);
          Parser.fix_holes_rframe(tip, rframe, tip);
        };
        switch (rest) {
        | [] =>
          let rframe =
            fix_holes_rframe((prefix, [Selection(selection), ...suffix]));
          let (prefix, suffix) = Restructuring.get_sframe(rframe);
          let prefix = Parser.parse_selection(Left, prefix);
          let suffix = Parser.parse_selection(Right, suffix);
          let (sframe, frame) =
            Parser.parse_zipper((prefix, suffix), frame);
          Ok((Pointing(sframe), frame));
        | [selection', ...rest'] =>
          let rframe =
            switch (d_backpack) {
            | Left => (
                prefix,
                [Restructuring.Selection(selection), ...suffix],
              )
            | Right => ([Selection(selection), ...prefix], suffix)
            };
          let rframe = fix_holes_rframe(rframe);
          let backpack = (d_backpack, selection', rest');
          Ok((Restructuring((backpack, rframe)), frame));
        };
      } else {
        Error(
          Cant_put_down_selection(
            Restructuring.Backpack.bound_sort(backpack),
            frame_sort,
          ),
        );
      }
    | Move(d) =>
      let+ ((backpack, rframe), frame) =
        move_restructuring(d, restructuring, frame);
      (Subject.Restructuring((backpack, rframe)), frame);
    | Delete(_) =>
      // [Delete]
      let (prefix, suffix) =
        Restructuring.get_sframe(~filter_selections=true, rframe);
      let prefix' = Selection.filter_tiles(frame_sort, prefix);
      let suffix' = Selection.filter_tiles(frame_sort, suffix);
      let fixed = fix_holes(frame_sort, (prefix', suffix'));
      Ok((Pointing(fixed), frame));
    | Construct(_) => Error(Cant_construct_in_restructuring)
    }
  };
};
