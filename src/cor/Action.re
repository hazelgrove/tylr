open Util;
open OptUtil.Syntax;

[@deriving sexp]
type t =
  | Mark
  | Move(Direction.t)
  | Delete
  | Construct(Tile.t);

let rec perform = (a: t, (subject, frame): Zipper.t): option(Zipper.t) =>
  switch (subject) {
  | Pointing((prefix, suffix) as sframe) =>
    switch (a) {
    | Mark =>
      // [MarkPointing]
      Some((Selecting([], sframe), frame))
    | Move(Left) =>
      switch (prefix) {
      | [] =>
        // [MoveLeftFrame]
        let* ((prefix', suffix'), frame') = Parser.disassemble_frame(frame);
        switch (prefix') {
        | [] => None
        | [_, ..._] =>
          let subject' = Subject.Pointing((prefix', suffix @ suffix'));
          perform(a, (subject', frame'));
        };
      | [selem, ...prefix] =>
        switch (Parser.disassemble_selem(selem)) {
        | [] =>
          // [MoveLeftAtomic]
          let suffix = Parser.parse_selection(Right, [selem, ...suffix]);
          let (sframe, frame) =
            Parser.parse_zipper((prefix, suffix), frame);
          Some((Pointing(sframe), frame));
        | [_, ..._] as disassembled =>
          // [MoveLeftDisassembles]
          let subject =
            Subject.Pointing((List.rev(disassembled) @ prefix, suffix));
          perform(a, (subject, frame));
        }
      }
    | Move(Right) =>
      switch (suffix) {
      | [] =>
        // [MoveRightFrame]
        let* ((prefix', suffix'), frame') = Parser.disassemble_frame(frame);
        switch (suffix') {
        | [] => None
        | [_, ..._] =>
          let subject' = Subject.Pointing((prefix @ prefix', suffix'));
          perform(a, (subject', frame'));
        };
      | [selem, ...suffix] =>
        switch (Parser.disassemble_selem(selem)) {
        | [] =>
          // [MoveRightAtomic]
          let prefix = Parser.parse_selection(Left, [selem, ...prefix]);
          let (sframe, frame) =
            Parser.parse_zipper((prefix, suffix), frame);
          Some((Pointing(sframe), frame));
        | [_, ..._] as disassembled =>
          // [MoveRightDisassembles]
          let subject = Subject.Pointing((prefix, disassembled @ suffix));
          perform(a, (subject, frame));
        }
      }
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
  | Selecting(selection, (prefix, suffix) as sframe) =>
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
    | Move(Left) =>
      switch (prefix) {
      | [] =>
        // [SelectLeftFrame]
        let* (sframe', frame') = Parser.disassemble_frame(frame);
        let subject =
          Subject.Selecting(selection, ListFrame.append(sframe, sframe'));
        perform(a, (subject, frame'));
      | [selem, ...prefix] =>
        switch (Parser.disassemble_selem(selem)) {
        | [] =>
          // [SelectLeftAtomic]
          let selection' =
            Parser.parse_selection(Right, [selem, ...selection]);
          let subject = Subject.Selecting(selection', (prefix, suffix));
          Some((subject, frame));
        | [_, ..._] as disassembled =>
          // [SelectLeftDisassembles]
          let subject =
            Subject.Selecting(selection, (disassembled @ prefix, suffix));
          perform(a, (subject, frame));
        }
      }
    | Move(Right) =>
      switch (selection) {
      | [] =>
        // disallowing left-to-right selections for simplicity
        None
      | [selem, ...selection] =>
        switch (Parser.disassemble_selem(selem)) {
        | [] =>
          // [SelectRightAtomic]
          let (sframe', frame') =
            Parser.parse_zipper(([selem, ...prefix], suffix), frame);
          let subject = Subject.Selecting(selection, sframe');
          Some((subject, frame'));
        | [_, ..._] as disassembled =>
          // [SelectRightDisassembles]
          let subject = Subject.Selecting(disassembled @ selection, sframe);
          perform(a, (subject, frame));
        }
      }
    }
  | Restructuring(selection, (prefix, suffix) as sframe) =>
    switch (a) {
    | Mark =>
      if (Selection.is_partial(selection)
          || Selection.is_whole(Frame.sort(frame), selection)) {
        // [MarkRestructuring]
        let tip = Tip.(Convex, Frame.sort(frame));
        let (sframe, frame) =
          Parser.parse_zipper(
            Parser.fix_holes(tip, (prefix, selection @ suffix), tip),
            frame,
          );
        Some((Pointing(sframe), frame));
      } else {
        None;
      }
    | Move(Left) => failwith("todo")
    | Move(Right) =>
      if (Selection.is_whole_any(selection)) {
        // [MoveRightRestructuringWhole]
        switch (suffix) {
        | []
        | [Token(_), ..._] => None
        | [Tile(_) as selem, ...suffix] =>
          let subject =
            Subject.Restructuring(selection, ([selem, ...prefix], suffix));
          Some((subject, frame));
        };
      } else {
        switch (perform(a, (Pointing(sframe), frame))) {
        | Some((Pointing(sframe'), frame')) =>
          Some((Restructuring(selection, sframe'), frame'))
        | _ => None
        };
      }
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
