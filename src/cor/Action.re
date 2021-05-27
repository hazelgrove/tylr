open Util;
open OptUtil.Syntax;

type t =
  | Mark
  | Move(Direction.t)
  | Delete
  | Construct(Tile.t);

let rec perform = (a: t, (subject, frame): Zipper.t): option(Zipper.t) =>
  switch (subject) {
  | Pointing((prefix, suffix) as sframe) =>
    switch (a) {
    | Mark => Some((Selecting([], sframe), frame))
    | Move(Left) => failwith("todo")
    | Move(Right) =>
      switch (suffix) {
      | [] =>
        // [MoveRightFrame]
        let ((prefix', suffix'), frame') = Parser.disassemble_frame(frame);
        switch (suffix') {
        | [] => None
        | [_, ..._] =>
          let subject' = Subject.Pointing((prefix' @ prefix, suffix'));
          perform(a, (subject', frame'));
        };
      | [selem, ...suffix] =>
        switch (Parser.disassemble_selem(selem)) {
        | [] =>
          // [MoveRightAtomic]
          let subject = Subject.Pointing((prefix @ [selem], suffix));
          Some(Parser.parse_zipper((subject, frame)));
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
        let sframe = Parser.fix_holes(tip, sframe, tip);
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
        let (sframe', frame') = Parser.disassemble_frame(frame);
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
            Parser.parse_zipper((([selem, ...prefix], suffix), frame));
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
      switch (Selection.is_whole(selection)) {
      | Some(tiles) when Tiles.sort(tiles) != Frame.sort(frame) => None
      | _ =>
        // [MarkRestructuring]
        let tip = Tip.(Convex, Frame.sort(frame));
        let subject =
          Subject.Pointing(
            Parser.fix_holes(tip, (prefix, selection @ suffix), tip),
          );
        Some(Parser.parse_zipper((subject, frame)));
      }
    | Move(Left) => failwith("todo")
    | Move(Right) =>
      switch (Selection.is_whole(selection)) {
      | None =>
        // [MoveRightRestructuringWhole]
        switch (suffix) {
        | []
        | [Token(_), ..._] => None
        | [Tile(_) as selem, ...suffix] =>
          let subject =
            Subject.Restructuring(selection, ([selem, ...prefix], suffix));
          Some((subject, frame));
        }
      | Some(_) =>
        switch (perform(a, (Pointing(sframe), frame))) {
        | Some((Pointing(sframe'), frame')) =>
          Some((Restructuring(selection, sframe'), frame'))
        | _ => None
        }
      }
    | Delete =>
      let s = Frame.sort(frame);
      let tip = Tip.(Convex, s);
      let prefix' = Selection.filter_tiles(s, prefix);
      let suffix' = Selection.filter_tiles(s, suffix);
      let fixed = Parser.fix_holes(tip, (prefix', suffix'), tip);
      Some((Pointing(fixed), frame));
    | Construct(_) => None
    }
  };
