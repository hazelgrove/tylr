open Util;

[@deriving show]
type step = int;

[@deriving show]
type t = {
  label: Label.t,
  mold: Mold.t,
  children: ListFrame.t(Segment.t),
};

// TODO flatten with shard indices
let step = (frame: t): step => {
  let (prefix, _) = frame.children;
  List.length(prefix);
};

let sort = (frame: t): Sort.t =>
  List.nth(frame.mold.sorts.in_, step(frame));
// let disassemble = (frame: t): (s, s) =>
//   switch (frame) {
//   | Placeholder({substance: (prefix, suffix)}) =>
//   }

// [@deriving show]
// type t = (Tile.Frame.t, Siblings.t);

let disassemble = (_: t): Siblings.t => failwith("todo disassemble");
