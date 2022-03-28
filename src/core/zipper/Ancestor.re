open Util;

[@deriving show]
type step = int;

[@deriving show]
type t = {
  label: Tile.Label.t,
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

module Match = {
  module Prefix = Tile.Match.Make(Orientation.L);
  module Suffix = Tile.Match.Make(Orientation.R);

  type ancestor = t;
  type t = (Prefix.t, Suffix.t);

  let label = ((_, suf)) => Suffix.label(suf);

  let length = ((pre, suf)) => Prefix.length(pre) + Suffix.length(suf);

  let children = ((pre, suf)) => (
    Prefix.children(pre),
    Suffix.children(suf),
  );

  let complete = (m: t): option(ancestor) => {
    let label = label(m);
    length(m) == Tile.Label.length(label)
      ? Some({
          label,
          mold: failwith("todo complete"),
          children: children(m),
        })
      : None;
  };
};
