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

let remold = (a: t): list(t) =>
  Molds.get(a.label) |> List.map(mold => {...a, mold});

let sort = (frame: t): Sort.t =>
  List.nth(frame.mold.sorts.in_, step(frame));

let sort_rank = (a: t, (s_l, s_r): (Sort.t, Sort.t)) => {
  let s = a.mold.sorts.out;
  Bool.to_int(s != s_l) + Bool.to_int(s != s_r);
};

let disassemble = ({children: (l, r), _}: t): Siblings.t => (
  // YOLO(andrew)
  List.flatten(l),
  List.flatten(r),
);

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
          mold: failwith("todo complete ancestor"),
          children: children(m),
        })
      : None;
  };
};
