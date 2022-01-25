module Subject = {
  [@deriving sexp]
  type t = {
    focus: Util.Direction.t,
    selection: Segment.t,
    affixes: Segment.Frame.t,
  };
};

module Frame = {
  type level = (Tile.Frame.t, Tiles.Frame.t);
  type t = list(level);

  let sort = _ => failwith("todo Zipper.Frame.sort");
};

type t = {
  subject: Subject.t,
  frame: Frame.t,
};
