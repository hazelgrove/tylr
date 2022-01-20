module Subject = {
  [@deriving sexp]
  type t = {
    focus: Direction.t,
    selection: Segment.t,
    affixes: Segment.Frame.t,
  };
};

module Frame = {
  type t = list((Tile.Frame.t, Tile.Frame.s));
};

type t = {
  subject: Subject.t,
  frame: Frame.t,
};
