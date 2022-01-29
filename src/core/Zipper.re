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

  let sort: t => Sort.t =
    fun
    | [] => Sort.Exp
    | [(tile_frame, _), ..._] => Tile.Frame.sort(tile_frame);
};

type t = {
  subject: Subject.t,
  frame: Frame.t,
};
