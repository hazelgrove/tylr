module Down = {
  [@deriving sexp]
  type t = (Direction.t, Segment.t, Segment.Frame.t);
};

// backpack
module Up = {
  [@deriving sexp]
  type t = list(Segment.t);

  let is_balanced = (segments: t): bool =>
    Segment.is_whole_any(
      Parser.parse_selection(Right, List.concat(segments)),
    );

  let total_segment = (up: t): Segment.t => List.concat(up);

  let extend = (side: Direction.t, segment, up): t =>
    switch (side) {
    | Left => [segment, ...up]
    | Right => up @ [segment]
    };

  let pop = (side, up: t): option((Segment.t, Up.t)) =>
    switch (side) {
    | Left =>
      switch (up) {
      | [] => None
      | [popped, ...up] => Some((popped, up))
      }
    | Right =>
      open Util.OptUtil.Syntax;
      let+ (up, popped) = ListUtil.split_last(up);
      (popped, up);
    };
};

[@deriving sexp]
type t = (Down.t, Up.t);
