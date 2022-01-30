open Sexplib.Std;

module Selection = {
  [@deriving sexp]
  type t = {
    focus: Util.Direction.t,
    content: Segment.t,
  };

  let map_content = (f, selection) => {
    ...selection,
    content: f(selection.content),
  };

  let toggle_focus = selection => {
    ...selection,
    focus: Util.Direction.toggle(selection.focus),
  };

  let is_balanced = _ => failwith("todo Selection.is_balanced");

  let is_empty = (selection: t) => selection.content == Segment.empty;

  let clear = (selection: t) => {...selection, content: Segment.empty};

  let push = (p: Segment.Piece.t, {focus, content} as selection: t): t => {
    let content =
      switch (focus) {
      | Left => Segment.cons(p, content)
      | Right => Segment.snoc(content, p)
      };
    {...selection, content};
  };

  let pop = (_selection: t): option((Segment.Piece.t, t)) =>
    failwith("todo Selection.pop");

  let trim = (_selection: t): (t, Grouts.Frame.t) =>
    failwith("todo Selection.trim");
};

module Backpack = {
  [@deriving sexp]
  type t = list(Segment.t);

  let empty = [];

  // let is_balanced = (segments: t): bool =>
  //   Segment.is_whole_any(
  //     Parser.parse_selection(Right, List.concat(segments)),
  //   );
  let is_balanced = _ => failwith("Backpack.is_balanced");

  let total_segment = (up: t): Segment.t => Segment.concat(up);

  let pick_up =
      (side: Util.Direction.t, segments: list(Segment.t), backpack): t => {
    let segments = List.filter((!=)(Segment.empty), segments);
    switch (side) {
    | Left => segments @ backpack
    | Right => backpack @ segments
    };
  };

  let put_down = (side: Util.Direction.t, up: t): option((Segment.t, t)) =>
    switch (side) {
    | Left =>
      switch (up) {
      | [] => None
      | [popped, ...up] => Some((popped, up))
      }
    | Right =>
      open Util.OptUtil.Syntax;
      let+ (up, popped) = Util.ListUtil.split_last_opt(up);
      (popped, up);
    };
};

module Siblings = {
  include Segment.Frame;
};

module Ancestor = {
  type t = (Tile.Frame.t, Tiles.Frame.t);
};

module Ancestors = {
  type t = list(Ancestor.t);

  let empty = [];

  let sort: t => Sort.t =
    fun
    | [] => Sort.Exp
    | [(tile_frame, _), ..._] => Tile.Frame.sort(tile_frame);
};

type t = {
  id_gen: IdGen.t,
  selection: Selection.t,
  backpack: Backpack.t,
  siblings: Siblings.t,
  ancestors: Ancestors.t,
};
