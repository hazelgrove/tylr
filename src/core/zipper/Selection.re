[@deriving sexp]
type t = {
  focus: Util.Direction.t,
  content: Tiles.t,
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

let is_empty = (selection: t) => selection.content == Tiles.empty;

let clear = (selection: t) => {...selection, content: Tiles.empty};

let push = (tile: Tile.t, {focus, content} as selection: t): t => {
  let content =
    switch (focus) {
    | Left => Tiles.cons(tile, content)
    | Right => Tiles.snoc(content, tile)
    };
  {...selection, content};
};

let pop = (_selection: t): option((Tile.t, t)) =>
  failwith("todo Selection.pop");

// let trim = (_selection: t): (t, Grouts.Frame.t) =>
//   failwith("todo Selection.trim");

let split_piece = _: option((Piece.t, t)) => failwith("todo split_piece");
