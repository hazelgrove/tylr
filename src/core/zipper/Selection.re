open Util;

[@deriving sexp]
type t = {
  focus: Direction.t,
  content: Tiles.t,
};

let mk = (focus, content) => {focus, content};

let empty = mk(Left, Tiles.empty);

let map = (f, sel) => {...sel, content: f(sel.content)};

let toggle_focus = selection => {
  ...selection,
  focus: Util.Direction.toggle(selection.focus),
};

let is_balanced = _ => failwith("todo Selection.is_balanced");

let is_empty = (selection: t) => selection.content == Tiles.empty;

let clear = (selection: t) => {...selection, content: Tiles.empty};

let push = (p: Piece.t, {focus, content}: t): t => {
  let tile = Tile.of_piece(p);
  let content =
    Tiles.reassemble(
      switch (focus) {
      | Left => Tiles.cons(tile, content)
      | Right => Tiles.snoc(content, tile)
      },
    );
  {focus, content};
};

let pop = (sel: t): option((Piece.t, t)) =>
  switch (sel.focus, sel.content, ListUtil.split_last_opt(sel.content)) {
  | (_, [], _)
  | (_, _, None) => None
  | (Left, [_, ..._], _)
  | (Right, _, Some((_, _))) => failwith("todo pop")
  };

// let trim = (_selection: t): (t, Grouts.Frame.t) =>
//   failwith("todo Selection.trim");

let split_piece = _: option((Piece.t, t)) => failwith("todo split_piece");
