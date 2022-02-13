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
