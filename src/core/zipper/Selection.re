open Util;

[@deriving show]
type t = {
  focus: Direction.t,
  content: Segment.t,
};

let mk = (focus, content) => {focus, content};

let empty = mk(Left, Segment.empty);

let map = (f, sel) => {...sel, content: f(sel.content)};

let toggle_focus = selection => {
  ...selection,
  focus: Util.Direction.toggle(selection.focus),
};

// let is_balanced = sel => Segment.is_balanced(sel.content);

let is_empty = (selection: t) => selection.content == Segment.empty;

let clear = (selection: t) => {...selection, content: Segment.empty};

let push = (p: Piece.t, {focus, content}: t): t => {
  let content =
    Segment.reassemble(
      switch (focus) {
      | Left => Segment.cons(p, content)
      | Right => Segment.snoc(content, p)
      },
    );
  {focus, content};
};

let pop = (sel: t): option((Piece.t, t)) =>
  switch (sel.focus, sel.content, ListUtil.split_last_opt(sel.content)) {
  | (_, [], _)
  | (_, _, None) => None
  | (Left, [p, ...content], _) =>
    let (p, suffix) = Piece.pop(Left, p);
    Some((p, {...sel, content: suffix @ content}));
  | (Right, _, Some((content, p))) =>
    let (p, prefix) = Piece.pop(Right, p);
    Some((p, {...sel, content: content @ List.rev(prefix)}));
  };

let split_piece = _: option((Piece.t, t)) => failwith("todo split_piece");
