open Util;
open OptUtil.Syntax;

type t =
  | Pointing(Path.t)
  | Selecting(Path.steps, (Path.caret_step, Path.caret_step))
  | Restructuring(Path.t, Selection.t);

let mk = ((subject, frame): Zipper.t): t =>
  switch (subject) {
  | Pointing(sframe) => Pointing(Path.mk(sframe, frame))
  | Selecting(selection, (prefix, suffix)) =>
    let (steps, l) = Path.mk((prefix, selection @ suffix), frame);
    let (_, r) = Path.mk((List.rev(selection) @ prefix, selection), frame);
    Selecting(steps, (l, r));
  | Restructuring(selection, sframe) =>
    Restructuring(Path.mk(sframe, frame), selection)
  };

let neighbors =
    ((subject, frame): Zipper.t): (option(Path.t), option(Path.t)) => {
  let sframe =
    switch (subject) {
    | Pointing(sframe)
    | Restructuring(_, sframe) => sframe
    | Selecting(selection, (prefix, suffix)) => (prefix, selection @ suffix)
    };
  let move = d => {
    let+ (sframe, frame) = Path.move_zipper(d, sframe, frame);
    Path.mk(sframe, frame);
  };
  (move(Left), move(Right));
};
