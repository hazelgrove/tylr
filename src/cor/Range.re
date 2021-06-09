type t = (Path.steps, (Path.caret_step, Path.caret_step));

let empty = ((steps, caret_step)) => (steps, (caret_step, caret_step));

let mk = ((steps, caret_step), len) => (
  steps,
  (caret_step, caret_step + len),
);
