open Sexplib.Std;

[@deriving sexp]
type selem_step = int;
[@deriving sexp]
type caret_step = int;

[@deriving sexp]
type two_step = (selem_step, ChildStep.t);
[@deriving sexp]
type steps = list(two_step);

[@deriving sexp]
type t = (steps, caret_step);

[@deriving sexp]
type range = (steps, (caret_step, caret_step));

let rec mk_steps = (~steps=[], frame: Frame.t): steps => {
  let mk_pat_s = (child_step, ((prefix, _), frame): Frame_pat.s) =>
    mk_steps(
      ~steps=[(List.length(prefix), child_step), ...steps],
      Pat(frame),
    );
  let mk_exp_s = (child_step, ((prefix, _), frame): Frame_exp.s) =>
    mk_steps(
      ~steps=[(List.length(prefix), child_step), ...steps],
      Exp(frame),
    );
  frame
  |> Frame.get(
       fun
       | Frame_pat.Paren_body(s) => mk_pat_s(ChildStep.paren_body, s)
       | Lam_pat(s) => mk_exp_s(ChildStep.lam_pat, s)
       | Let_pat(_, s) => mk_exp_s(ChildStep.let_pat, s),
       fun
       | Frame_exp.Root => steps
       | Paren_body(s) => mk_exp_s(ChildStep.paren_body, s)
       | Ap_arg(s) => mk_exp_s(ChildStep.ap_arg, s)
       | Let_def(_, s) => mk_exp_s(ChildStep.let_def, s)
       | Cond_then(s) => mk_exp_s(ChildStep.cond_then, s),
     );
};

let mk' = ((prefix, _): Restructuring.frame, frame: Frame.t) => (
  mk_steps(frame),
  Restructuring.len(prefix),
);
let mk = ((prefix, _): Selection.frame, frame: Frame.t): t => (
  mk_steps(frame),
  List.length(prefix),
);

let mk_range = ((subject, frame): Zipper.t) =>
  switch (subject) {
  | Pointing(sframe) =>
    let (steps, caret_step) = mk(sframe, frame);
    (steps, (caret_step, caret_step));
  | Selecting(_, selection, sframe) =>
    let (steps, caret_step) = mk(sframe, frame);
    (steps, (caret_step, caret_step + List.length(selection)));
  | Restructuring((_, rframe)) =>
    let (steps, caret_step) = mk'(rframe, frame);
    (steps, (caret_step, caret_step));
  };

let get_child_steps =
  Selem.get(
    _ => [],
    Tile.get(
      fun
      | OpHole
      | Var(_)
      | BinHole
      | Prod => []
      | Paren(_) => [ChildStep.paren_body],
      fun
      | OpHole
      | Num(_)
      | Var(_)
      | Fact
      | BinHole
      | Plus
      | Minus
      | Times
      | Div
      | Prod => []
      | Paren(_) => [ChildStep.paren_body]
      | Ap(_) => [ChildStep.ap_arg]
      | Lam(_) => ChildStep.[lam_pat]
      | Let(_) => ChildStep.[let_pat, let_def]
      | Cond(_) => [ChildStep.cond_then],
    ),
  );
