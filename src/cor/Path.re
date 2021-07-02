open Sexplib.Std;
open Util;
open OptUtil.Syntax;

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
       | Let_def(_, s) => mk_exp_s(ChildStep.let_def, s),
     );
};

let mk = ((prefix, _): Selection.frame, frame: Frame.t): t => (
  mk_steps(frame),
  List.length(prefix),
);

let mk_range = ((subject, frame): Zipper.t) =>
  switch (subject) {
  | Pointing(sframe)
  | Restructuring(_, sframe) =>
    let (steps, caret_step) = mk(sframe, frame);
    (steps, (caret_step, caret_step));
  | Selecting(selection, sframe) =>
    let (steps, caret_step) = mk(sframe, frame);
    (steps, (caret_step, caret_step + List.length(selection)));
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
      | BinHole
      | Plus
      | Times
      | Prod => []
      | Paren(_) => [ChildStep.paren_body]
      | Lam(_) => [ChildStep.lam_pat]
      | Let(_) => ChildStep.[let_pat, let_def],
    ),
  );

let get_child_steps_of_frame =
  Frame.get(
    fun
    | Frame_pat.Paren_body(_) => Some((ChildStep.paren_body, []))
    | Lam_pat(_) => Some((ChildStep.lam_pat, []))
    | Let_pat(_) => Some((ChildStep.let_pat, [ChildStep.let_def])),
    fun
    | Frame_exp.Paren_body(_) => Some((ChildStep.paren_body, []))
    | Let_def(_) => Some((ChildStep.let_def, [ChildStep.let_pat]))
    | Root => None,
  );

let enter_selem =
    (
      d: Direction.t,
      selem: Selem.t,
      (prefix, suffix): Selection.frame,
      frame: Frame.t,
    )
    : option((Selection.frame, Frame.t)) => {
  let get_pat_s = () => {
    let+ prefix = Selection.get_tiles_pat(prefix)
    and+ suffix = Selection.get_tiles_pat(suffix)
    and+ frame = Frame.get_pat(frame);
    ((prefix, suffix), frame);
  };
  let get_exp_s = () => {
    let+ prefix = Selection.get_tiles_exp(prefix)
    and+ suffix = Selection.get_tiles_exp(suffix)
    and+ frame = Frame.get_exp(frame);
    ((prefix, suffix), frame);
  };
  let mk_sframe_of_pat = ts => {
    let affix = Selection.of_tiles_pat(ts);
    d == Left ? (List.rev(affix), []) : ([], affix);
  };
  let mk_sframe_of_exp = ts => {
    let affix = Selection.of_tiles_exp(ts);
    d == Left ? (List.rev(affix), []) : ([], affix);
  };
  selem
  |> Selem.get(
       _ => None,
       Tile.get(
         fun
         | Tile_pat.OpHole
         | Var(_)
         | BinHole
         | Prod => None
         | Paren(body) => {
             let sframe = {
               let affix = Selection.of_tiles_pat(body);
               d == Left ? (List.rev(affix), []) : ([], affix);
             };
             let+ s = get_pat_s();
             (sframe, Frame.Pat(Paren_body(s)));
           },
         fun
         | Tile_exp.OpHole
         | Num(_)
         | Var(_)
         | BinHole
         | Plus
         | Times
         | Prod => None
         | Paren(body) => {
             let+ s = get_exp_s();
             let sframe = mk_sframe_of_exp(body);
             (sframe, Frame.Exp(Paren_body(s)));
           }
         | Lam(p) => {
             let+ s = get_exp_s();
             let sframe = mk_sframe_of_pat(p);
             (sframe, Frame.Exp(Paren_body(s)));
           }
         | Let(p, def) => {
             let+ s = get_exp_s();
             switch (d) {
             | Left =>
               let sframe = mk_sframe_of_exp(def);
               (sframe, Frame.Exp(Let_def(p, s)));
             | Right =>
               let sframe = mk_sframe_of_pat(p);
               (sframe, Frame.Pat(Let_pat(def, s)));
             };
           },
       ),
     );
};

let enter_frame =
    (d: Direction.t, frame: Frame.t): option((Selection.frame, Frame.t)) => {
  let enter_pat_s =
      (tile: Tile_pat.t, ((prefix, suffix), frame): Frame_pat.s) => {
    let tframe =
      d == Left
        ? (prefix, [tile, ...suffix]) : ([tile, ...prefix], suffix);
    Some((
      TupleUtil.map2(Selection.of_tiles_pat, tframe),
      Frame.Pat(frame),
    ));
  };
  let enter_exp_s =
      (tile: Tile_exp.t, ((prefix, suffix), frame): Frame_exp.s) => {
    let tframe =
      d == Left
        ? (prefix, [tile, ...suffix]) : ([tile, ...prefix], suffix);
    Some((
      TupleUtil.map2(Selection.of_tiles_exp, tframe),
      Frame.Exp(frame),
    ));
  };
  frame
  |> Frame.get(
       fun
       | Frame_pat.Paren_body(s) => enter_pat_s(Paren([OpHole]), s)
       | Lam_pat(s) => enter_exp_s(Lam([OpHole]), s)
       | Let_pat(def, s) =>
         switch (d) {
         | Left => enter_exp_s(Let([OpHole], def), s)
         | Right =>
           Some((
             ([], Selection.of_tiles_exp(def)),
             Exp(Let_def([OpHole], s)),
           ))
         },
       fun
       | Frame_exp.Root => None
       | Paren_body(s) => enter_exp_s(Paren([OpHole]), s)
       | Let_def(p, s) =>
         switch (d) {
         | Left =>
           Some((
             (List.rev(Selection.of_tiles_pat(p)), []),
             Pat(Let_pat([OpHole], s)),
           ))
         | Right => enter_exp_s(Let(p, [OpHole]), s)
         },
     );
};

/**
 * A "lossy" move in the sense that, when the caret exits
 * a subject and enters the frame, the subject is replaced
 * with a hole.
 * (TODO motivation for lossy)
 * The path computed from the move result is
 * guaranteed to be the same path w.r.t. the original zipper
 * as if the move had not been lossy.
 *
 * TODO move out of Path?
 */
let move_zipper =
    (d: Direction.t, sframe: Selection.frame, frame: Frame.t)
    : option((Selection.frame, Frame.t)) => {
  let (toward, away) = ListFrame.orient(d, sframe);
  switch (toward) {
  | [] => enter_frame(d, frame)
  | [selem, ...toward] =>
    let sframe = ListFrame.unorient(d, (toward, away));
    switch (enter_selem(d, selem, sframe, frame)) {
    | Some(r) => Some(r)
    | None =>
      let sframe = ListFrame.unorient(d, (toward, [selem, ...away]));
      Some((sframe, frame));
    };
  };
};
