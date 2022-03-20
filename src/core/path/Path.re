[@deriving show]
type piece_step = int;
[@deriving show]
type caret_step = int;

[@deriving show]
type two_step = (piece_step, ChildStep.t);
[@deriving show]
type steps = list(two_step);

[@deriving show]
type t = (steps, caret_step);

[@deriving show]
type range = (steps, (caret_step, caret_step));

// let rec mk_steps = (~steps=[], frame: Zipper.Frame.t): steps => {
//   let mk_pat_s = (child_step, ((prefix, _), frame): Frame_pat.s) =>
//     mk_steps(
//       ~steps=[(List.length(prefix), child_step), ...steps],
//       Pat(frame),
//     );
//   let mk_exp_s = (child_step, ((prefix, _), frame): Frame_exp.s) =>
//     mk_steps(
//       ~steps=[(List.length(prefix), child_step), ...steps],
//       Exp(frame),
//     );
//   frame
//   |> Frame.get(
//        Identified.get(
//          fun
//          | Frame_pat.Paren_body(s) => mk_pat_s(ChildStep.paren_body, s)
//          | Lam_pat(_, s) => mk_exp_s(ChildStep.lam_pat, s)
//          | Let_pat(_, s) => mk_exp_s(ChildStep.let_pat, s),
//        ),
//        Identified.get(
//          fun
//          | Frame_exp.Root => steps
//          | Paren_body(s) => mk_exp_s(ChildStep.paren_body, s)
//          | Ap_arg(s) => mk_exp_s(ChildStep.ap_arg, s)
//          | Lam_body(_, s) => mk_exp_s(ChildStep.lam_body, s)
//          | Let_def(_, s) => mk_exp_s(ChildStep.let_def, s)
//          | Cond_then(s) => mk_exp_s(ChildStep.cond_then, s),
//        ),
//      );
// };

// let mk = ((prefix, _): Segment.frame, frame: Frame.t): t => (
//   mk_steps(frame),
//   List.length(prefix),
// );

// let mk_range = ((((selection, affixes), _), frame): Zipper.t) => {
//   let (steps, caret_step) = mk(affixes, frame);
//   (steps, (caret_step, caret_step + List.length(selection)));
// };

// let get_child_steps =
//   Piece.get(
//     _ => [],
//     Tile.get(
//       Identified.get(
//         fun
//         | Tile_pat.OpHole
//         | Var(_)
//         | BinHole
//         | Prod => []
//         | Paren(_) => [ChildStep.paren_body],
//       ),
//       Identified.get(
//         fun
//         | Tile_exp.OpHole
//         | Num(_)
//         | Var(_)
//         | Fact
//         | BinHole
//         | Plus
//         | Minus
//         | Times
//         | Div
//         | Prod => []
//         | Paren(_) => [ChildStep.paren_body]
//         | Ap(_) => [ChildStep.ap_arg]
//         | Lam(_) => ChildStep.[lam_pat, lam_body]
//         | Let(_) => ChildStep.[let_pat, let_def]
//         | Cond(_) => [ChildStep.cond_then],
//       ),
//     ),
//   );
