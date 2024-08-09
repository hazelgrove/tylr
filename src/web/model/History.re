open Tylr_core;

// type t = {
//   succeeded: (list((Zipper.Action.t, Zipper.state)) as 'affix, 'affix),
//   just_failed: option(FailedInput.t),
//   // TODO(d): forgetting why we need this...
//   // not seeing it get read anywhere. possibly
//   // to trigger view redraw? review blame
//   last_attempt: option(float),
// };
type t = (Chain.Affix.t(Edit.t, Zipper.t) as 'affix, 'affix);

// let empty = {succeeded: ([], []), just_failed: None, last_attempt: None};
let empty = Chain.Affix.(empty, empty);

// not sure why warning flag is needed
[@warning "-32"]
let can_undo = ((before, _): t) => Chain.Affix.is_empty(before);
[@warning "-32"]
let can_undo = ((_, after): t) => Chain.Affix.is_empty(after);

// let clear_just_failed = history => {...history, just_failed: None};

// let just_failed = (reason: FailedInput.reason, history: t) => {
//   let last_attempt = Some(JsUtil.timestamp());
//   let just_failed =
//     Some(
//       switch (history.just_failed) {
//       | None => FailedInput.mk(reason)
//       | Some(failed_input) =>
//         FailedInput.replace_or_increment_attempts(reason, failed_input)
//       },
//     );
//   {...history, last_attempt, just_failed};
// };
// let unrecognized_input = just_failed(Unrecognized);
// let failure = f => just_failed(Failure(f));

// let succeeded = (a: Zipper.Action.t, zipper: Zipper.state, history: t) => {
//   let (before, _) = history.succeeded;
//   {
//     succeeded: ([(a, zipper), ...before], []),
//     just_failed: None,
//     last_attempt: Some(JsUtil.timestamp()),
//   };
// };
let do_ = (a: Edit.t, z: Zipper.t, (pre, _): t) =>
  Chain.Affix.(link(a, z, pre), empty);

// let escaped = (history: t) => {
//   ...history,
//   just_failed: None,
//   last_attempt: Some(JsUtil.timestamp()),
// };

let undo = (z: Zipper.t, (before, after): t): option((Zipper.t, t)) =>
  Chain.Affix.unlink(before)
  |> Option.map(((a, prev, before)) =>
       (prev, (before, Chain.Affix.link(a, z, after)))
     );

let redo = (z: Zipper.t, (before, after): t): option((Zipper.t, t)) =>
  Chain.Affix.unlink(after)
  |> Option.map(((a, next, after)) =>
       (next, (Chain.Affix.link(a, z, before), after))
     );
