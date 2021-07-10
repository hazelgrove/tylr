open Util;
open Core;

type timestamp = float;

type t = {
  succeeded: AltList.a_frame(Zipper.t, Action.t),
  just_failed: option(FailedInput.t),
  last_attempt: option(timestamp),
};

let empty = {succeeded: ([], []), just_failed: None, last_attempt: None};

let can_undo = ({succeeded: (prefix, _), _}: t) => prefix != [];
let can_redo = ({succeeded: (_, suffix), _}: t) => suffix != [];

let clear_just_failed = history => {...history, just_failed: None};

let just_failed = (reason: FailedInput.reason, history: t) => {
  let last_attempt = Some(JsUtil.date_now()##valueOf);
  let just_failed =
    Some(
      switch (history.just_failed) {
      | None => FailedInput.mk(reason)
      | Some(failed_input) =>
        FailedInput.replace_or_increment_attempts(reason, failed_input)
      },
    );
  {...history, last_attempt, just_failed};
};
let unrecognized_input = just_failed(Unrecognized);
let failure = f => just_failed(Failure(f));

let succeeded = (a: Action.t, zipper: Zipper.t, history: t) => {
  let (before, _) = history.succeeded;
  {
    succeeded: ([(a, zipper), ...before], []),
    just_failed: None,
    last_attempt: Some(JsUtil.date_now()##valueOf),
  };
};

let escaped = (history: t) => {
  ...history,
  just_failed: None,
  last_attempt: Some(JsUtil.date_now()##valueOf),
};

let undo = (zipper: Zipper.t, history: t): option((Zipper.t, t)) => {
  switch (history.succeeded) {
  | ([], _) => None
  | (
      [
        (a, (Selecting(_), _) as prev),
        (a', (Pointing(_) | Selecting(_, [], _), _) as prev'),
        ...before,
      ],
      after,
    ) =>
    let succeeded = (before, [(a', prev), (a, zipper), ...after]);
    Some((prev', {...history, just_failed: None, succeeded}));
  | ([(a, prev), ...before], after) =>
    let succeeded = (before, [(a, zipper), ...after]);
    Some((prev, {...history, just_failed: None, succeeded}));
  };
};

let redo = (zipper: Zipper.t, history: t): option((Zipper.t, t)) => {
  switch (history.succeeded) {
  | (_, []) => None
  | (
      before,
      [
        (a, (Selecting(_), _) as next),
        (a', (Pointing(_) | Selecting(_, [], _), _) as next'),
        ...after,
      ],
    ) =>
    let succeeded = ([(a', next), (a, zipper), ...before], after);
    Some((next', {...history, just_failed: None, succeeded}));
  | (before, [(a, prev), ...after]) =>
    let succeeded = ([(a, zipper), ...before], after);
    Some((prev, {...history, just_failed: None, succeeded}));
  };
};

let zipper_before_restructuring =
    ({succeeded: (prefix, _), _}: t): option(Zipper.t) => {
  let (_restructuring, before) =
    prefix
    |> ListUtil.take_while(
         fun
         | (_, (Subject.Restructuring(_), _)) => true
         | _ => false,
       );
  switch (before) {
  | [] => None
  | [(_, zipper), ..._] => Some(zipper)
  };
};
