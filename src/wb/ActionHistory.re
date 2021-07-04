open Util;
open Cor;

type timestamp = float;

type t = {
  succeeded: AltList.a_frame(Zipper.t, Action.t),
  just_failed: option(Action.t),
  last_attempt: option(timestamp),
};

let empty = {succeeded: ([], []), just_failed: None, last_attempt: None};

let failed = (a: Action.t, history: t) => {
  let now = JsUtil.date_now()##valueOf;
  {...history, just_failed: Some(a), last_attempt: Some(now)};
};

let succeeded = (a: Action.t, zipper: Zipper.t, history: t) => {
  let (before, _) = history.succeeded;
  {
    succeeded: ([(a, zipper), ...before], []),
    just_failed: None,
    last_attempt: Some(JsUtil.date_now()##valueOf),
  };
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
    Some((prev', {...history, succeeded}));
  | ([(a, prev), ...before], after) =>
    let succeeded = (before, [(a, zipper), ...after]);
    Some((prev, {...history, succeeded}));
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
    Some((next', {...history, succeeded}));
  | (before, [(a, prev), ...after]) =>
    let succeeded = ([(a, zipper), ...before], after);
    Some((prev, {...history, succeeded}));
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
