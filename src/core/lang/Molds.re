open Util;

let forms_assoc: list((Label.t, list(Mold.t))) =
  List.fold_left(
    (acc, (_, {label, mold, _}: Form.t)) => {
      let molds =
        switch (List.assoc_opt(label, acc)) {
        | Some(old_molds) => old_molds @ [mold]
        | None => [mold]
        };
      List.cons((label, molds), List.remove_assoc(label, acc));
    },
    [],
    Form.forms,
  );

let convex_mono_molds = (label: list(Token.t)): option(list(Mold.t)) =>
  // TODO(andrew): cleanup
  switch (label) {
  | [t] =>
    switch (Form.convex_mono_molds(t)) {
    | [] => None
    | ms => Some(ms)
    }
  | _ => None
  };

let get = (label: Label.t): list(Mold.t) =>
  switch (convex_mono_molds(label), List.assoc_opt(label, forms_assoc)) {
  | (Some(molds), _) => molds
  | (_, Some(molds)) => molds
  | (_, None) => []
  };

[@deriving show]
type completes = list((string, (list(string), Direction.t)));

let delayed_completes: completes =
  List.filter_map(
    ((_, {expansion, label, _}: Form.t)) =>
      switch (expansion, label) {
      | ((Delayed, Delayed), [hd, ..._]) =>
        Some([
          (hd, (label, Direction.Left)),
          (ListUtil.last(label), (label, Right)),
        ])
      | ((Delayed, _), [hd, ..._]) => Some([(hd, (label, Left))])
      | ((_, Delayed), [_, ..._]) =>
        Some([(ListUtil.last(label), (label, Right))])
      | _ => None
      },
    Form.forms,
  )
  |> List.flatten
  |> List.sort_uniq(compare);

let instant_completes: completes =
  List.filter_map(
    ((_, {expansion, label, _}: Form.t)) =>
      switch (expansion, label) {
      | ((Instant, Instant), [hd, ..._]) =>
        Some([
          (hd, (label, Direction.Left)),
          (ListUtil.last(label), (label, Right)),
        ])
      | ((Instant, _), [hd, ..._]) => Some([(hd, (label, Left))])
      | ((_, Instant), [_, ..._]) =>
        Some([(ListUtil.last(label), (label, Right))])
      | _ => None
      },
    Form.forms,
  )
  |> List.flatten
  |> List.sort_uniq(compare);

let delayed_completion: (string, Direction.t) => (list(Token.t), Direction.t) =
  (s, direction_preference) =>
    /* Completions which must be defered as they are ambiguous prefixes */
    switch (List.assoc_opt(s, delayed_completes)) {
    | Some(completion) => completion
    | None => ([s], direction_preference)
    };

let instant_completion: (string, Direction.t) => (list(Token.t), Direction.t) =
  (s, direction_preference) =>
    /* Completions which can or must be executed immediately */
    switch (List.assoc_opt(s, instant_completes)) {
    | Some(completion) => completion
    | None => ([s], direction_preference)
    };
