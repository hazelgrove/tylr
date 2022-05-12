[@deriving show]
type t = list(Selection.t);

let empty = [];

let left_to_right: t => list(Selection.t) =
  List.fold_left(
    (l2r, sel: Selection.t) =>
      switch (sel.focus) {
      | Left => [sel, ...l2r]
      | Right => l2r @ [sel]
      },
    [],
  );

let is_balanced = (bp: t) =>
  left_to_right(bp)
  |> List.map((s: Selection.t) => s.content)
  |> Segment.concat
  |> Segment.reassemble
  |> Segment.is_balanced;

let push = sel => Selection.is_empty(sel) ? Fun.id : List.cons(sel);

let push_s: (list(Selection.t), t) => t = List.fold_right(push);

let pop = Util.ListUtil.split_first_opt;

let remove_matching = (ss: list(Shard.t), bp: t) =>
  List.fold_left(
    (bp, s) =>
      bp
      |> List.map(Selection.map(Segment.remove_matching(s)))
      |> List.filter_map(
           fun
           | Selection.{content: [], _} => None
           | sel => Some(sel),
         ),
    bp,
    ss,
  );

let is_first_matching = (t: Token.t, bp: t): bool =>
  /* Does the first selection in the backpack consist
     of a single token which matches the one provided? */
  switch (bp) {
  | [] => false
  | [{content: [p], _}, ..._] =>
    switch (p) {
    | Tile({label: [s], _}) => s == t
    | Shard({label: (n, label), _}) =>
      assert(n < List.length(label));
      List.nth(label, n) == t;
    | _ => false
    }
  | _ => false
  };
