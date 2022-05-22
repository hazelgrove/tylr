open Util;

[@deriving show]
type t = list(Selection.t);

let empty = [];

module SelMatch = {
  type t = {
    labels: Id.Map.t(Tile.Label.t),
    counts: Id.Map.t(int),
    lt: (Shard.t, Shard.t) => option(bool),
  };

  let mem = (id, m) => Id.Map.mem(id, m.labels);

  let exists_mem = (ss: list(Shard.t), m) =>
    List.exists((s: Shard.t) => mem(s.tile_id, m), ss);

  let is_complete = (m: t) =>
    m.counts
    |> Id.Map.for_all((id, n) =>
         n == List.length(Id.Map.find(id, m.labels))
       );

  let lt_or_un = (ss: list(Shard.t), ss': list(Shard.t), m: t): bool =>
    ss
    |> List.for_all(s =>
         ss'
         |> List.for_all(s' =>
              switch (m.lt(s, s')) {
              | None => true
              | Some(b) => b
              }
            )
       );

  let not_all_un = (ss: list(Shard.t), ss': list(Shard.t), m: t): bool =>
    ss
    |> List.exists(s =>
         ss' |> List.exists(s' => Option.is_some(m.lt(s, s')))
       );
};

module SelMatches = {
  type bp = t;
  type t = Id.Uf.store(SelMatch.t);
  include Id.Uf;
  let mk = (_: bp): t => failwith("SelMatches.mk");
};

// let left_to_right: t => list(Selection.t) =
//   List.fold_left(
//     (l2r, (_, sel: Selection.t)) =>
//       switch (sel.focus) {
//       | Left => [sel, ...l2r]
//       | Right => l2r @ [sel]
//       },
//     [],
//   );

// let is_balanced = (bp: t) =>
//   left_to_right(bp)
//   |> List.map((s: Selection.t) => s.content)
//   |> Segment.concat
//   |> Segment.reassemble
//   |> Segment.is_balanced;

let push = sel => Selection.is_empty(sel) ? Fun.id : List.cons(sel);

let push_s: (list(Selection.t), t) => t = List.fold_right(push);

// let pop = (ids: list(Id.t), bp: t): option((Selection.t, t)) =>
//   switch (Util.ListUtil.split_first_opt(bp)) {
//   | Some(((ids', sel), bp)) when Selection.is_balanced(sel) || ids == ids' =>
//     Some((sel, bp))
//   | _ => None
//   };

// let labels = (bp: t) => Id.Map.t(Tile.Label.t) =>
//   Id.Map.empty
//   |> List.fold_right(
//     ((_, sel: Selection.t)) =>
//       List.fold_right(

//         Segment.shards(sel.content),
//       )
//     bp,
//   );

// Shards are considered selection-matching if they are related transitively
// by normal within-tile matching or by same-selection-containment.
// Returns a map whose keys are tile ids of all shards selection-matching
// those in the head selection of the backpack,
// values are their tile labels and shard counts in the backpack.
let selection_matching =
    (hd: Selection.t, tl: t): Id.Map.t((Tile.Label.t, int)) => {
  let init_shards: (list(Shard.t), Id.Map.t(_)) => Id.Map.t(_) =
    List.fold_right((s: Shard.t) =>
      Id.Map.add(s.tile_id, (Shard.tile_label(s), 0))
    );
  let count_shards: (list(Shard.t), Id.Map.t(_)) => Id.Map.t(_) =
    List.fold_right((s: Shard.t) =>
      Id.Map.update(
        s.tile_id,
        fun
        | None => Some((Shard.tile_label(s), 1))
        | Some((lbl, n)) => Some((lbl, n + 1)),
      )
    );
  // initialize map indexed with tile ids of shards
  // selection-matching shards in hd
  let init_map =
    Id.Map.empty
    |> init_shards(Segment.shards(hd.content))
    |> List.fold_right(
         (sel: Selection.t, map) => {
           let ss = Segment.shards(sel.content);
           List.exists((s: Shard.t) => Id.Map.mem(s.tile_id, map), ss)
             ? init_shards(ss, map) : map;
         },
         tl,
       );
  // count selection-matching shards in total backpack
  init_map
  |> List.fold_right(
       (sel: Selection.t) => count_shards(Segment.shards(sel.content)),
       [hd, ...tl],
     );
};

let has_selection_matching =
    (map: Id.Map.t((Tile.Label.t, int)), (pre, suf): ListFrame.t(Shard.t)) => {
  let has_matching = List.exists((s: Shard.t) => Id.Map.mem(s.tile_id, map));
  has_matching(pre) || has_matching(suf);
};

let valid_order = (sel: Selection.t, (pre, suf): ListFrame.t(Shard.t)) =>
  Segment.shards(sel.content)
  |> List.for_all((s: Shard.t) => {
       let after_pre =
         pre
         |> List.for_all((s': Shard.t) =>
              s'.tile_id != s.tile_id || Shard.index(s') < Shard.index(s)
            );
       let before_suf =
         suf
         |> List.for_all((s': Shard.t) =>
              s'.tile_id != s.tile_id || Shard.index(s') > Shard.index(s)
            );
       after_pre && before_suf;
     });

let pop =
    ((pre, suf): ListFrame.t(Shard.t), bp: t): option((Selection.t, t)) => {
  open OptUtil.Syntax;
  let* (hd, tl) = ListUtil.split_first_opt(bp);
  let ok = Some((hd, tl));
  switch (Segment.shards(hd.content)) {
  | [] => ok
  | [s, ..._] as ss =>
    let match = SelMatches.(get(s.tile_id, mk(bp)));
    SelMatch.(
      is_complete(match)
      || (not_all_un(pre, ss, match) || not_all_un(ss, suf, match))
      && lt_or_un(pre, ss, match)
      && lt_or_un(ss, suf, match)
    )
      ? ok : None;
  };
};

let remove_matching = (ss: list(Shard.t), bp: t) =>
  List.fold_left(
    (bp, s: Shard.t) =>
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
