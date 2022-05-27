open Util;

module ShardInfo = {
  module Order = {
    type key = (Id.t, int);
    let key = (s: Shard.t) => (s.tile_id, Shard.index(s));

    open Hashtbl;
    type t = Hashtbl.t(key, Hashtbl.t(key, unit));

    let n = 20;
    let init = () => create(n);

    let lt = (l: Shard.t, r: Shard.t, ord: t): bool =>
      switch (find_opt(ord, key(l))) {
      | None => false
      | Some(row) => Option.is_some(find_opt(row, key(r)))
      };
    let gt = (l, r, ord) => lt(r, l, ord);
    let un = (l, r, ord) => !lt(l, r, ord) && !gt(l, r, ord);

    let lt_or_un = (ls, rs, ord) =>
      ls
      |> List.for_all(l =>
           rs |> List.for_all(r => lt(l, r, ord) || un(l, r, ord))
         );

    let get = (i, j, m) => {
      open OptUtil.Syntax;
      let* r = find_opt(m, i);
      find_opt(r, j);
    };

    let set = (i, j, m) => {
      let r =
        switch (find_opt(m, i)) {
        | None => create(n)
        | Some(r) => r
        };
      replace(r, j, ());
      replace(m, i, r);
    };

    let add_tile = (id, lbl, ord) =>
      lbl
      |> List.iteri((i, _) => {
           switch (find_opt(ord, (id, i))) {
           | Some(_) => ()
           | None => replace(ord, (id, i), create(n))
           };
           i == 0 ? () : set((id, i - 1), (id, i), ord);
         });

    // Warshall's algorithm https://cs.winona.edu/lin/cs440/ch08-2.pdf
    let tran_close = (ord: t): unit => {
      let keys = List.of_seq(Hashtbl.to_seq_keys(ord));
      keys
      |> List.iteri((n, k) =>
           if (n == 0) {
             ();
           } else {
             keys
             |> List.iter(i =>
                  keys
                  |> List.iter(j =>
                       switch (get(i, j, ord)) {
                       | Some(_) => ()
                       | None =>
                         switch (get(i, k, ord), get(k, j, ord)) {
                         | (None, _)
                         | (_, None) => ()
                         | (Some(_), Some(_)) => set(i, j, ord)
                         }
                       }
                     )
                );
           }
         );
    };
  };

  module Count = {
    type t = {
      labels: Id.Map.t(Tile.Label.t),
      counts: Id.Map.t(int),
    };

    let of_shard = (s: Shard.t) => {
      labels: Id.Map.singleton(s.tile_id, Shard.tile_label(s)),
      counts: Id.Map.singleton(s.tile_id, 1),
    };

    let merge = (m: t, m': t) => {
      labels: Id.Map.union((_, lbl, _) => Some(lbl), m.labels, m'.labels),
      counts: Id.Map.union((_, n, n') => Some(n + n'), m.counts, m'.counts),
    };

    let mem = (id, m) => Id.Map.mem(id, m.labels);

    let exists_mem = (ss: list(Shard.t), m) =>
      List.exists((s: Shard.t) => mem(s.tile_id, m), ss);

    let is_complete = (m: t) =>
      m.counts
      |> Id.Map.for_all((id, n) =>
           n == List.length(Id.Map.find(id, m.labels))
         );
  };

  module Counts = {
    type t = Id.Uf.store(Count.t);
    include Id.Uf;
    let merge = merge(Count.merge);
    let add_shard = (s: Shard.t, cs: t): unit =>
      switch (get_opt(s.tile_id, cs)) {
      | None => add(s.tile_id, Count.of_shard(s), cs)
      | Some(c) =>
        let c = {
          ...c,
          counts: Id.Map.update(s.tile_id, Option.map((+)(1)), c.counts),
        };
        set(s.tile_id, c, cs);
      };
  };

  // Shards are considered selection-matching if they are related transitively
  // by normal within-tile matching or by same-selection-containment.
  // Selection-matching shards are ordered transitively by within-tile
  // ordering and same-selection ordering combined with well-nestedness.
  // Represents the expected order of all selection-matching shards as
  // imposed by selections in the backpack, along with counts of those
  // shards contained in the backpack.
  // Counts are partitioned by the selection-matching relation.
  type t = {
    order: Order.t,
    counts: Counts.t,
  };

  let init = () => {order: Order.init(), counts: Counts.init()};

  let add_sel = (sel: Selection.t, {counts, order}: t): unit => {
    let ss = Segment.shards(sel.content);
    // initialize
    ss
    |> List.iter((s: Shard.t) => {
         Counts.add_shard(s, counts);
         Order.add_tile(s.tile_id, Shard.tile_label(s), order);
       });
    // merge counts
    ignore(
      ss
      |> List.fold_left(
           (prev: option(Shard.t), curr: Shard.t) => {
             switch (prev) {
             | None => ()
             | Some(prev) => Counts.merge(prev.tile_id, curr.tile_id, counts)
             };
             Some(curr);
           },
           None,
         ),
    );
    // propagate well-nested ordering constraints
    ListUtil.ordered_pairs(ss)
    |> List.iter(((l: Shard.t, r: Shard.t)) => {
         let Count.{labels, _} = Counts.get(l.tile_id, counts);
         let (i_l, i_r) = (Shard.index(l), Shard.index(r));
         let n_l = List.length(Id.Map.find(l.tile_id, labels));
         let n_r = List.length(Id.Map.find(r.tile_id, labels));
         if (i_l == n_l - 1 && i_r != 0) {
           Order.set((r.tile_id, i_r - 1), (l.tile_id, 0), order);
         } else if (i_l != n_l - 1 && i_r == 0) {
           Order.set((r.tile_id, n_r - 1), (l.tile_id, i_l + 1), order);
         };
         Order.(set(key(l), key(r), order));
       });
  };
};

[@deriving show]
type t = list(Selection.t);

let empty = [];

let shard_info = (bp: t) => {
  open ShardInfo;
  let info = init();
  bp |> List.iter(sel => add_sel(sel, info));
  Order.tran_close(info.order);
  info;
};

let push = sel => Selection.is_empty(sel) ? Fun.id : List.cons(sel);

let push_s: (list(Selection.t), t) => t = List.fold_right(push);

let pop =
    ((pre, suf): ListFrame.t(Shard.t), bp: t)
    : option((bool, Selection.t, t)) => {
  open OptUtil.Syntax;
  let* (hd, tl) = ListUtil.split_first_opt(bp);
  switch (Segment.shards(hd.content)) {
  | [] => Some((true, hd, tl))
  | [s, ..._] as ss =>
    open ShardInfo;
    let {counts, order} = shard_info(bp);
    let count = Counts.get(s.tile_id, counts);
    let first = Count.is_complete(count);
    first
    || (Count.exists_mem(pre, count) || Count.exists_mem(suf, count))
    && Order.lt_or_un(pre, ss, order)
    && Order.lt_or_un(ss, suf, order)
      ? Some((first, hd, tl)) : None;
  };
};

let remove_matching = (ts: list(Tile.t), bp: t) =>
  List.fold_left(
    (bp, t: Tile.t) =>
      bp
      |> List.map(Selection.map(Segment.remove_matching(t)))
      |> List.filter_map(
           fun
           | sel when !Selection.is_empty(sel) => Some(sel)
           | _ => None,
         ),
    bp,
    ts,
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
