type s = list(t)
and t = Tile.t(op, pre, post, bin)
and op =
  | OpHole
  | Text(string)
  | Paren(s)
and pre =
  | Lam(s)
  | Let(s, s)
and post =
  | Ap(s)
  | Ann(s)
and bin =
  | BinHole
  | Plus
  | Arrow;

let fix_empty_holes_between = (prefix: s, suffix: s): (s, s) => {
  switch (ListUtil.split_last_opt(prefix), suffix) {
  | (None, _)
  | (_, []) => (prefix, suffix)
  | (Some((leading, last)), [first, ...trailing]) =>
    switch (last, first) {
    | (Op(OpHole), Bin(BinHole))
    | (Bin(BinHole), Op(OpHole)) => (leading, trailing)
    | (Op(OpHole), Op(_) | Pre(_)) => (leading, suffix)
    | (Op(_) | Post(_), Op(OpHole)) => (prefix, trailing)
    | (Op(_) | Post(_), Op(_) | Pre(_)) => (
        prefix,
        [Bin(BinHole), ...suffix],
      )
    | (Bin(BinHole), Bin(_) | Post(_)) => (leading, suffix)
    | (Bin(_) | Pre(_), Bin(BinHole)) => (prefix, trailing)
    | (Bin(_) | Pre(_), Bin(_) | Post(_)) => (
        prefix,
        [Op(OpHole), ...suffix],
      )
    | _ => (prefix, suffix)
    }
  };
};

let rec fix_empty_holes_left = tss =>
  switch (tss) {
  | [] => [] // should never hit this case
  | [[], ...tss] =>
    switch (tss) {
    | [] => [[Tile.Op(OpHole)]]
    | [_, ..._] => [[], ...fix_empty_holes_left(tss)]
    }
  | [[Tile.Bin(BinHole), ...ts], ...tss] => [
      ts,
      ...switch (ts) {
         | [] => fix_empty_holes_left(tss)
         | [_, ..._] => tss
         },
    ]
  | [[t, ..._] as ts, ...tss] =>
    let left_cap =
      switch (t) {
      | Op(_)
      | Pre(_) => []
      | Post(_)
      | Bin(_) => [Tile.Op(OpHole)]
      };
    [left_cap @ ts, ...tss];
  };

let fix_empty_holes_right = tss =>
  tss |> List.rev_map(rev) |> fix_empty_holes_left |> List.rev_map(rev);

let fix_empty_holes = (tss: list(s)): list(s) => {
  let rec fix = (ts: s, tss: list(s)): list(s) => {
    let skip_empty = (ts, tss) => {
      let (ts, tss) = ListUtil.split_first(fix(ts, tss));
      [ts, [], ...tss];
    };
    switch (tss) {
    | [] => [ts]
    | [[], ...tss] => skip_empty(ts, tss)
    | [[_, ..._] as ts', ...tss] =>
      let (ts, ts') = fix_empty_holes_between(ts, ts');
      switch (ts') {
      | [] => skip_empty(ts, tss)
      | [_, ..._] => [ts, ts', ...tss]
      };
    };
  };
  let fixed_between = List.fold_right(fix, tss, []);
  fix_empty_holes_left(fix_empty_holes_right(fixed_between));
};
