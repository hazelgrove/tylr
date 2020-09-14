module Make =
       (T: Tile.S)
       : {
         let mk_hole: unit => T.s;

         let fix_empty_holes: (T.s, T.s) => (T.s, T.s);

         type root =
           | Operand(T.operand)
           | PreOp(T.preop, T.s)
           | PostOp(T.s, T.postop)
           | BinOp(T.s, T.binop, T.s);

         let root: T.s => root;

         let get_root:
           (
             ~operand: T.operand => 'a,
             ~preop: T.preop => 'a,
             ~postop: T.postop => 'a,
             ~binop: T.binop => 'a,
             T.s
           ) =>
           'a;

         let update_root:
           (
             ~operand: T.operand => T.operand,
             ~preop: T.preop => T.preop,
             ~postop: T.postop => T.postop,
             ~binop: T.binop => T.binop,
             T.s
           ) =>
           T.s;
       } => {
  type hole_shape =
    | Operand
    | Operator;

  let keystone_shape =
      (t1: option(T.t), t2: option(T.t)): option(hole_shape) =>
    switch (t1, t2) {
    | (None, None) => Some(Operand)
    | (Some(t), None) =>
      switch (t) {
      | Operand(_)
      | PostOp(_) => None
      | PreOp(_)
      | BinOp(_) => Some(Operand)
      }
    | (None, Some(t)) =>
      switch (t) {
      | Operand(_)
      | PreOp(_) => None
      | PostOp(_)
      | BinOp(_) => Some(Operand)
      }
    | (Some(t1), Some(t2)) =>
      switch (t1, t2) {
      | (Operand(_) | PostOp(_), Operand(_) | PreOp(_)) => Some(Operator)
      | (PreOp(_) | BinOp(_), PostOp(_) | BinOp(_)) => Some(Operand)
      | _ => None
      }
    };

  let mk_hole = (): T.s => [T.mk_operand_hole()];

  let fix_empty_holes = (prefix: T.s, suffix: T.s): (T.s, T.s) => {
    let go_prefix = (tiles: T.s) => {
      let rec go_operand = (tiles: T.s) => {
        switch (tiles) {
        | [] => []
        | [t1, t2, ...ts]
            when T.is_operand_hole(t1) && T.is_operator_hole(t2) =>
          go_operand(ts)
        | [t, ...ts] when T.is_operator_hole(t) => go_operand(ts)
        | [t, ...ts] =>
          switch (t) {
          | PreOp(_) => [t, ...go_operand(ts)]
          | Operand(_) => [t, ...go_operator(ts)]
          | PostOp(_)
          | BinOp(_) => [T.mk_operand_hole(), ...go_operator(tiles)]
          }
        };
      }
      and go_operator = (tiles: T.s) => {
        switch (tiles) {
        | [] => []
        | [t1, t2, ...ts]
            when T.is_operator_hole(t1) && T.is_operand_hole(t2) =>
          go_operator(ts)
        | [t, ...ts] when T.is_operand_hole(t) => go_operator(ts)
        | [t, ...ts] =>
          switch (t) {
          | PostOp(_) => [t, ...go_operator(ts)]
          | BinOp(_) => [t, ...go_operand(ts)]
          | PreOp(_)
          | Operand(_) => [T.mk_operator_hole(), ...go_operand(tiles)]
          }
        };
      };
      go_operand(tiles);
    };

    let go_suffix = (tiles: T.s) => {
      let rec go_operand = (tiles: T.s) => {
        switch (tiles) {
        | [] => []
        | [t1, t2, ...ts]
            when T.is_operand_hole(t1) && T.is_operator_hole(t2) =>
          go_operand(ts)
        | [t, ...ts] when T.is_operator_hole(t) => go_operand(ts)
        | [t, ...ts] =>
          switch (t) {
          | PostOp(_) => [t, ...go_operand(ts)]
          | Operand(_) => [t, ...go_operator(ts)]
          | PreOp(_)
          | BinOp(_) => [T.mk_operand_hole(), ...go_operator(tiles)]
          }
        };
      }
      and go_operator = (tiles: T.s) => {
        switch (tiles) {
        | [] => []
        | [t1, t2, ...ts]
            when T.is_operator_hole(t1) && T.is_operand_hole(t2) =>
          go_operator(ts)
        | [t, ...ts] when T.is_operand_hole(t) => go_operator(ts)
        | [t, ...ts] =>
          switch (t) {
          | PreOp(_) => [t, ...go_operator(ts)]
          | BinOp(_) => [t, ...go_operand(ts)]
          | PostOp(_)
          | Operand(_) => [T.mk_operator_hole(), ...go_operand(tiles)]
          }
        };
      };
      go_operand(tiles);
    };

    let fixed_prefix = go_prefix(prefix);
    let fixed_suffix = List.rev(go_suffix(List.rev(suffix)));
    switch (
      keystone_shape(
        ListUtil.last_opt(fixed_prefix),
        ListUtil.hd_opt(fixed_suffix),
      )
    ) {
    | None => (fixed_prefix, fixed_suffix)
    | Some(Operand) => (
        fixed_prefix,
        [T.mk_operand_hole(), ...fixed_suffix],
      )
    | Some(Operator) => (
        fixed_prefix,
        [T.mk_operator_hole(), ...fixed_prefix],
      )
    };
  };

  module Skel = Skel.Make(T);

  type root =
    | Operand(T.operand)
    | PreOp(T.preop, T.s)
    | PostOp(T.s, T.postop)
    | BinOp(T.s, T.binop, T.s);

  let root = (tiles: T.s): root =>
    switch (Skel.mk(tiles)) {
    | Operand(_) => Operand(Tile.get_operand(List.hd(tiles)))
    | PreOp(_) => PreOp(Tile.get_preop(List.hd(tiles)), List.tl(tiles))
    | PostOp(_) =>
      let (prefix, last) = ListUtil.split_last(tiles);
      PostOp(prefix, Tile.get_postop(last));
    | BinOp(_, n, _) =>
      let (prefix, nth, suffix) = ListUtil.split_nth(n, tiles);
      BinOp(prefix, Tile.get_binop(nth), suffix);
    };

  let get_root =
      (
        ~operand: T.operand => 'a,
        ~preop: T.preop => 'a,
        ~postop: T.postop => 'a,
        ~binop: T.binop => 'a,
        tiles: T.s,
      )
      : 'a =>
    switch (root(tiles)) {
    | Operand(t) => operand(t)
    | PreOp(t, _) => preop(t)
    | PostOp(_, t) => postop(t)
    | BinOp(_, t, _) => binop(t)
    };

  let update_root =
      (
        ~operand: T.operand => T.operand,
        ~preop: T.preop => T.preop,
        ~postop: T.postop => T.postop,
        ~binop: T.binop => T.binop,
        tiles: T.s,
      )
      : T.s =>
    switch (root(tiles)) {
    | Operand(t) => [Operand(operand(t))]
    | PreOp(t, ts) => [PreOp(preop(t)), ...ts]
    | PostOp(ts, t) => ts @ [PostOp(postop(t))]
    | BinOp(ts1, t, ts2) => ts1 @ [BinOp(binop(t)), ...ts2]
    };
};
