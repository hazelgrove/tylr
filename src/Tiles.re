module type TILE = {
  type operand;
  type preop;
  type postop;
  type binop;

  type t = Tile.t(operand, preop, postop, binop);
  type s = list(t);

  let mk_operand_hole: unit => t;
  let mk_operator_hole: unit => t;

  let is_operand_hole: t => bool;
  let is_operator_hole: t => bool;

  let precedence: t => int;
  let associativity: IntMap.t(Associativity.t);

  let get_open_children: t => list(s);
};

module Util =
       (T: TILE)
       : {
         let mk_hole: unit => T.s;

         let fix_empty_holes: (T.s, T.s) => (T.s, T.s);

         let parse: T.s => Skel.t;

         let delete_tile_and_fix_empty_holes: (T.s, T.t, T.s) => (T.s, int);
         let insert_tile_and_fix_empty_holes: (T.s, T.t, T.s) => (T.s, int);

         type root =
           | Operand(T.operand)
           | PreOp(T.preop, T.s)
           | PostOp(T.s, T.postop)
           | BinOp(T.s, T.binop, T.s);

         let root: T.s => root;

         let map_root:
           (
             ~operand: T.operand => T.operand,
             ~preop: T.preop => T.preop,
             ~postop: T.postop => T.postop,
             ~binop: T.binop => T.binop,
             T.s
           ) =>
           T.s;
       } => {
  type itile = (int, T.t);

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

  let parse = (tiles: T.s): Skel.t => {
    let push_output =
        ((i, tile): itile, output_stack: list(Skel.t)): list(Skel.t) =>
      switch (tile) {
      | Operand(_) => [Operand(i), ...output_stack]
      | PreOp(_) =>
        switch (output_stack) {
        | [] => failwith("impossible: preop encountered empty stack")
        | [skel, ...skels] => [PreOp(i, skel), ...skels]
        }
      | PostOp(_) =>
        switch (output_stack) {
        | [] => failwith("impossible: postop encountered empty stack")
        | [skel, ...skels] => [PostOp(skel, i), ...skels]
        }
      | BinOp(_) =>
        switch (output_stack) {
        | []
        | [_] =>
          failwith("impossible: binop encountered empty or singleton stack")
        | [skel1, skel2, ...skels] => [BinOp(skel1, i, skel2), ...skels]
        }
      };

    let process_operand = (~output_stack, ~shunted_stack, operand) => (
      output_stack,
      [operand, ...shunted_stack],
    );

    let rec process_preop =
            (
              ~output_stack: list(Skel.t),
              ~shunted_stack: list(itile),
              ipreop: itile,
            ) => {
      switch (shunted_stack) {
      | [] => (output_stack, [ipreop, ...shunted_stack])
      | [(_, tile) as itile, ...itiles] =>
        switch (tile) {
        | PreOp(_)
        | BinOp(_) => (output_stack, [ipreop, ...shunted_stack])
        | Operand(_)
        | PostOp(_) =>
          process_preop(
            ~output_stack=push_output(itile, output_stack),
            ~shunted_stack=itiles,
            ipreop,
          )
        }
      };
    };

    // assumes postops lose ties with preops and binops
    let rec process_postop =
            (
              ~output_stack: list(Skel.t),
              ~shunted_stack: list(itile),
              (_, postop) as ipostop: itile,
            ) =>
      switch (shunted_stack) {
      | [] => (output_stack, [ipostop, ...shunted_stack])
      | [(_, tile) as itile, ...itiles] =>
        switch (tile) {
        | Operand(_)
        | PostOp(_) =>
          process_postop(
            ~output_stack=push_output(itile, output_stack),
            ~shunted_stack=itiles,
            ipostop,
          )
        | PreOp(_)
        | BinOp(_) =>
          T.precedence(tile) <= T.precedence(postop)
            ? process_postop(
                ~output_stack=push_output(itile, output_stack),
                ~shunted_stack=itiles,
                ipostop,
              )
            : (output_stack, [ipostop, ...shunted_stack])
        }
      };

    // currently assumes all binops are left-associative
    // and binops lose ties with preops
    let rec process_binop =
            (
              ~output_stack: list(Skel.t),
              ~shunted_stack: list(itile),
              (_, binop) as ibinop: itile,
            ) =>
      switch (shunted_stack) {
      | [] => (output_stack, [ibinop, ...shunted_stack])
      | [(_, tile) as itile, ...itiles] =>
        switch (tile) {
        | Operand(_)
        | PostOp(_) =>
          process_binop(
            ~output_stack=push_output(itile, output_stack),
            ~shunted_stack=itiles,
            ibinop,
          )
        | PreOp(_)
        | BinOp(_) =>
          T.precedence(tile) <= T.precedence(binop)
            ? process_binop(
                ~output_stack=push_output(itile, output_stack),
                ~shunted_stack=itiles,
                ibinop,
              )
            : (output_stack, [ibinop, ...shunted_stack])
        }
      };

    let rec go =
            (
              ~output_stack: list(Skel.t)=[],
              ~shunted_stack: list(itile)=[],
              itiles: list(itile),
            )
            : list(Skel.t) => {
      switch (itiles) {
      | [] =>
        shunted_stack
        |> List.fold_left(
             (output_stack, t) => push_output(t, output_stack),
             output_stack,
           )
      | [(_, tile) as itile, ...itiles] =>
        let process =
          switch (tile) {
          | Operand(_) => process_operand
          | PreOp(_) => process_preop
          | PostOp(_) => process_postop
          | BinOp(_) => process_binop
          };
        let (output_stack, shunted_stack) =
          process(~output_stack, ~shunted_stack, itile);
        go(~output_stack, ~shunted_stack, itiles);
      };
    };

    tiles |> List.mapi((i, tile) => (i, tile)) |> go |> List.hd;
  };

  let delete_tile_and_fix_empty_holes =
      (prefix: T.s, tile: T.t, suffix: T.s): (T.s, int) => {
    let open_children_tiles = tile |> T.get_open_children |> List.flatten;
    let (fixed_prefix, fixed_suffix) =
      fix_empty_holes(prefix @ open_children_tiles, suffix);
    (fixed_prefix @ fixed_suffix, List.length(fixed_prefix));
  };

  let insert_tile_and_fix_empty_holes =
      (prefix: T.s, tile: T.t, suffix: T.s): (T.s, int) => {
    let (fixed_prefix, fixed_suffix) =
      fix_empty_holes([tile, ...prefix], suffix);
    (fixed_prefix @ fixed_suffix, List.length(fixed_prefix));
  };

  type root =
    | Operand(T.operand)
    | PreOp(T.preop, T.s)
    | PostOp(T.s, T.postop)
    | BinOp(T.s, T.binop, T.s);

  let root = (tiles: T.s): root =>
    switch (parse(tiles)) {
    | Operand(_) => Operand(Tile.get_operand(List.hd(tiles)))
    | PreOp(_) => PreOp(Tile.get_preop(List.hd(tiles)), List.tl(tiles))
    | PostOp(_) =>
      let (prefix, last) = ListUtil.split_last(tiles);
      PostOp(prefix, Tile.get_postop(last));
    | BinOp(_, n, _) =>
      let (prefix, nth, suffix) = ListUtil.split_nth(n, tiles);
      BinOp(prefix, Tile.get_binop(nth), suffix);
    };

  let map_root =
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
