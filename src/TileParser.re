module type TILE = {
  type t;
  type s = list(t);
  type term;
  let operand_hole: t;
  let operator_hole: t;
  let shape: t => TileShape.t(term);
};

let fix_empty_holes =
    (type a, module Tile: TILE with type t = a, tiles: Tile.s): Tile.s => {
  let rec fix_operand = (tiles: Tile.s) =>
    switch (tiles) {
    | [] => [Tile.operand_hole]
    | [t, ...ts] =>
      switch (Tile.shape(t)) {
      | PreOp(_) => [t, ...fix_operand(ts)]
      | Operand(_) => [t, ...fix_operator(ts)]
      | PostOp(_)
      | BinOp(_) => [Tile.operand_hole, ...fix_operator(tiles)]
      }
    }
  and fix_operator = (tiles: Tile.shape) =>
    switch (tiles) {
    | [] => []
    | [t, ...ts] =>
      switch (Tile.shape(t)) {
      | PostOp(_) => [t, ...fix_operator(ts)]
      | BinOp(_) => [t, ...fix_operand(ts)]
      | PreOp(_)
      | Operand(_) => [Tile.operator_hole, ...fix_operand(tiles)]
      }
    };
  fix_operand(tiles);
};

let parse =
    (
      type a,
      type b,
      module Tile: TILE with type t = a and type term = b,
      tiles: Tile.s,
    )
    : Tile.term => {
  let push_output =
      ((i: int, tile: Tile.t), output_stack: list(Tile.term)): list(t) =>
    switch (Tile.shape(tile)) {
    | Operand(tm) => [tm, ...output_stack]
    | PreOp(pre, _) =>
      switch (output_stack) {
      | [] => failwith("impossible: preop encountered empty stack")
      | [tm, ...tms] => [pre(tm), ...tms]
      }
    | PostOp(post, _) =>
      switch (output_stack) {
      | [] => failwith("impossible: postop encountered empty stack")
      | [tm, ...tms] => [post(tm), ...tms]
      }
    | BinOp(bin, _, _) =>
      switch (output_stack) {
      | []
      | [_] =>
        failwith("impossible: binop encountered empty or singleton stack")
      | [tm1, tm2, ...tms] => [bin(tm1, tm2), ...tms]
      }
    };

  let process_operand = (~output_stack, ~shunted_stack, operand) => (
    output_stack,
    [operand, ...shunted_stack],
  );

  let rec process_preop =
          (
            ~output_stack: list(Tile.term),
            ~shunted_stack: list((int, Tile.t)),
            preop,
          ) => {
    switch (shunted_stack) {
    | [] => (output_stack, [preop, ...shunted_stack])
    | [(_, tile) as hd, ...tl] =>
      switch (Tile.shape(tile)) {
      | PreOp(_)
      | BinOp(_) => (output_stack, [preop, ...shunted_stack])
      | Operand(_)
      | PostOp(_) =>
        process_preop(
          ~output_stack=push_output(hd, output_stack),
          ~shunted_stack=tl,
          preop,
        )
      }
    };
  };

  // assumes postops lose ties with preops and binops
  let rec process_postop =
          (
            ~output_stack: list(Tile.term),
            ~shunted_stack: list((int, Tile.t)),
            (_, op) as postop: (int, Tile.t),
          ) =>
    switch (shunted_stack) {
    | [] => (output_stack, [postop, ...shunted_stack])
    | [(_, tile) as hd, ...tl] =>
      switch (Tile.shape(tile)) {
      | Operand(_)
      | PostOp(_) =>
        process_postop(
          ~output_stack=push_output(hd, output_stack),
          ~shunted_stack=tl,
          postop,
        )
      | PreOp(_, precedence)
      | BinOp(_, precedence, _) =>
        precedence <= TileShape.precedence(Tile.shape(op))
          ? process_postop(
              ~output_stack=push_output(hd, output_stack),
              ~shunted_stack=tl,
              postop,
            )
          : (output_stack, [postop, ...shunted_stack])
      }
    };

  // currently assumes all binops are left-associative
  // and binops lose ties with preops
  let rec process_binop =
          (
            ~output_stack: list(Tile.term),
            ~shunted_stack: list((int, Tile.t)),
            (_, op) as binop,
          ) =>
    switch (shunted_stack) {
    | [] => (output_stack, [binop, ...shunted_stack])
    | [(_, tile) as hd, ...tl] =>
      switch (Tile.shape(tile)) {
      | Operand(_)
      | PostOp(_) =>
        process_binop(
          ~output_stack=push_output(hd, output_stack),
          ~shunted_stack=tl,
          binop,
        )
      | PreOp(_, precedence)
      | BinOp(_, precedence, _) =>
        precedence <= TileShape.precedence(Tile.shape(op))
          ? process_binop(
              ~output_stack=push_output(hd, output_stack),
              ~shunted_stack=tl,
              binop,
            )
          : (output_stack, [binop, ...shunted_stack])
      }
    };

  let rec go =
          (
            ~output_stack: list(Tile.term),
            ~shunted_stack: list((int, Tile.t)),
            tiles: list((int, Tile.t)),
          )
          : list(t) => {
    switch (tiles) {
    | [] =>
      shunted_stack
      |> List.fold_left(
           (output_stack, itile) => push_output(itile, output_stack),
           output_stack,
         )
    | [(_, tile) as t, ...ts] =>
      let process =
        switch (Tile.shape(tile)) {
        | Operand(_) => process_operand
        | PreOp(_) => process_preop
        | PostOp(_) => process_postop
        | BinOp(_) => process_binop
        };
      let (output_stack, shunted_stack) =
        process(~output_stack, ~shunted_stack, t);
      go(~output_stack, ~shunted_stack, ts);
    };
  };

  tiles
  |> fix_empty_holes((module Tile))
  |> List.mapi((i, tile) => (i, tile))
  |> go(~output_stack=[], ~shunted_stack=[])
  |> List.hd;
};
