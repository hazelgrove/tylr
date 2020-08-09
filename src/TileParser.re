module type TILE = {
  type term;
  type t;

  let mk_operand_hole: unit => t;
  let mk_operator_hole: unit => t;

  let is_operand_hole: t => bool;
  let is_operator_hole: t => bool;

  let shape: t => TileShape.t(term);
};

let fix_empty_holes =
    (type a, module Tile: TILE with type t = a, tiles: list(Tile.t))
    : list(Tile.t) => {
  let rec fix_operand = (tiles: list(Tile.t)) =>
    switch (tiles) {
    | [] => [Tile.mk_operand_hole()]
    | [t1, t2, ...ts]
        when Tile.is_operand_hole(t1) && Tile.is_operator_hole(t2) =>
      fix_operand(ts)
    | [t, ...ts] when Tile.is_operator_hole(t) => fix_operand(ts)
    | [t, ...ts] =>
      switch (Tile.shape(t)) {
      | PreOp(_) => [t, ...fix_operand(ts)]
      | Operand(_) => [t, ...fix_operator(ts)]
      | PostOp(_)
      | BinOp(_) => [Tile.mk_operand_hole(), ...fix_operator(tiles)]
      }
    }
  and fix_operator = (tiles: list(Tile.t)) =>
    switch (tiles) {
    | [] => []
    | [t1, t2, ...ts]
        when Tile.is_operator_hole(t1) && Tile.is_operand_hole(t2) =>
      fix_operator(ts)
    | [t, ...ts] when Tile.is_operand_hole(t) => fix_operator(ts)
    | [t, ...ts] =>
      switch (Tile.shape(t)) {
      | PostOp(_) => [t, ...fix_operator(ts)]
      | BinOp(_) => [t, ...fix_operand(ts)]
      | PreOp(_)
      | Operand(_) => [Tile.mk_operator_hole(), ...fix_operand(tiles)]
      }
    };
  fix_operand(tiles);
};

let parse =
    (
      type a,
      type b,
      module Tile: TILE with type t = a and type term = b,
      tiles: list(Tile.t),
    )
    : Tile.term => {
  let push_output =
      (tile: Tile.t, output_stack: list(Tile.term)): list(Tile.term) =>
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
            ~shunted_stack: list(Tile.t),
            preop: Tile.t,
          ) => {
    switch (shunted_stack) {
    | [] => (output_stack, [preop, ...shunted_stack])
    | [t, ...ts] =>
      switch (Tile.shape(t)) {
      | PreOp(_)
      | BinOp(_) => (output_stack, [preop, ...shunted_stack])
      | Operand(_)
      | PostOp(_) =>
        process_preop(
          ~output_stack=push_output(t, output_stack),
          ~shunted_stack=ts,
          preop,
        )
      }
    };
  };

  // assumes postops lose ties with preops and binops
  let rec process_postop =
          (
            ~output_stack: list(Tile.term),
            ~shunted_stack: list(Tile.t),
            postop: Tile.t,
          ) =>
    switch (shunted_stack) {
    | [] => (output_stack, [postop, ...shunted_stack])
    | [t, ...ts] =>
      switch (Tile.shape(t)) {
      | Operand(_)
      | PostOp(_) =>
        process_postop(
          ~output_stack=push_output(t, output_stack),
          ~shunted_stack=ts,
          postop,
        )
      | PreOp(_, precedence)
      | BinOp(_, precedence, _) =>
        precedence <= TileShape.precedence(Tile.shape(postop))
          ? process_postop(
              ~output_stack=push_output(t, output_stack),
              ~shunted_stack=ts,
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
            ~shunted_stack: list(Tile.t),
            binop: Tile.t,
          ) =>
    switch (shunted_stack) {
    | [] => (output_stack, [binop, ...shunted_stack])
    | [t, ...ts] =>
      switch (Tile.shape(t)) {
      | Operand(_)
      | PostOp(_) =>
        process_binop(
          ~output_stack=push_output(t, output_stack),
          ~shunted_stack=ts,
          binop,
        )
      | PreOp(_, precedence)
      | BinOp(_, precedence, _) =>
        precedence <= TileShape.precedence(Tile.shape(binop))
          ? process_binop(
              ~output_stack=push_output(t, output_stack),
              ~shunted_stack=ts,
              binop,
            )
          : (output_stack, [binop, ...shunted_stack])
      }
    };

  let rec go =
          (
            ~output_stack: list(Tile.term),
            ~shunted_stack: list(Tile.t),
            tiles: list(Tile.t),
          )
          : list(Tile.term) => {
    switch (tiles) {
    | [] =>
      shunted_stack
      |> List.fold_left(
           (output_stack, t) => push_output(t, output_stack),
           output_stack,
         )
    | [t, ...ts] =>
      let process =
        switch (Tile.shape(t)) {
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
  |> go(~output_stack=[], ~shunted_stack=[])
  |> List.hd;
};
