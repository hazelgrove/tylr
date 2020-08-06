type t =
  | Operand(int) // Hole, Num, Var
  | PreOp(int, t) // If, Let
  | PostOp(t, int) // Ann
  | BinOp(t, int, t); // OpHole, Plus, Times

let mk = (type a, module Tile: Tile.S with type t = a, tiles: list(a)): t => {
  let push_output =
      ((i: int, tile: Tile.t), output_stack: list(t)): list(t) =>
    switch (Tile.shape(tile)) {
    | Operand => [Operand(i), ...output_stack]
    | PreOp =>
      switch (output_stack) {
      | [] => failwith("impossible: preop encountered empty stack")
      | [skel, ...skels] => [PreOp(i, skel), ...skels]
      }
    | PostOp =>
      switch (output_stack) {
      | [] => failwith("impossible: postop encountered empty stack")
      | [skel, ...skels] => [PostOp(skel, i), ...skels]
      }
    | BinOp =>
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
            ~output_stack: list(t),
            ~shunted_stack: list((int, Tile.t)),
            preop,
          ) => {
    switch (shunted_stack) {
    | [] => (output_stack, [preop, ...shunted_stack])
    | [(_, tile) as hd, ...tl] =>
      switch (Tile.shape(tile)) {
      | PreOp
      | BinOp => (output_stack, [preop, ...shunted_stack])
      | Operand
      | PostOp =>
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
            ~output_stack: list(t),
            ~shunted_stack: list((int, Tile.t)),
            (_, op) as postop: (int, Tile.t),
          ) =>
    switch (shunted_stack) {
    | [] => (output_stack, [postop, ...shunted_stack])
    | [(_, tile) as hd, ...tl] =>
      switch (Tile.shape(tile)) {
      | Operand
      | PostOp =>
        process_postop(
          ~output_stack=push_output(hd, output_stack),
          ~shunted_stack=tl,
          postop,
        )
      | PreOp
      | BinOp =>
        Tile.precedence(tile) <= Tile.precedence(op)
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
            ~output_stack: list(t),
            ~shunted_stack: list((int, Tile.t)),
            (_, op) as binop,
          ) =>
    switch (shunted_stack) {
    | [] => (output_stack, [binop, ...shunted_stack])
    | [(_, tile) as hd, ...tl] =>
      switch (Tile.shape(tile)) {
      | Operand
      | PostOp =>
        process_binop(
          ~output_stack=push_output(hd, output_stack),
          ~shunted_stack=tl,
          binop,
        )
      | PreOp
      | BinOp =>
        Tile.precedence(tile) <= Tile.precedence(op)
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
            ~output_stack: list(t),
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
        | Operand => process_operand
        | PreOp => process_preop
        | PostOp => process_postop
        | BinOp => process_binop
        };
      let (output_stack, shunted_stack) =
        process(~output_stack, ~shunted_stack, t);
      go(
        ~output_stack,
        ~shunted_stack,
        ts,
      );
    };
  };

  tiles
  |> List.mapi((i, tile) => (i, tile))
  |> go(
       ~output_stack=[],
       ~shunted_stack=[],
     )
  |> List.hd;
};
