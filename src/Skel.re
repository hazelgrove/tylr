type t =
  | Operand(int)
  | PreOp(int, t)
  | PostOp(t, int)
  | BinOp(t, int, t);

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
            ~operand_count: int,
            ~operator_count: int,
            ~new_hole_count: int,
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
    | [(i, tile), ..._]
        when
          (Tile.shape(tile) == PreOp || Tile.shape(tile) == Operand)
          && operand_count > operator_count =>
      let (output_stack, shunted_stack) =
        process_binop(
          ~output_stack,
          ~shunted_stack,
          (i, Tile.operator_hole),
        );
      go(
        ~operand_count,
        ~operator_count=operator_count + 1,
        ~new_hole_count=new_hole_count + 1,
        ~output_stack,
        ~shunted_stack,
        tiles,
      );
    | [(i, tile), ..._]
        when
          (Tile.shape(tile) == PostOp || Tile.shape(tile) == BinOp)
          && operand_count <= operator_count =>
      let (output_stack, shunted_stack) =
        process_operand(
          ~output_stack,
          ~shunted_stack,
          (i, Tile.operand_hole),
        );
      go(
        ~operand_count=operand_count + 1,
        ~operator_count,
        ~new_hole_count=new_hole_count + 1,
        ~output_stack,
        ~shunted_stack,
        tiles,
      );
    | [t, ...ts] =>
      let (operand_count, operator_count, process) =
        switch (Tile.shape(snd(t))) {
        | Operand => (operand_count + 1, operator_count, process_operand)
        | PreOp => (operand_count, operator_count, process_preop)
        | PostOp => (operand_count, operator_count, process_postop)
        | BinOp => (operand_count, operator_count + 1, process_binop)
        };
      let (output_stack, shunted_stack) =
        process(~output_stack, ~shunted_stack, t);
      go(
        ~operand_count,
        ~operator_count,
        ~new_hole_count,
        ~output_stack,
        ~shunted_stack,
        ts,
      );
    };
  };

  tiles
  |> List.mapi((i, tile) => (i, tile))
  |> go(
       ~operand_count=0,
       ~operator_count=0,
       ~new_hole_count=0,
       ~output_stack=[],
       ~shunted_stack=[],
     )
  |> List.hd;
};
