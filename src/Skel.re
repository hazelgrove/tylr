type t =
  | Operand(int)
  | PreOp(int, t)
  | PostOp(t, int)
  | BinOp(t, int, t);

let mk =
    (
      ~precedence: Tile.t('operand, 'preop, 'postop, 'binop) => int,
      ~operand_hole: 'operand,
      ~operator_hole: 'binop,
      tiles: list(Tile.t('operand, 'preop, 'postop, 'binop)),
    )
    : t => {
  let push_output =
      (
        (i: int, tile: Tile.t('operand, 'preop, 'postop, 'binop)),
        output_stack: list(t),
      )
      : list(t) =>
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
            ~output_stack: list(t),
            ~shunted_stack:
               list((int, Tile.t('operand, 'preop, 'postop, 'binop))),
            preop,
          ) =>
    switch (shunted_stack) {
    | []
    | [(_, PreOp(_) | BinOp(_)), ..._] => (
        output_stack,
        [preop, ...shunted_stack],
      )
    | [(_, Operand(_) | PostOp(_)) as hd, ...tl] =>
      process_preop(
        ~output_stack=push_output(hd, output_stack),
        ~shunted_stack=tl,
        preop,
      )
    };

  // assumes postops lose ties with preops and binops
  let rec process_postop =
          (
            ~output_stack: list(t),
            ~shunted_stack:
               list((int, Tile.t('operand, 'preop, 'postop, 'binop))),
            (_, op) as postop: (
              int,
              Tile.t('operand, 'preop, 'postop, 'binop),
            ),
          ) =>
    switch (shunted_stack) {
    | [] => (output_stack, [postop, ...shunted_stack])
    | [(_, Operand(_) | PostOp(_)) as hd, ...tl] =>
      process_postop(
        ~output_stack=push_output(hd, output_stack),
        ~shunted_stack=tl,
        postop,
      )
    | [(_, (PreOp(_) | BinOp(_)) as hd_op) as hd, ...tl] =>
      precedence(hd_op) <= precedence(op)
        ? process_postop(
            ~output_stack=push_output(hd, output_stack),
            ~shunted_stack=tl,
            postop,
          )
        : (output_stack, [postop, ...shunted_stack])
    };

  // currently assumes all binops are left-associative
  // and binops lose ties with preops
  let rec process_binop =
          (
            ~output_stack: list(t),
            ~shunted_stack:
               list((int, Tile.t('operand, 'preop, 'postop, 'binop))),
            (_, op) as binop,
          ) =>
    switch (shunted_stack) {
    | [] => (output_stack, [binop, ...shunted_stack])
    | [(_, Operand(_) | PostOp(_)) as hd, ...tl] =>
      process_binop(
        ~output_stack=push_output(hd, output_stack),
        ~shunted_stack=tl,
        binop,
      )
    | [(_, (PreOp(_) | BinOp(_)) as hd_op) as hd, ...tl] =>
      precedence(hd_op) <= precedence(op)
        ? process_binop(
            ~output_stack=push_output(hd, output_stack),
            ~shunted_stack=tl,
            binop,
          )
        : (output_stack, [binop, ...shunted_stack])
    };

  let rec go =
          (
            ~operand_count: int,
            ~operator_count: int,
            ~new_hole_count: int,
            ~output_stack: list(t),
            ~shunted_stack:
               list((int, Tile.t('operand, 'preop, 'postop, 'binop))),
            tiles: list((int, Tile.t('operand, 'preop, 'postop, 'binop))),
          )
          : list(t) => {
    switch (tiles) {
    | [] =>
      shunted_stack
      |> List.fold_left(
           (output_stack, itile) => push_output(itile, output_stack),
           output_stack,
         )
    | [(i, PreOp(_) | Operand(_)), ..._] when operand_count > operator_count =>
      let (output_stack, shunted_stack) =
        process_binop(
          ~output_stack,
          ~shunted_stack,
          (i, BinOp(operator_hole)),
        );
      go(
        ~operand_count,
        ~operator_count=operator_count + 1,
        ~new_hole_count=new_hole_count + 1,
        ~output_stack,
        ~shunted_stack,
        tiles,
      );
    | [(i, PostOp(_) | BinOp(_)), ..._] when operand_count <= operator_count =>
      let (output_stack, shunted_stack) =
        process_operand(
          ~output_stack,
          ~shunted_stack,
          (i, Operand(operand_hole)),
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
        switch (t) {
        | (_, Operand(_)) => (
            operand_count + 1,
            operator_count,
            process_operand,
          )
        | (_, PreOp(_)) => (operand_count, operator_count, process_preop)
        | (_, PostOp(_)) => (operand_count, operator_count, process_postop)
        | (_, BinOp(_)) => (operand_count, operator_count + 1, process_binop)
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
