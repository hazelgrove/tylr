open Sexplib.Std;

[@deriving sexp]
type t =
  | Operand(int)
  | PreOp(int, t)
  | PostOp(t, int)
  | BinOp(t, int, t);

let rec size =
  fun
  | Operand(_) => 1
  | PreOp(_, r) => 1 + size(r)
  | PostOp(l, _) => size(l) + 1
  | BinOp(l, _, r) => size(l) + 1 + size(r);

let root_index =
  fun
  | Operand(n)
  | PreOp(n, _)
  | PostOp(_, n)
  | BinOp(_, n, _) => n;

module Make = (T: Tile.S) : {let mk: T.s => t;} => {
  type itile = (int, T.t);

  let mk = (tiles: T.s): t => {
    let push_output = ((i, tile): itile, output_stack: list(t)): list(t) =>
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
        | [skel1, skel2, ...skels] => [BinOp(skel2, i, skel1), ...skels]
        }
      };

    let process_operand = (~output_stack, ~shunted_stack, operand) => (
      output_stack,
      [operand, ...shunted_stack],
    );

    let rec process_preop =
            (
              ~output_stack: list(t),
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
              ~output_stack: list(t),
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
              ~output_stack: list(t),
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
              ~output_stack: list(t)=[],
              ~shunted_stack: list(itile)=[],
              itiles: list(itile),
            )
            : list(t) => {
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
};
