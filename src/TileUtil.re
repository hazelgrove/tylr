module type TILE = {
  type operand;
  type preop;
  type postop;
  type binop;
  type t = Tile.t(operand, preop, postop, binop);

  let mk_operand_hole: unit => t;
  let mk_operator_hole: unit => t;

  let is_operand_hole: t => bool;
  let is_operator_hole: t => bool;

  let precedence: t => int;
  let associativity: IntMap.t(Associativity.t);

  let get_open_children: t => list(list(t));
};

type hole_shape =
  | Operand
  | Operator;

let keystone_shape =
    (t1: option(Tile.t(_)), t2: option(Tile.t(_))): option(hole_shape) =>
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

module Make = (Tile: TILE) => {
  type itile = (int, Tile.t);

  let fix_empty_holes =
      (prefix: list(Tile.t), suffix: list(Tile.t))
      : (list(Tile.t), list(Tile.t)) => {
    let go_prefix = (tiles: list(Tile.t)) => {
      let rec go_operand = (tiles: list(Tile.t)) => {
        switch (tiles) {
        | [] => []
        | [t1, t2, ...ts]
            when Tile.is_operand_hole(t1) && Tile.is_operator_hole(t2) =>
          go_operand(ts)
        | [t, ...ts] when Tile.is_operator_hole(t) => go_operand(ts)
        | [t, ...ts] =>
          switch (t) {
          | PreOp(_) => [t, ...go_operand(ts)]
          | Operand(_) => [t, ...go_operator(ts)]
          | PostOp(_)
          | BinOp(_) => [Tile.mk_operand_hole(), ...go_operator(tiles)]
          }
        };
      }
      and go_operator = (tiles: list(Tile.t)) => {
        switch (tiles) {
        | [] => []
        | [t1, t2, ...ts]
            when Tile.is_operator_hole(t1) && Tile.is_operand_hole(t2) =>
          go_operator(ts)
        | [t, ...ts] when Tile.is_operand_hole(t) => go_operator(ts)
        | [t, ...ts] =>
          switch (t) {
          | PostOp(_) => [t, ...go_operator(ts)]
          | BinOp(_) => [t, ...go_operand(ts)]
          | PreOp(_)
          | Operand(_) => [Tile.mk_operator_hole(), ...go_operand(tiles)]
          }
        };
      };
      go_operand(tiles);
    };

    let go_suffix = (tiles: list(Tile.t)) => {
      let rec go_operand = (tiles: list(Tile.t)) => {
        switch (tiles) {
        | [] => []
        | [t1, t2, ...ts]
            when Tile.is_operand_hole(t1) && Tile.is_operator_hole(t2) =>
          go_operand(ts)
        | [t, ...ts] when Tile.is_operator_hole(t) => go_operand(ts)
        | [t, ...ts] =>
          switch (t) {
          | PostOp(_) => [t, ...go_operand(ts)]
          | Operand(_) => [t, ...go_operator(ts)]
          | PreOp(_)
          | BinOp(_) => [Tile.mk_operand_hole(), ...go_operator(tiles)]
          }
        };
      }
      and go_operator = (tiles: list(Tile.t)) => {
        switch (tiles) {
        | [] => []
        | [t1, t2, ...ts]
            when Tile.is_operator_hole(t1) && Tile.is_operand_hole(t2) =>
          go_operator(ts)
        | [t, ...ts] when Tile.is_operand_hole(t) => go_operator(ts)
        | [t, ...ts] =>
          switch (t) {
          | PreOp(_) => [t, ...go_operator(ts)]
          | BinOp(_) => [t, ...go_operand(ts)]
          | PostOp(_)
          | Operand(_) => [Tile.mk_operator_hole(), ...go_operand(tiles)]
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
        [Tile.mk_operand_hole(), ...fixed_suffix],
      )
    | Some(Operator) => (
        fixed_prefix,
        [Tile.mk_operator_hole(), ...fixed_prefix],
      )
    };
  };

  let parse = (tiles: list(Tile.t)): Skel.t => {
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
          Tile.precedence(tile) <= Tile.precedence(postop)
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
          Tile.precedence(tile) <= Tile.precedence(binop)
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
      (prefix: list(Tile.t), tile: Tile.t, suffix: list(Tile.t))
      : (list(Tile.t), int) => {
    let open_children_tiles = tile |> Tile.get_open_children |> List.flatten;
    let (fixed_prefix, fixed_suffix) =
      fix_empty_holes(prefix @ open_children_tiles, suffix);
    (fixed_prefix @ fixed_suffix, List.length(fixed_prefix));
  };

  let insert_tile_and_fix_empty_holes =
      (prefix: list(Tile.t), tile: Tile.t, suffix: list(Tile.t))
      : (list(Tile.t), int) => {
    let (fixed_prefix, fixed_suffix) =
      fix_empty_holes([tile, ...prefix], suffix);
    (fixed_prefix @ fixed_suffix, List.length(fixed_prefix));
  };
};
