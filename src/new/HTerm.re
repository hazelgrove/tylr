open Util;

type t('op, 'pre, 'post, 'bin) =
  | Op('op)
  | Pre('pre, t('op, 'pre, 'post, 'bin))
  | Post(t('op, 'pre, 'post, 'bin), 'post)
  | Bin(t('op, 'pre, 'post, 'bin), 'bin, t('op, 'pre, 'post, 'bin));

let get =
    (
      f_op: 'op => 'a,
      f_pre: ('pre, t('op, 'pre, 'post, 'bin) as 't) => 'a,
      f_post: ('t, 'post) => 'a,
      f_bin: ('t, 'bin, 't) => 'a,
      t: 't,
    ) =>
  switch (t) {
  | Op(op) => f_op(op)
  | Pre(pre, r) => f_pre(pre, r)
  | Post(l, post) => f_post(l, post)
  | Bin(l, bin, r) => f_bin(l, bin, r)
  };

module type IN = {
  type op;
  type pre;
  type post;
  type bin;

  type nonrec t = t(op, pre, post, bin);
  type tile = Tile.t(op, pre, post, bin);

  let precedence: HTile.t => option(int);
  let associativity: IntMap.t(Associativity.t);

  let of_htiles: HTile.s => option(list(tile));
  let of_htile: HTile.t => option(tile);

  let to_htiles: list(tile) => HTile.s;
  let to_htile: tile => HTile.t;
};

module Make = (I: IN) => {
  type itile = (int, I.tile);
  type tiles = list(I.tile);

  let rec flatten = (tm: t): tiles =>
    switch (tm) {
    | Op(op) => [Op(op)]
    | Pre(pre, r) => [Pre(pre), ...flatten(r)]
    | Post(l, post) => flatten(l) @ [Post(post)]
    | Bin(l, bin, r) => flatten(l) @ [Bin(bin), ...flatten(r)]
    };

  let associate = (tiles: tiles): Skel.t => {
    let push_output = ((i, tile): itile, output_stack: list(t)): list(t) =>
      switch (tile) {
      | Op(_) => [Op(i), ...output_stack]
      | Pre(_) =>
        switch (output_stack) {
        | [] => failwith("impossible: pre encountered empty stack")
        | [skel, ...skels] => [Pre(i, skel), ...skels]
        }
      | Post(_) =>
        switch (output_stack) {
        | [] => failwith("impossible: post encountered empty stack")
        | [skel, ...skels] => [Post(skel, i), ...skels]
        }
      | Bin(_) =>
        switch (output_stack) {
        | []
        | [_] =>
          failwith("impossible: bin encountered empty or singleton stack")
        | [skel1, skel2, ...skels] => [Bin(skel2, i, skel1), ...skels]
        }
      };

    let process_operand = (~output_stack, ~shunted_stack, op) => (
      output_stack,
      [op, ...shunted_stack],
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
        | Pre(_)
        | Bin(_) => (output_stack, [ipreop, ...shunted_stack])
        | Op(_)
        | Post(_) =>
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
              (_, post) as ipostop: itile,
            ) =>
      switch (shunted_stack) {
      | [] => (output_stack, [ipostop, ...shunted_stack])
      | [(_, tile) as itile, ...itiles] =>
        switch (tile) {
        | Op(_)
        | Post(_) =>
          process_postop(
            ~output_stack=push_output(itile, output_stack),
            ~shunted_stack=itiles,
            ipostop,
          )
        | Pre(_)
        | Bin(_) =>
          I.precedence(tile) <= I.precedence(post)
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
              (_, bin) as ibinop: itile,
            ) =>
      switch (shunted_stack) {
      | [] => (output_stack, [ibinop, ...shunted_stack])
      | [(_, tile) as itile, ...itiles] =>
        switch (tile) {
        | Op(_)
        | Post(_) =>
          process_binop(
            ~output_stack=push_output(itile, output_stack),
            ~shunted_stack=itiles,
            ibinop,
          )
        | Pre(_)
        | Bin(_) =>
          I.precedence(tile) <= I.precedence(bin)
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
          | Op(_) => process_operand
          | Pre(_) => process_preop
          | Post(_) => process_postop
          | Bin(_) => process_binop
          };
        let (output_stack, shunted_stack) =
          process(~output_stack, ~shunted_stack, itile);
        go(~output_stack, ~shunted_stack, itiles);
      };
    };

    tiles |> List.mapi((i, tile) => (i, tile)) |> go |> List.hd;
  };

  let mk = (tiles: tiles): t => {
    let rec go = (skel: Skel.t) => {
      let root = List.nth(tiles, Skel.root_index(skel));
      switch (skel) {
      | Op(_) => Op(Tile.get_op(root))
      | Pre(_, r) => Pre(Tile.get_pre(root), go(r))
      | Post(l, _) => Post(go(l), Tile.get_post(root))
      | Bin(l, _, r) => Bin(go(l), Tile.get_bin(root), go(r))
      };
    };
    go(associate(tiles));
  };
};
