open Util;

[@deriving sexp]
type t('op, 'pre, 'post, 'bin) =
  | Op('op)
  | Pre('pre)
  | Post('post)
  | Bin('bin);

let get_op: t('op, _, _, _) => 'op =
  fun
  | Op(op) => op
  | _ => raise(Invalid_argument("Tile.get_op"));
let get_pre: t(_, 'pre, _, _) => 'pre =
  fun
  | Pre(pre) => pre
  | _ => raise(Invalid_argument("Tile.get_pre"));
let get_post: t(_, _, 'post, _) => 'post =
  fun
  | Post(post) => post
  | _ => raise(Invalid_argument("Tile.get_post"));
let get_bin: t(_, _, _, 'bin) => 'bin =
  fun
  | Bin(bin) => bin
  | _ => raise(Invalid_argument("Tile.get_bin"));

let is_op =
  fun
  | Op(_) => true
  | _ => false;
let is_bin =
  fun
  | Bin(_) => true
  | _ => false;

let get =
    (
      get_op: 'op => 'a,
      get_pre: 'pre => 'a,
      get_post: 'post => 'a,
      get_bin: 'bin => 'a,
    )
    : (t('op, 'pre, 'post, 'bin) => 'a) =>
  fun
  | Op(op) => get_op(op)
  | Pre(pre) => get_pre(pre)
  | Post(post) => get_post(post)
  | Bin(bin) => get_bin(bin);

let flip: t('op, 'pre, 'post, 'bin) => t('op, 'post, 'pre, 'bin) =
  fun
  | (Op(_) | Bin(_)) as t => t
  | Pre(pre) => Post(pre)
  | Post(post) => Pre(post);

let rev =
    (ts: list(t('op, 'pre, 'post, 'bin))): list(t('op, 'post, 'pre, 'bin)) =>
  List.rev_map(flip, ts);

module type S = {
  type op;
  type pre;
  type post;
  type bin;
  type nonrec t = t(op, pre, post, bin);
};

module type SORTED_INPUT = {
  module Term: Term.S;

  include
    S with
      type op := Term.op and
      type pre := Term.pre and
      type post := Term.post and
      type bin := Term.bin;

  let precedence: t => int;
  let associativity: IntMap.t(Associativity.t);
};
module type SORTED = {
  include SORTED_INPUT;
  let flatten_term: Term.t => list(t);
  let associate: list(t) => Skel.t;
  let mk_term: list(t) => Term.t;
};

module Make_sorted =
       (Term: Term.S, Sorted_input: SORTED_INPUT with module Term := Term) => {
  type itile = (int, Sorted_input.t);
  type tiles = list(Sorted_input.t);

  let rec flatten = (tm: Term.t): tiles =>
    switch (tm) {
    | Op(op) => [Op(op)]
    | Pre(pre, r) => [Pre(pre), ...flatten(r)]
    | Post(l, post) => flatten(l) @ [Post(post)]
    | Bin(l, bin, r) => flatten(l) @ [Bin(bin), ...flatten(r)]
    };

  let associate = (tiles: tiles): Skel.t => {
    let push_output =
        ((i, tile): itile, output_stack: list(Skel.t)): list(Skel.t) =>
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
              ~output_stack: list(Skel.t),
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
              ~output_stack: list(Skel.t),
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
          Sorted_input.precedence(tile) <= Sorted_input.precedence(post)
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
          Sorted_input.precedence(tile) <= Sorted_input.precedence(bin)
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

  let mk_term = (tiles: tiles): Term.t => {
    let rec go = (skel: Skel.t): Term.t => {
      let root = List.nth(tiles, Skel.root_index(skel));
      switch (skel) {
      | Op(_) => Op(get_op(root))
      | Pre(_, r) => Pre(get_pre(root), go(r))
      | Post(l, _) => Post(go(l), get_post(root))
      | Bin(l, _, r) => Bin(go(l), get_bin(root), go(r))
      };
    };
    go(associate(tiles));
  };
};
