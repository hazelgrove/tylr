open Util;
open OptUtil.Syntax;

module type S_INPUT = {
  module Term: Term.S;
  module Tile: Tile.S with module Term := Term;

  let sort:
    (
      ~sort_and_associate: Unsorted.Tile.s => option(Term.t),
      Unsorted.Tile.t
    ) =>
    option(Tile.t);
  let unsort:
    (~dissociate_and_unsort: Term.t => Unsorted.Tile.s, Tile.t) =>
    Unsorted.Tile.t;

  // TODO return option?
  let connect: AltList.t(Unsorted.Tessera.t, Term.t) => Tile.t;
  let disconnect: Tile.t => AltList.t(Unsorted.Tessera.t, Term.t);
};

module type S = {
  module Term: Term.S;
  module Tile: Tile.S with module Term := Term;

  let associate: list(Tile.t) => Term.t;
  let dissociate: Term.t => list(Tile.t);

  let sort: Unsorted.Tile.t => option(Tile.t);
  let unsort: Tile.t => Unsorted.Tile.t;

  let sort_and_associate: Unsorted.Tile.s => option(Term.t);
  let dissociate_and_unsort: Term.t => Unsorted.Tile.s;

  let connect: AltList.t(Unsorted.Tessera.t, Term.t) => Tile.t;
  let disconnect: Tile.t => AltList.t(Unsorted.Tessera.t, Term.t);

  let connect_selection: Selection.t(Tile.t) => Selection.t(Tile.t);
  let fix_empty_holes:
    list(Selection.t(Tile.t)) => list(Selection.t(Tile.t));
};

// outside of functor for use by unsorted selections
let connect_selection =
    (
      ~connect: AltList.t(Unsorted.Tessera.t, list('tile)) => 'tile,
      selection: Selection.t('tile) as 'selection,
    )
    : 'selection => {
  let rec go = (selection: 'selection): 'selection =>
    switch (selection) {
    | [] => []
    | [Tile(_) as elem, ...selection] => [elem, ...go(selection)]
    | [Tessera(tessera) as elem, ...selection'] =>
      if (Unsorted.Tessera.is_closing(tessera)) {
        [elem, ...go(selection')];
      } else {
        // TODO handle mid
        switch (go_opening(tessera, selection')) {
        | None => selection
        | Some((tile, selection)) => [Tile(tile), ...go(selection)]
        };
      }
    }
  and go_opening =
      (
        ~rev_tiles: list('tile)=[],
        open_: Unsorted.Tessera.t,
        selection: 'selection,
      )
      : option(('tile, 'selection)) =>
    switch (selection) {
    | [] => None
    | [Tile(tile), ...selection] =>
      go_opening(~rev_tiles=[tile, ...rev_tiles], open_, selection)
    | [Tessera(tessera), ...selection] =>
      // TODO handle mid tesserae
      if (Unsorted.Tessera.is_closing(tessera)) {
        let ts = List.rev(rev_tiles);
        let tile = connect((open_, [(ts, tessera)]));
        Some((tile, selection));
      } else {
        let* (tile, selection) = go_opening(open_, selection);
        go_opening(~rev_tiles=[tile, ...rev_tiles], open_, selection);
      }
    };
  go(selection);
};

// TODO resolve this awkwardness
module T = Tile;
module Make =
       (
         Term: Term.S,
         Tile: Tile.S with module Term := Term,
         Input: S_INPUT with module Term := Term and module Tile := Tile,
       )
       : (S with module Term := Term and module Tile := Tile) => {
  type selection = Selection.t(Tile.t);
  type tiles = list(Tile.t);

  type itile = (int, Tile.t);
  let mk_skel = (tiles: tiles): Skel.t => {
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
          Tile.precedence(tile) <= Tile.precedence(post)
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
          Tile.precedence(tile) <= Tile.precedence(bin)
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
  let associate = (tiles: tiles): Term.t => {
    let rec go = (skel: Skel.t): Term.t => {
      let root = List.nth(tiles, Skel.root_index(skel));
      switch (skel) {
      | Op(_) => Op(T.get_op(root))
      | Pre(_, r) => Pre(T.get_pre(root), go(r))
      | Post(l, _) => Post(go(l), T.get_post(root))
      | Bin(l, _, r) => Bin(go(l), T.get_bin(root), go(r))
      };
    };
    go(mk_skel(tiles));
  };
  let rec dissociate = (tm: Term.t): tiles =>
    switch (tm) {
    | Op(op) => [Op(op)]
    | Pre(pre, r) => [Pre(pre), ...dissociate(r)]
    | Post(l, post) => dissociate(l) @ [Post(post)]
    | Bin(l, bin, r) => dissociate(l) @ [Bin(bin), ...dissociate(r)]
    };

  let rec sort_s = (tiles: Unsorted.Tile.s): option(list(Tile.t)) =>
    tiles |> List.map(sort) |> OptUtil.sequence
  and sort = (tile: Unsorted.Tile.t) => Input.sort(~sort_and_associate, tile)
  and sort_and_associate = ts => {
    let+ sorted = sort_s(ts);
    associate(sorted);
  };

  let rec unsort_s = (tiles: list(Tile.t)): Unsorted.Tile.s =>
    List.map(unsort, tiles)
  and unsort = (tile: Tile.t): Unsorted.Tile.t =>
    Input.unsort(~dissociate_and_unsort, tile)
  and dissociate_and_unsort = term => unsort_s(dissociate(term));

  let connect = Input.connect;
  let disconnect = Input.disconnect;

  let connect_selection =
    connect_selection(~connect=ts => connect(AltList.map_b(associate, ts)));

  let is_convex = (d: Direction.t) =>
    fun
    | Selection.Tile(tile) => Tile.is_convex(d, tile)
    | Tessera(tessera) => Unsorted.Tessera.is_convex(d, tessera);
  let is_hole =
    fun
    | Selection.Tile(Op(op)) => Term.is_op_hole(op)
    | Tile(Bin(bin)) => Term.is_bin_hole(bin)
    | _ => false;

  let fix_empty_holes_between =
      (prefix: selection, suffix: selection): (selection, selection) =>
    switch (ListUtil.split_last_opt(prefix), suffix) {
    | (None, _)
    | (_, []) => (prefix, suffix)
    | (Some((leading, last)), [first, ...trailing]) =>
      let last_is_convex = is_convex(Right, last);
      let first_is_convex = is_convex(Left, first);
      if (last_is_convex != first_is_convex) {
        is_hole(last) && is_hole(first)
          ? (leading, trailing) : (prefix, suffix);
      } else if (is_hole(last)) {
        (leading, suffix);
      } else if (is_hole(first)) {
        (prefix, trailing);
      } else {
        let hole =
          Selection.Tile(
            last_is_convex ? Bin(Term.mk_bin_hole()) : Op(Term.mk_op_hole()),
          );
        (prefix, [hole, ...suffix]);
      };
    };

  let rec fix_empty_holes_end = (~side: Direction.t) =>
    fun
    | [] => []
    | [[], ...selections] =>
      switch (selections) {
      | [] => [[Selection.Tile(Op(Term.mk_op_hole()))]]
      | [_, ..._] => [[], ...fix_empty_holes_end(~side, selections)]
      }
    | [[elem, ...selection'] as selection, ...selections] =>
      if (!is_convex(side, elem) && is_hole(elem)) {
        [
          selection',
          ...switch (selection') {
             | [] => fix_empty_holes_end(~side, selections)
             | [_, ..._] => selections
             },
        ];
      } else {
        let cap =
          is_convex(side, elem)
            ? [] : [Selection.Tile(Op(Term.mk_op_hole()))];
        switch (side) {
        | Left => [cap @ selection, ...selections]
        | Right => [cap, selection, ...selections]
        };
      };

  let fix_empty_holes_left = fix_empty_holes_end(~side=Left);
  let fix_empty_holes_right = selections =>
    selections
    |> List.rev_map(List.rev)
    |> fix_empty_holes_end(~side=Right)
    |> List.rev_map(List.rev);

  let fix_empty_holes = (selections: list(selection)): list(selection) => {
    let rec fix =
            (selection: selection, selections: list(selection))
            : list(selection) => {
      let skip_empty = (selection, selections) => {
        let (selection, selections) =
          ListUtil.split_first(fix(selection, selections));
        [selection, [], ...selections];
      };
      switch (selections) {
      | [] => [selection]
      | [[], ...selections] => skip_empty(selection, selections)
      | [[_, ..._] as selection', ...selections] =>
        let (selection, selection') =
          fix_empty_holes_between(selection, selection');
        switch (selection') {
        | [] => skip_empty(selection, selections)
        | [_, ..._] => [selection, selection', ...selections]
        };
      };
    };
    let fixed_between = List.fold_right(fix, selections, []);
    fix_empty_holes_left(fix_empty_holes_right(fixed_between));
  };
};
