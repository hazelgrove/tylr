open Util;
open OptUtil.Syntax;

module type S_INPUT = {
  module Tm: Term.S;
  module T: Tile.S with module Tm := Tm;
  module F: Frame.S with module Tm := Tm;

  let sort:
    (~sort_and_associate: Unsorted.Tile.s => option(Tm.t), Unsorted.Tile.t) =>
    option(T.t);
  let unsort:
    (~dissociate_and_unsort: Tm.t => Unsorted.Tile.s, T.t) => Unsorted.Tile.t;

  // TODO return option?
  let assemble_tile: AltList.t(Unsorted.Tessera.t, Tm.t) => T.t;
  let disassemble_tile: T.t => AltList.t(Unsorted.Tessera.t, Tm.t);

  let assemble_open_frame:
    (
      ~associate: list(T.t) => Tm.t,
      ZZList.t(AltList.b_frame(Unsorted.Tessera.t, Tm.t), T.t),
      F.t
    ) =>
    option(F.open_);
  let disassemble_open_frame:
    (~dissociate: Tm.t => list(T.t), F.open_) =>
    (ZZList.t(AltList.b_frame(Unsorted.Tessera.t, Tm.t), T.t), F.t);
};

module type S = {
  module Tm: Term.S;
  module T: Tile.S with module Tm := Tm;
  module F: Frame.S with module Tm := Tm;

  let mk_skel: list(T.t) => Skel.t;
  let term_of_skel: (Skel.t, list(T.t)) => ZZList.t(Tm.t, T.t);

  let associate: list(T.t) => Tm.t;
  let dissociate: Tm.t => list(T.t);

  let sort_s: Unsorted.Tile.s => option(list(T.t));
  let sort: Unsorted.Tile.t => option(T.t);
  let unsort_s: list(T.t) => Unsorted.Tile.s;
  let unsort: T.t => Unsorted.Tile.t;

  let sort_and_associate: Unsorted.Tile.s => option(Tm.t);
  let dissociate_and_unsort: Tm.t => Unsorted.Tile.s;

  // raises Invalid_argument if ...
  let assemble_tile: AltList.t(Unsorted.Tessera.t, Tm.t) => T.t;
  let disassemble_tile: T.t => AltList.t(Unsorted.Tessera.t, Tm.t);

  // legit total, may return same selection back
  let assemble_tiles_in_selection: Selection.t(T.t) => Selection.t(T.t);
  let fix_empty_holes: list(Selection.t(T.t)) => list(Selection.t(T.t));

  let associate_frame: ((list(T.t), list(T.t)), F.bidelimited) => F.t;
  let dissociate_frame: F.t => ((list(T.t), list(T.t)), F.bidelimited);

  // 1 + ( 2 + [let x =] __3 + 4__ [in] x )
  let assemble_open_frame:
    (
      ZZList.t(AltList.b_frame(Unsorted.Tessera.t, Tm.t), T.t),
      F.bidelimited
    ) =>
    option(F.open_);
  let disassemble_open_frame:
    F.open_ =>
    (
      ZZList.t(AltList.b_frame(Unsorted.Tessera.t, Tm.t), T.t),
      F.bidelimited,
    );
};

type hole_patch =
  | RemoveBoth
  | RemoveLeft
  | RemoveRight
  | InsertOp
  | InsertBin;

let fix_holes_at_juncture =
    (
      ~left_is_hole=false,
      ~right_is_hole=false,
      ~left_is_convex: bool,
      ~right_is_convex: bool,
      (),
    )
    : option(hole_patch) =>
  if (left_is_convex != right_is_convex) {
    left_is_hole && right_is_hole ? Some(RemoveBoth) : None;
  } else if (left_is_hole) {
    Some(RemoveLeft);
  } else if (right_is_hole) {
    Some(RemoveRight);
  } else {
    left_is_convex ? Some(InsertBin) : Some(InsertOp);
  };

// outside of functor for use by unsorted selections
let assemble_tiles_in_selection =
    (
      ~assemble_tile: AltList.t(Unsorted.Tessera.t, list('tile)) => 'tile,
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
        let tile = assemble_tile((open_, [(ts, tessera)]));
        Some((tile, selection));
      } else {
        let* (tile, selection) = go_opening(open_, selection);
        go_opening(~rev_tiles=[tile, ...rev_tiles], open_, selection);
      }
    };
  go(selection);
};

module Make =
       (
         Tm: Term.S,
         T: Tile.S with module Tm := Tm,
         F: Frame.S with module Tm := Tm,
         Input:
           S_INPUT with module Tm := Tm and module T := T and module F := F,
       )
       : (S with module Tm := Tm and module T := T and module F := F) => {
  type selection = Selection.t(T.t);
  type tiles = list(T.t);

  type itile = (int, T.t);
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
          T.precedence(tile) <= T.precedence(post)
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
          T.precedence(tile) <= T.precedence(bin)
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

  let term_of_skel = (skel: Skel.t, ts: tiles): ZZList.t(Tm.t, T.t) => {
    let (a, b) = Skel.range(skel);
    let prefix = ListUtil.sublist((0, a), ts);
    let suffix = ListUtil.sublist((b, List.length(ts)), ts);
    let rec go = (skel: Skel.t): Tm.t => {
      let root = List.nth(ts, Skel.root_index(skel));
      switch (skel) {
      | Op(_) => Op(Tile.get_op(root))
      | Pre(_, r) => Pre(Tile.get_pre(root), go(r))
      | Post(l, _) => Post(go(l), Tile.get_post(root))
      | Bin(l, _, r) => Bin(go(l), Tile.get_bin(root), go(r))
      };
    };
    (prefix, go(skel), suffix);
  };

  let associate = (tiles: tiles): Tm.t => {
    let (_, term, _) = term_of_skel(mk_skel(tiles), tiles);
    term;
  };
  let rec dissociate = (tm: Tm.t): tiles =>
    switch (tm) {
    | Op(op) => [Op(op)]
    | Pre(pre, r) => [Pre(pre), ...dissociate(r)]
    | Post(l, post) => dissociate(l) @ [Post(post)]
    | Bin(l, bin, r) => dissociate(l) @ [Bin(bin), ...dissociate(r)]
    };

  let rec sort_s = (tiles: Unsorted.Tile.s): option(list(T.t)) =>
    tiles |> List.map(sort) |> OptUtil.sequence
  and sort = (tile: Unsorted.Tile.t) => Input.sort(~sort_and_associate, tile)
  and sort_and_associate = ts => {
    let+ sorted = sort_s(ts);
    associate(sorted);
  };

  let rec unsort_s = (tiles: list(T.t)): Unsorted.Tile.s =>
    List.map(unsort, tiles)
  and unsort = (tile: T.t): Unsorted.Tile.t =>
    Input.unsort(~dissociate_and_unsort, tile)
  and dissociate_and_unsort = term => unsort_s(dissociate(term));

  let assemble_tile = Input.assemble_tile;
  let disassemble_tile = Input.disassemble_tile;

  let assemble_tiles_in_selection =
    assemble_tiles_in_selection(~assemble_tile=ts =>
      assemble_tile(AltList.map_b(associate, ts))
    );

  let is_convex = (d: Direction.t) =>
    fun
    | Selection.Tile(tile) => Tile.is_convex(d, tile)
    | Tessera(tessera) => Unsorted.Tessera.is_convex(d, tessera);
  let is_hole =
    fun
    | Selection.Tile(Op(op)) => Tm.is_op_hole(op)
    | Tile(Bin(bin)) => Tm.is_bin_hole(bin)
    | _ => false;

  let fix_empty_holes_between =
      (prefix: selection, suffix: selection): (selection, selection) =>
    switch (ListUtil.split_last_opt(prefix), suffix) {
    | (None, _)
    | (_, []) => (prefix, suffix)
    | (Some((leading, last)), [first, ...trailing]) =>
      switch (
        fix_holes_at_juncture(
          ~left_is_hole=is_hole(last),
          ~right_is_hole=is_hole(first),
          ~left_is_convex=is_convex(Right, last),
          ~right_is_convex=is_convex(Left, first),
          (),
        )
      ) {
      | None => (prefix, suffix)
      | Some(RemoveBoth) => (leading, trailing)
      | Some(RemoveLeft) => (leading, suffix)
      | Some(RemoveRight) => (prefix, trailing)
      | Some(InsertOp) => (
          prefix,
          [Selection.Tile(Op(Tm.mk_op_hole())), ...suffix],
        )
      | Some(InsertBin) => (
          prefix,
          [Selection.Tile(Bin(Tm.mk_bin_hole())), ...suffix],
        )
      }
    };

  let rec fix_empty_holes_end = (~side: Direction.t) =>
    fun
    | [] => []
    | [[], ...selections] =>
      switch (selections) {
      | [] => [[Selection.Tile(Op(Tm.mk_op_hole()))]]
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
            ? [] : [Selection.Tile(Op(Tm.mk_op_hole()))];
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

  let associate_frame = ((prefix, suffix), frame: F.bidelimited) => {
    let n = List.length(prefix);
    let dummy_hole = Tile.Op(Tm.mk_op_hole());
    let tiles = prefix @ [dummy_hole, ...suffix];
    let rec go = (skel, frame: F.t) => {
      let tile = List.nth(tiles, Skel.root_index(skel));
      switch (skel) {
      | Skel.Op(_) => frame
      | Pre(_, r) => go(r, Uni(Pre_r(Tile.get_pre(tile), frame)))
      | Post(l, _) => go(l, Uni(Post_l(frame, Tile.get_post(tile))))
      | Bin(l, m, r) =>
        if (n < m) {
          let (_, r, _) = term_of_skel(r, tiles);
          go(l, Uni(Bin_l(frame, Tile.get_bin(tile), r)));
        } else {
          let (_, l, _) = term_of_skel(l, tiles);
          go(r, Uni(Bin_r(l, Tile.get_bin(tile), frame)));
        }
      };
    };
    go(mk_skel(tiles), Bi(frame));
  };

  let dissociate_frame = (frame: F.t) => {
    let rec go = (~prefix=[], ~suffix=[], frame: F.t) =>
      switch (frame) {
      | Bi(bidelimited) => ((prefix, suffix), bidelimited)
      | Uni(unidelimited) =>
        switch (unidelimited) {
        | Pre_r(pre, frame) =>
          go(~prefix=prefix @ [Tile.Pre(pre)], ~suffix, frame)
        | Post_l(frame, post) =>
          go(~prefix, ~suffix=[Tile.Post(post), ...suffix], frame)
        | Bin_l(frame, bin, r) =>
          go(
            ~prefix,
            ~suffix=[Tile.Bin(bin), ...dissociate(r)] @ suffix,
            frame,
          )
        | Bin_r(l, bin, frame) =>
          go(
            ~prefix=prefix @ dissociate(l) @ [Tile.Bin(bin)],
            ~suffix,
            frame,
          )
        }
      };
    go(frame);
  };

  let assemble_open_frame =
      (
        (prefix, ts, suffix):
          ZZList.t(AltList.b_frame(Unsorted.Tessera.t, Tm.t), T.t),
        frame: F.bidelimited,
      )
      : option(F.open_) => {
    let dummy_tile = {
      let dummy_hole = Term.Op(Tm.mk_op_hole());
      assemble_tile(AltList.fill_b_frame(dummy_hole, ts));
    };
    let n = List.length(prefix);
    let tiles = prefix @ [dummy_tile, ...suffix];
    let assemble = Input.assemble_open_frame(~associate);
    let rec go = (skel: Skel.t, frame: F.t) => {
      let t = List.nth(tiles, Skel.root_index(skel));
      switch (skel) {
      | Op(m) =>
        assert(n == m);
        assemble(([], ts, []), frame);
      | Pre(m, r) =>
        if (n == m) {
          let r = ListUtil.sublist(Skel.range(r), tiles);
          assemble(([], ts, r), frame);
        } else {
          go(r, Uni(Pre_r(Tile.get_pre(t), frame)));
        }
      | Post(l, m) =>
        if (n == m) {
          let l = ListUtil.sublist(Skel.range(l), tiles);
          assemble((l, ts, []), frame);
        } else {
          go(l, Uni(Post_l(frame, Tile.get_post(t))));
        }
      | Bin(l, m, r) =>
        let bin = Tile.get_bin(t);
        if (n < m) {
          let (_, r, _) = term_of_skel(r, tiles);
          go(l, Uni(Bin_l(frame, bin, r)));
        } else if (n > m) {
          let (_, l, _) = term_of_skel(l, tiles);
          go(r, Uni(Bin_r(l, bin, frame)));
        } else {
          let l = ListUtil.sublist(Skel.range(l), tiles);
          let r = ListUtil.sublist(Skel.range(r), tiles);
          assemble((l, ts, r), frame);
        };
      };
    };
    go(mk_skel(tiles), Bi(frame));
  };

  let disassemble_open_frame = (frame: F.open_) => {
    let ((prefix, disassembled, suffix), frame) =
      Input.disassemble_open_frame(~dissociate, frame);
    let ((outer_prefix, outer_suffix), frame) = dissociate_frame(frame);
    ((outer_prefix @ prefix, disassembled, suffix @ outer_suffix), frame);
  };
};
