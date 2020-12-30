open Sexplib.Std;
open Util;
open OptUtil.Syntax;

[@deriving sexp]
type tile_shape =
  | Num
  | Bool
  | NumLit(int)
  | Var(Var.t)
  | Paren
  | Lam
  | Let
  | Ap
  | Ann
  | Plus
  | Arrow;

module type COMMON_INPUT = {
  module T: Tile.S;

  type tile_construction =
    | Wraps(T.s => T.t, ZPath.child_step)
    | DoesNotWrap(T.t);

  let construction_of_shape: tile_shape => option(tile_construction);
};

[@deriving sexp]
type t =
  | Mark
  | Move(Direction.t)
  | Delete(Direction.t)
  | Construct(tile_shape);

module Common = (T: Tile.S, M: COMMON_INPUT with module T := T) => {
  module Ts = Tiles.Make(T);

  let construct = (s: tile_shape, j: ZPath.caret_step, ts: T.s) => {
    let+ construction = M.construction_of_shape(s);
    switch (construction) {
    | DoesNotWrap(tile) =>
      let (prefix, suffix) = ListUtil.split_n(j, ts);
      let (prefix, tile, suffix) =
        ListUtil.take_3(Ts.fix_empty_holes([prefix, [tile], suffix]));
      (([], List.length(prefix @ tile)), prefix @ tile @ suffix);
    | Wraps(wrap, child_step) =>
      if (j == List.length(ts)) {
        let body = Ts.mk_hole();
        let tile = wrap(body);
        let (ts, tile) = ListUtil.take_2(Ts.fix_empty_holes([ts, [tile]]));
        (([(List.length(ts), child_step)], 0), ts @ tile);
      } else {
        let (prefix, wrapped, suffix) = ListUtil.split_nth(j, ts);
        let tile = wrap(List.hd(Ts.fix_empty_holes([[wrapped]])));
        let (prefix, tile, suffix) =
          ListUtil.take_3(Ts.fix_empty_holes([prefix, [tile], suffix]));
        (
          ([(List.length(prefix), child_step)], 0),
          prefix @ tile @ suffix,
        );
      }
    };
  };
};

module Typ = {
  module M = {
    type tile_construction =
      | Wraps(HTyp.t => HTyp.T.t, ZPath.child_step)
      | DoesNotWrap(HTyp.T.t);

    let construction_of_shape: tile_shape => option(tile_construction) =
      fun
      | NumLit(_)
      | Lam
      | Let
      | Ap
      | Plus
      | Var(_)
      | Ann => None
      | Num => Some(DoesNotWrap(Op(Num)))
      | Bool => Some(DoesNotWrap(Op(Bool)))
      | Paren => Some(Wraps(body => Op(Paren(body)), 0))
      | Arrow => Some(DoesNotWrap(Bin(Arrow)));
  };
  include Common(HTyp.T, M);
};
module Pat = {
  module M = {
    type tile_construction =
      | Wraps(HPat.t => HPat.T.t, ZPath.child_step)
      | DoesNotWrap(HPat.T.t);

    let construction_of_shape: tile_shape => option(tile_construction) =
      fun
      | Num
      | Bool
      | NumLit(_)
      | Lam
      | Let
      | Ap
      | Plus
      | Arrow => None
      | Var(x) => Some(DoesNotWrap(Op(Var(x))))
      | Paren => Some(Wraps(body => Op(Paren(body)), 0))
      // TODO come back to Ann, consider generalizing output to list of tiles
      // so that bidelimited wrapping is not the only possible outcome on a selection
      | Ann => Some(DoesNotWrap(Post(Ann(NotInHole, HTyp.mk_hole()))));
  };
  include Common(HPat.T, M);
};
module Exp = {
  module M = {
    type tile_construction =
      | Wraps(HExp.t => HExp.T.t, ZPath.child_step)
      | DoesNotWrap(HExp.T.t);

    let construction_of_shape: tile_shape => option(tile_construction) =
      fun
      | Num
      | Bool
      | Ann
      | Arrow => None
      | NumLit(n) => Some(DoesNotWrap(Op(Num(NotInHole, n))))
      | Var(x) => Some(DoesNotWrap(Op(Var(NotInHole, x))))
      | Paren => Some(Wraps(body => Op(Paren(body)), 0))
      // TODO come back to Lam, consider generalizing output to list of tiles
      // so that bidelimited wrapping is not the only possible outcome on a selection
      | Lam => Some(DoesNotWrap(Pre(Lam(NotInHole, HPat.mk_hole()))))
      | Let => Some(Wraps(def => Pre(Let(HPat.mk_hole(), def)), 0))
      | Ap => Some(Wraps(arg => Post(Ap(NotInHole, arg)), 0))
      | Plus => Some(DoesNotWrap(Bin(Plus(NotInHole))));
  };
  include Common(HExp.T, M);
};

let rec move_selecting =
        (
          d: Direction.t,
          {anchor, focus, _} as selection: ZPath.anchored_selection,
          zipper: EditState.Zipper.t,
        )
        : option((ZPath.anchored_selection, EditState.Zipper.t)) => {
  let anchor_sort = EditState.Zipper.sort_at(anchor, zipper);
  let (_, focus_side) = ZPath.mk_ordered_selection(selection);
  let* (next_focus, did_it_zip) = EditState.Zipper.move(d, focus, zipper);
  let (selection, zipper) =
    switch (did_it_zip) {
    | None => ({...selection, focus: next_focus}, zipper)
    | Some((two_step, zipper)) => (
        {
          ...ZPath.cons_anchored_selection(two_step, selection),
          focus: next_focus,
        },
        zipper,
      )
    };
  let next_sort = EditState.Zipper.sort_at(next_focus, zipper);
  let c = Sort.compare(next_sort, anchor_sort);
  if ((anchor == focus || d == focus_side) && c > 0) {
    let+ (selection, zipper) =
      move_selecting(
        Direction.toggle(d),
        {...selection, anchor: selection.focus, focus: selection.anchor},
        zipper,
      );
    (
      {...selection, anchor: selection.focus, focus: selection.anchor},
      zipper,
    );
  } else if ((anchor == focus || d == focus_side) && c < 0) {
    move_selecting(d, selection, zipper);
  } else if (d != focus_side && c < 0) {
    let rec move_until_same_sort = anchor => {
      // assuming movement should never cause zip up here, review assumption
      let (next_anchor, _) =
        EditState.Zipper.move(Direction.toggle(d), anchor, zipper)
        |> OptUtil.get(() => assert(false));
      if (Sort.compare(
            EditState.Zipper.sort_at(next_anchor, zipper),
            next_sort,
          )
          == 0) {
        next_anchor;
      } else {
        move_until_same_sort(next_anchor);
      };
    };
    let next_anchor = move_until_same_sort(selection.anchor);
    Some(({...selection, anchor: next_anchor}, zipper));
  } else {
    Some((selection, zipper));
  };
};

let rec perform_normal =
        (a: t, (steps, j) as focus: ZPath.t, zipper: EditState.Zipper.t)
        : option(EditState.t) =>
  switch (a) {
  | Mark => Some((Selecting({origin: focus, anchor: focus, focus}), zipper))

  | Move(d) =>
    let+ (focus, did_it_zip) = EditState.Zipper.move(d, focus, zipper);
    let mode = EditState.Mode.Normal(focus);
    switch (did_it_zip) {
    | None => (mode, zipper)
    | Some((two_step, zip_result)) => (
        EditState.Mode.update_anchors(ZPath.cons(two_step), mode),
        zip_result,
      )
    };

  | Delete(d) =>
    let* (selection, zipper) =
      move_selecting(d, {origin: focus, anchor: focus, focus}, zipper);
    let (((steps_l, _) as l, (steps_r, _)) as selection, _) =
      ZPath.mk_ordered_selection(selection);
    if (steps_l == steps_r) {
      let+ (path, zipper) =
        EditState.Zipper.delete_selection(selection, zipper);
      (EditState.Mode.Normal(path), zipper);
    } else {
      Some((EditState.Mode.Restructuring(selection, l), zipper));
    };

  | Construct(s) =>
    switch (steps) {
    | [two_step, ...steps] =>
      let zipper = EditState.Zipper.unzip(two_step, zipper);
      perform_normal(a, (steps, j), zipper);
    | [] =>
      switch (zipper) {
      | `Typ(ty, unzipped) =>
        let+ (path, ty) = Typ.construct(s, j, ty);
        (EditState.Mode.Normal(path), `Typ((ty, unzipped)));
      | `Pat(p, unzipped) =>
        let* (path, p) = Pat.construct(s, j, p);
        let+ zipper = ZInfo.Pat.fix_holes((p, unzipped));
        (EditState.Mode.Normal(path), `Pat(zipper));
      | `Exp(e, unzipped) =>
        let* (path, e) = Exp.construct(s, j, e);
        let+ zipper = ZInfo.Exp.fix_holes((e, unzipped));
        (EditState.Mode.Normal(path), `Exp(zipper));
      }
    }
  };

let perform_selecting =
    (a: t, selection: ZPath.anchored_selection, zipper: EditState.Zipper.t)
    : option(EditState.t) =>
  switch (a) {
  | Delete(_)
  | Mark =>
    let (ordered, _) = ZPath.mk_ordered_selection(selection);
    Some((Restructuring(ordered, fst(ordered)), zipper));

  | Move(d) =>
    let+ (selection, zipper) = move_selecting(d, selection, zipper);
    (EditState.Mode.Selecting(selection), zipper);

  | Construct(_) =>
    // TODO
    None
  };

let perform_restructuring =
    (
      a: t,
      (l, r) as selection: ZPath.ordered_selection,
      (steps_t, j_t) as target: ZPath.t,
      zipper: EditState.Zipper.t,
    )
    : option(EditState.t) =>
  switch (a) {
  | Mark =>
    let+ (path, zipper) =
      EditState.Zipper.restructure(selection, target, zipper);
    (EditState.Mode.Normal(path), zipper);
  | Move(d) =>
    let ((steps_l, j_l), (steps_r, j_r)) = (l, r);
    // assuming now that restructuring mode is only
    // entered when selection contains unmatched delim
    let j_t = j_t + Direction.sign(d);
    if (steps_t == steps_l
        && 0 <= j_t
        && j_t <= j_l
        || steps_t == steps_r
        && j_r <= j_t
        && j_t <= EditState.Zipper.length_at(steps_r, zipper)) {
      Some((Restructuring(selection, (steps_t, j_t)), zipper));
    } else if (steps_t == steps_l && j_t > j_l) {
      Some((Restructuring(selection, r), zipper));
    } else if (steps_t == steps_r && j_t < j_r) {
      Some((Restructuring(selection, l), zipper));
    } else {
      None;
    };
  | Delete(_) =>
    let+ (path, zipper) =
      EditState.Zipper.delete_selection(selection, zipper);
    (EditState.Mode.Normal(path), zipper);
  | Construct(_) => None
  };

let perform = (a: t, (mode, zipper): EditState.t): option(EditState.t) =>
  switch (mode) {
  | Normal(focus) => perform_normal(a, focus, zipper)
  | Selecting(selection) => perform_selecting(a, selection, zipper)
  | Restructuring(selection, target) =>
    perform_restructuring(a, selection, target, zipper)
  };
