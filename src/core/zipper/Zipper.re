open Util;
open Sexplib.Std;

[@deriving show]
type caret =
  | Outer
  | Inner(int);

// assuming single backpack, shards may appear in selection, backpack, or siblings
[@deriving show]
type t = {
  id_gen: IdGen.t,
  selection: Selection.t,
  backpack: Backpack.t,
  relatives: Relatives.t,
  caret,
};

module Action = {
  [@deriving (show, sexp)]
  type t =
    | Move(Direction.t)
    | Select(Direction.t)
    | Destruct(Direction.t)
    | Insert(string)
    // `Construct(d, lbl)` constructs `lbl` starting from `d` side
    //| Construct(Direction.t, Tile.Label.t)
    | Pick_up
    | Put_down;

  module Failure = {
    [@deriving (show, sexp)]
    type t =
      | Cant_move
      | Cant_insert
      | Cant_put_down_inside_token
      | Cant_put_down;
  };

  module Result = {
    include Result;
    type t('success) = Result.t('success, Failure.t);
  };
};

let update_caret = (f: caret => caret, z: t): t => {
  ...z,
  caret: f(z.caret),
};
let set_caret = (caret: caret) => update_caret(_ => caret);
let update_relatives = (f: Relatives.t => Relatives.t, z: t): t => {
  ...z,
  relatives: f(z.relatives),
};
let update_siblings: (Siblings.t => Siblings.t, t) => t =
  f => update_relatives(rs => {...rs, siblings: f(rs.siblings)});

let remove_right_sib: t => t =
  update_siblings(((l, r)) => (l, r == [] ? [] : List.tl(r)));

let remove_left_sib: t => t =
  update_siblings(((l, r)) => (l == [] ? [] : List.tl(l), r));

let neighbor_monotiles = siblings =>
  switch (Siblings.neighbors(siblings)) {
  | (Some(l), Some(r)) => (Piece.monotile(l), Piece.monotile(r))
  | (Some(l), None) => (Piece.monotile(l), None)
  | (None, Some(r)) => (None, Piece.monotile(r))
  | (None, None) => (None, None)
  };

let unselect = (z: t): t => {
  let relatives =
    z.relatives
    |> Relatives.prepend(z.selection.focus, z.selection.content)
    |> Relatives.reassemble;
  let selection = Selection.clear(z.selection);
  {...z, selection, relatives};
};

let zip = (z: t): Segment.t => {
  let z = unselect(z);
  let (pre, suf) = Relatives.disassemble(z.relatives);
  Segment.reassemble(List.rev(pre) @ suf);
};

let convex = (z: t): bool => Segment.convex(zip(z));

let remold_regrout = (z: t): t => {
  assert(Selection.is_empty(z.selection));
  let ls_relatives =
    Relatives.remold(z.relatives)
    |> List.sort((rel, rel') => {
         open Relatives;
         let c = Int.compare(sort_rank(rel), sort_rank(rel'));
         c != 0 ? c : Int.compare(shape_rank(rel), shape_rank(rel'));
       });
  assert(ls_relatives != []);
  let relatives = ls_relatives |> List.hd |> Relatives.regrout;
  {...z, relatives};
};

let update_selection = (selection: Selection.t, z: t): (Selection.t, t) => {
  let old = z.selection;
  // used to be necessary to unselect when selection update
  // included remold/regrout, now no longer necessary if needs
  // to be changed but keeping for now to minimize change
  let z = unselect({...z, selection});
  (old, z);
};

let put_selection = (sel, z) => snd(update_selection(sel, z));

let grow_selection = (z: t): option(t) => {
  open OptUtil.Syntax;
  let+ (p, relatives) =
    Relatives.pop(~balanced=false, z.selection.focus, z.relatives);
  let selection = Selection.push(p, z.selection);
  {...z, selection, relatives};
};

// toggles focus and grows if selection is empty
let shrink_selection = (z: t): option(t) => {
  switch (Selection.pop(z.selection)) {
  | None =>
    let selection = Selection.toggle_focus(z.selection);
    grow_selection({...z, selection});
  | Some((p, selection)) =>
    let relatives = Relatives.push(selection.focus, p, z.relatives);
    Some({...z, selection, relatives});
  };
};

let move_outer = (d: Direction.t, z: t): option(t) =>
  if (Selection.is_empty(z.selection)) {
    open OptUtil.Syntax;
    // let balanced = !Backpack.is_balanced(z.backpack);
    let+ (p, relatives) = Relatives.pop(~balanced=false, d, z.relatives);
    let relatives =
      relatives
      |> Relatives.push(Direction.toggle(d), p)
      |> Relatives.reassemble;
    {...z, relatives};
  } else {
    let selection = {...z.selection, focus: Direction.toggle(d)};
    Some(unselect({...z, selection}));
  };

let select_outer = (d: Direction.t, z: t): option(t) =>
  d == z.selection.focus ? grow_selection(z) : shrink_selection(z);

let pick_up = (z: t): t => {
  let (selected, z) = update_selection(Selection.empty, z);
  let selections =
    selected.content
    |> Segment.split_by_grout
    |> Aba.get_as
    |> List.filter((!=)(Segment.empty))
    |> List.map(Selection.mk(selected.focus));
  let backpack =
    Backpack.push_s(
      (z.selection.focus == Left ? Fun.id : List.rev)(selections),
      z.backpack,
    );
  {...z, backpack};
};

let destruct_outer = (z: t): t => {
  let (selected, z) = update_selection(Selection.empty, z);
  let (to_pick_up, to_remove) =
    Segment.shards(selected.content)
    |> List.partition(shard =>
         Siblings.contains_matching(shard, z.relatives.siblings)
       );
  let backpack =
    z.backpack
    |> Backpack.remove_matching(to_remove)
    |> Backpack.push_s(
         to_pick_up
         |> List.map(Segment.of_shard)
         |> List.map(Selection.mk(z.selection.focus)),
       );
  {...z, backpack};
};

let put_down = (z: t): option(t) => {
  open OptUtil.Syntax;
  let z = destruct_outer(z);
  let+ (_, popped, backpack) =
    Backpack.pop(Siblings.shards(z.relatives.siblings), z.backpack);
  {...z, backpack} |> put_selection(popped) |> unselect;
};

let grout_between: Siblings.t => Piece.t =
  fun
  | ([], []) => failwith("insert_space_grout: impossible")
  | ([], [_, ..._]) => Base.Piece.Grout((Convex, Nib.Shape.concave()))
  | ([p, ..._], _) => {
      let nib_shape_r = p |> Piece.nib_shapes |> snd;
      Grout((Nib.Shape.flip(nib_shape_r), nib_shape_r));
    };

let insert_space_grout =
    (_char: string, {relatives: {siblings, _}, _} as z: t) =>
  if (Siblings.has_space_neighbor(siblings)) {
    z;
  } else {
    update_siblings(((l, r)) => ([grout_between(siblings)] @ l, r), z)
    |> update_relatives(Relatives.regrout); //TODO(andrew): david is this necessary?
  };

let construct = (from: Direction.t, label: Tile.Label.t, z: t): t => {
  switch (label) {
  | [t] when Token.is_whitespace(t) => insert_space_grout(t, z)
  | _ =>
    let z = destruct_outer(z);
    let molds = Molds.get(label);
    assert(molds != []);
    // initial mold to typecheck, will be remolded
    let mold = List.hd(molds);
    let (id, id_gen) = IdGen.next(z.id_gen);
    let selections =
      Shard.mk_s(id, label, mold)
      |> List.map(Segment.of_shard)
      |> List.map(Selection.mk(from))
      |> ListUtil.rev_if(from == Right);
    let backpack = Backpack.push_s(selections, z.backpack);
    Option.get(put_down({...z, id_gen, backpack}));
  };
};

let replace_construct = (d: Direction.t, l: Tile.Label.t, z: t): option(t) =>
  z
  |> select_outer(d)
  |> Option.map(destruct_outer)
  |> Option.map(construct(d, l));

let can_merge_through: Base.Piece.t => bool =
  p => Piece.is_grout(p) || Piece.is_length_one_monotile(p);

let shift_siblings_maybe = (d: Direction.t, (l_sibs, r_sibs)) =>
  /* This is a bit of a hack. If we are Deleting (d==Right),
     we move the first right sibling to the left so the
     arrangement is the same as that used for Backspacing (d==Left).
     We do this both for checking mergability and doing the merge */
  d == Left || r_sibs == []
    ? (l_sibs, r_sibs)
    : (List.cons(List.hd(r_sibs), l_sibs), List.tl(r_sibs));

let merge_candidates:
  (Direction.t, caret, Siblings.t) => option((Token.t, Token.t)) =
  (d, caret, siblings) => {
    let (l_sibs, r_sibs) = shift_siblings_maybe(d, siblings);
    switch (l_sibs) {
    | [l_nhbr, ...ps] when can_merge_through(l_nhbr) =>
      switch (caret, neighbor_monotiles((ps, r_sibs))) {
      | (Outer, (Some(l_2nd_nhbr), Some(r_nhbr)))
          when Token.is_valid(l_2nd_nhbr ++ r_nhbr) =>
        Some((l_2nd_nhbr, r_nhbr))
      | _ => None
      }
    | _ => None
    };
  };

/* merge precondition: ... <Monotile> <DontCare> | <Monotile> ... */
let merge = ((l, r): (Token.t, Token.t), z: t): option(t) =>
  z
  |> set_caret(Inner(String.length(l) - 1))
  |> move_outer(Right)
  |> OptUtil.and_then(select_outer(Left))
  |> OptUtil.and_then(select_outer(Left))
  |> OptUtil.and_then(select_outer(Left))
  |> Option.map(destruct_outer)
  |> Option.map(construct(Right, [l ++ r]));

let decrement_caret: caret => caret =
  fun
  | Outer
  | Inner(0) => Outer
  | Inner(k) => Inner(k - 1);

let last_inner_pos = t => String.length(t) - 2;

let destruct =
    (
      d: Direction.t,
      {caret, relatives: {siblings: (l_sibs, r_sibs), _}, _} as z: t,
    )
    : option(t) => {
  /* Could add checks on valid tokens (all of these hold assuming substring) */
  let d_outer = z => z |> select_outer(d) |> Option.map(destruct_outer);
  switch (d, caret, neighbor_monotiles((l_sibs, r_sibs))) {
  | (Left, Inner(n), (_, Some(t))) =>
    z
    |> update_caret(decrement_caret)
    |> replace_construct(Right, [StringUtil.remove_nth(n, t)])
  | (Right, Inner(n), (_, Some(t))) =>
    z
    |> replace_construct(Right, [StringUtil.remove_nth(n + 1, t)])
    |> OptUtil.and_then(
         n == last_inner_pos(t)
           ? z => z |> set_caret(Outer) |> move_outer(Right) : Option.some,
       )
  | (_, Inner(_), (_, None)) => failwith("destruct: impossible")
  | (Left, Outer, (Some(t), _)) when String.length(t) > 1 =>
    replace_construct(Left, [StringUtil.remove_last(t)], z)
  | (Right, Outer, (_, Some(t))) when String.length(t) > 1 =>
    replace_construct(Right, [StringUtil.remove_first(t)], z)
  | (_, Outer, (Some(_), _)) /* t.length == 1 */
  | (_, Outer, (None, _)) => d_outer(z)
  };
};

let destruct_or_merge =
    (d: Direction.t, {caret, relatives: {siblings, _}, _} as z: t)
    : option(t) =>
  switch (merge_candidates(d, caret, siblings)) {
  | None => destruct(d, z)
  | Some(candidates) =>
    z |> update_siblings(shift_siblings_maybe(d)) |> merge(candidates)
  };

let instant_completion: (string, Direction.t) => (list(Token.t), Direction.t) =
  (s, direction_preference) =>
    /* Completions which can or must be executed immediately */
    //TODO(andrew): refactor to depend on Forms.re
    switch (s) {
    | "(" => (["(", ")"], Left)
    | ")" => (["(", ")"], Right)
    | "[" => (["[", "]"], Left)
    | "]" => (["[", "]"], Right)
    | "?" => (["?", ":"], Left)
    | "=>" => (["fun", "=>"], Right) /* Must as => not monotile */
    | t => ([t], direction_preference)
    };

let delayed_completion: (string, Direction.t) => (list(Token.t), Direction.t) =
  (s, direction_preference) =>
    /* Completions which must be defered as they are ambiguous prefixes */
    //TODO(andrew): refactor to depend on Forms.re
    switch (s) {
    | "fun" => (["fun", "=>"], Left)
    | "in" => (["let", "=", "in"], Right)
    | "let" => (["let", "=", "in"], Left)
    | t => ([t], direction_preference)
    };

let keyword_expand = (z: t): option(t) =>
  /* NOTE(andrew): We may want to allow editing of shards when only 1 of set
     is down (removing the rest of the set from backpack on edit) as something
     like this is necessary for backspace to act as undo after kw-expansion */
  switch (neighbor_monotiles(z.relatives.siblings)) {
  | (Some(kw), _) =>
    let (new_label, direction) = delayed_completion(kw, Left);
    z |> replace_construct(direction, new_label);
  | _ => Some(z)
  };

type appendability =
  | CanAddToLeft(string)
  | CanAddToRight(string)
  | CanAddToNeither;

let sibling_appendability: (string, Siblings.t) => appendability =
  (char, siblings) =>
    switch (neighbor_monotiles(siblings)) {
    | (Some(t), _) when Token.is_valid(t ++ char) => CanAddToLeft(t)
    | (_, Some(t)) when Token.is_valid(char ++ t) => CanAddToRight(t)
    | _ => CanAddToNeither
    };

let barf_or_construct =
    (new_token: string, direction_preference: Direction.t, z: t): t =>
  if (Backpack.is_first_matching(new_token, z.backpack)) {
    z |> put_down |> Option.get;
  } else {
    let (lbl, direction) =
      instant_completion(new_token, direction_preference);
    construct(direction, lbl, z);
  };

let insert_outer =
    (char: string, {relatives: {siblings, _}, _} as z: t): option(t) =>
  switch (sibling_appendability(char, siblings)) {
  | CanAddToNeither =>
    z |> keyword_expand |> Option.map(barf_or_construct(char, Left))
  | CanAddToLeft(left_token) =>
    z
    |> remove_left_sib
    |> barf_or_construct(left_token ++ char, Left)
    |> Option.some
  | CanAddToRight(right_token) =>
    z
    |> remove_right_sib
    |> barf_or_construct(char ++ right_token, Right)
    |> Option.some
  };

let split = (z: t, char: string, idx: int, t: string): option(t) => {
  let (l, r) = StringUtil.split_nth(idx, t);
  let (lbl, direction) = instant_completion(char, Left);
  z
  |> set_caret(Outer)
  |> replace_construct(Right, [r])
  |> Option.map(construct(Left, [l]))
  |> OptUtil.and_then(keyword_expand)
  |> Option.map(construct(direction, lbl));
};

let insert =
    (char: string, {caret, relatives: {siblings, _}, _} as z: t): option(t) =>
  switch (caret, neighbor_monotiles(siblings)) {
  | (Inner(n), (_, Some(t))) =>
    let idx = n + 1;
    let new_t = StringUtil.insert_nth(idx, char, t);
    /* If inserting wouldn't produce a valid token, split */
    Token.is_valid(new_t)
      ? z |> set_caret(Inner(idx)) |> replace_construct(Right, [new_t])
      : split(z, char, idx, t);
  | (Inner(_), (_, None)) =>
    failwith("insert: caret is inner, but left nhbr has no inner positions")
  | (Outer, (_, Some(_))) =>
    let caret =
      /* If we're adding to the right, move caret inside right nhbr */
      switch (sibling_appendability(char, siblings)) {
      | CanAddToRight(_) => Inner(0)
      | CanAddToNeither
      | CanAddToLeft(_) => Outer
      };
    z |> insert_outer(char) |> Option.map(set_caret(caret));
  | (Outer, (_, None)) => insert_outer(char, z)
  };

let move =
    (
      d: Direction.t,
      {caret, relatives: {siblings: (l_sibs, r_sibs), _}, _} as z: t,
    )
    : option(t) =>
  switch (d, caret, neighbor_monotiles((l_sibs, r_sibs))) {
  | (Left, Outer, (Some(t), _)) when String.length(t) > 1 =>
    z |> set_caret(Inner(last_inner_pos(t))) |> move_outer(d)
  | (Left, Outer, _) => move_outer(d, z)
  | (Left, Inner(_), _) => Some(update_caret(decrement_caret, z))
  | (Right, Outer, (_, Some(t))) when String.length(t) > 1 =>
    Some(set_caret(Inner(0), z))
  | (Right, Outer, _) => move_outer(d, z)
  | (Right, Inner(n), (_, Some(t))) when n == last_inner_pos(t) =>
    z |> set_caret(Outer) |> move_outer(d)
  | (Right, Inner(n), _) => Some(set_caret(Inner(n + 1), z))
  };

let select = (d: Direction.t, z: t): option(t) =>
  if (z.caret == Outer) {
    select_outer(d, z);
  } else if (d == Left) {
    z
    |> set_caret(Outer)
    |> move_outer(Right)
    |> OptUtil.and_then(select_outer(d));
  } else {
    z |> set_caret(Outer) |> select_outer(d);
  };

let perform = (a: Action.t, z: t): Action.Result.t(t) =>
  switch (a) {
  | Move(d) => Result.of_option(~error=Action.Failure.Cant_move, move(d, z))
  | Select(d) =>
    Result.of_option(~error=Action.Failure.Cant_move, select(d, z))
  | Destruct(d) =>
    Result.of_option(
      ~error=Action.Failure.Cant_move,
      z |> destruct_or_merge(d) |> Option.map(remold_regrout),
    )
  | Insert(char) =>
    z
    |> insert(char)
    |> Option.map(remold_regrout)
    |> Result.of_option(~error=Action.Failure.Cant_insert)
  //| Construct(from, label) => Ok(construct(from, label, z))
  | Pick_up => Ok(z |> pick_up |> remold_regrout)
  | Put_down =>
    /* Alternatively, putting down inside token could eiter merge-in or split */
    z.caret != Outer
      ? Error(Action.Failure.Cant_put_down_inside_token)
      : Result.of_option(
          ~error=Action.Failure.Cant_put_down,
          z |> put_down |> Option.map(remold_regrout),
        )
  };
