open Util;
open Sexplib.Std;
open OptUtil.Syntax;

[@deriving show]
type caret =
  | Outer
  | Inner(int, int);

[@deriving show]
type movability =
  | CanMoveInto(int, int)
  | CanMovePast
  | CantEven;

[@deriving show]
type appendability =
  | CanAddToLeft(string)
  | CanAddToRight(string)
  | CanAddToNeither;

// assuming single backpack, shards may appear in selection, backpack, or siblings
[@deriving show]
type t = {
  selection: Selection.t,
  backpack: Backpack.t,
  relatives: Relatives.t,
  caret,
  caret_col_target: int,
};
type state = (t, IdGen.state);

module Action = {
  [@deriving (show, sexp)]
  type t =
    | Move(Direction.plane)
    | Select(Direction.plane)
    | Destruct(Direction.t)
    | Insert(string)
    | RotateBackpack
    | Pick_up
    | Put_down;

  module Failure = {
    [@deriving (show, sexp)]
    type t =
      | Cant_move
      | Cant_insert
      | Cant_destruct
      | Cant_select
      | Cant_put_down;
  };

  module Result = {
    include Result;
    type t('success) = Result.t('success, Failure.t);
  };
};

let parent = (z: t): option(Piece.t) =>
  Relatives.parent(~sel=z.selection.content, z.relatives);

let caret_offset: caret => int =
  fun
  | Outer => 0
  | Inner(_, c) => c + 1;

let caret_direction =
    (
      {
        caret,
        selection: {content, focus},
        relatives: {siblings: (l_sibs, r_sibs), _},
        _,
      }: t,
    )
    : option(Direction.t) =>
  /* Direction the caret is facing in */
  switch (caret) {
  | Inner(_) => None
  | Outer =>
    switch (Siblings.neighbors((l_sibs, r_sibs))) {
    | (Some(l), Some(r))
        when Piece.is_whitespace(l) && Piece.is_whitespace(r) =>
      None
    | _ =>
      let sibs =
        switch (focus) {
        | Left => (l_sibs, content @ r_sibs)
        | Right => (l_sibs @ content, r_sibs)
        };
      Siblings.direction_between(sibs);
    }
  };

let indicated_piece = (z: t): option((Piece.t, Direction.t)) => {
  let ws = Piece.is_whitespace;
  /* Returns the piece currently indicated (if any) and which side of
     that piece the caret is on. We favor indicating the piece to the
     (R)ight, but may end up indicating the (P)arent or the (L)eft.
     We don't indicate whitespace tiles. */
  switch (Siblings.neighbors(z.relatives.siblings), parent(z)) {
  /* Non-empty selection => no indication */
  | _ when z.selection.content != [] => None
  /* Empty syntax => no indication */
  | ((None, None), None) => None
  /* L not whitespace, R is whitespace => indicate L */
  | ((Some(l), Some(r)), _) when !ws(l) && ws(r) => Some((l, Left))
  /* L and R are whitespaces => no indication */
  | ((Some(l), Some(r)), _) when ws(l) && ws(r) => None
  /* At right end of syntax and L is whitespace => no indication */
  | ((Some(l), None), None) when ws(l) => None
  /* At left end of syntax and R is whitespace => no indication */
  | ((None, Some(r)), None) when ws(r) => None
  /* No L and R is a whitespace and there is a P => indicate P */
  | ((None, Some(r)), Some(parent)) when ws(r) => Some((parent, Left))
  /* R is not whitespace => indicate R */
  | ((_, Some(r_nhbr)), _) => Some((r_nhbr, Right))
  /* No R and there is a P => indicate P */
  | ((_, None), Some(parent)) => Some((parent, Right))
  /* There is an L but no R and no P => indicate L */
  | ((Some(l_nhbr), None), None) => Some((l_nhbr, Right))
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
  update_siblings(((l, r)) => (l == [] ? [] : ListUtil.leading(l), r));

let neighbor_monotiles: Siblings.t => (option(Token.t), option(Token.t)) =
  siblings =>
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

let zip = (z: t): Segment.t =>
  Relatives.zip(~sel=z.selection.content, z.relatives);

let unselect_and_zip = (z: t): Segment.t => {
  let z = unselect(z);
  zip(z);
};

let convex = (z: t): bool => Segment.convex(zip(z));

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
  let+ (p, relatives) = Relatives.pop(z.selection.focus, z.relatives);
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
    let relatives =
      z.relatives
      |> Relatives.push(selection.focus, p)
      |> Relatives.reassemble;
    Some({...z, selection, relatives});
  };
};

let move_outer = (d: Direction.t, z: t): option(t) =>
  if (Selection.is_empty(z.selection)) {
    open OptUtil.Syntax;
    // let balanced = !Backpack.is_balanced(z.backpack);
    let+ (p, relatives) = Relatives.pop(d, z.relatives);
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
    Segment.incomplete_tiles(selected.content)
    |> List.partition(t =>
         Siblings.contains_matching(t, z.relatives.siblings)
       );
  let backpack =
    z.backpack
    |> Backpack.remove_matching(to_remove)
    |> Backpack.push_s(
         to_pick_up
         |> List.map(Segment.of_tile)
         |> List.map(Selection.mk(z.selection.focus)),
       );
  {...z, backpack};
};

let put_down = (z: t): option(t) =>
  if (z.caret != Outer) {
    None;
        /* Alternatively, putting down inside token could eiter merge-in or split */
  } else {
    open OptUtil.Syntax;
    let z = destruct_outer(z);
    let+ (_, popped, backpack) =
      Backpack.pop(
        Siblings.incomplete_tiles(z.relatives.siblings),
        z.backpack,
      );
    {...z, backpack} |> put_selection(popped) |> unselect;
  };

let insert_space_grout = (char: string, z: t): IdGen.t(t) => {
  open IdGen.Syntax;
  let+ id = IdGen.fresh;
  z
  |> update_siblings(((l, r)) =>
       (l @ [Piece.Whitespace({id, content: char})], r)
     );
};

let is_adjacent_space = (siblings: Siblings.t): bool =>
  switch (Siblings.neighbors(siblings)) {
  | _ when true => false //TODO(andrew): disabling this check for now
  | (Some(Whitespace({content, _})), _) when content == Whitespace.space =>
    true
  | (_, Some(Whitespace({content, _}))) when content == Whitespace.space =>
    true
  | _ => false
  };

let construct = (from: Direction.t, label: Label.t, z: t): IdGen.t(t) => {
  switch (label) {
  | [t] when t == Whitespace.linebreak => insert_space_grout(t, z)
  | [t] when t == Whitespace.space =>
    is_adjacent_space(z.relatives.siblings)
      ? IdGen.return(z) : insert_space_grout(t, z)
  | _ =>
    let z = destruct_outer(z);
    let molds = Molds.get(label);
    assert(molds != []);
    // initial mold to typecheck, will be remolded
    let mold = List.hd(molds);
    open IdGen.Syntax;
    let+ id = IdGen.fresh;
    let selections =
      Tile.split_shards(id, label, mold, List.mapi((i, _) => i, label))
      |> List.map(Segment.of_tile)
      |> List.map(Selection.mk(from))
      |> ListUtil.rev_if(from == Right);
    let backpack = Backpack.push_s(selections, z.backpack);
    Option.get(put_down({...z, backpack}));
  };
};

let replace_construct =
    (d: Direction.t, l: Label.t, (z, id_gen): state): option(state) =>
  z
  |> select_outer(d)
  |> Option.map(destruct_outer)
  |> Option.map(z => construct(d, l, z, id_gen));

let decrement_caret: caret => caret =
  fun
  | Outer
  | Inner(_, 0) => Outer
  | Inner(d, c) => Inner(d, c - 1);

let destruct =
    (
      d: Direction.t,
      ({caret, relatives: {siblings: (l_sibs, r_sibs), _}, _} as z, id_gen): state,
    )
    : option(state) => {
  /* Could add checks on valid tokens (all of these hold assuming substring) */
  let last_inner_pos = t => String.length(t) - 2;
  let d_outer = z =>
    z
    |> select_outer(d)
    |> Option.map(destruct_outer)
    |> Option.map(z => (z, id_gen));
  switch (d, caret, neighbor_monotiles((l_sibs, r_sibs))) {
  | (Left, Inner(_, c_idx), (_, Some(t))) =>
    let z = update_caret(decrement_caret, z);
    replace_construct(
      Right,
      [StringUtil.remove_nth(c_idx, t)],
      (z, id_gen),
    );
  | (Right, Inner(_, c_idx), (_, Some(t))) =>
    replace_construct(
      Right,
      [StringUtil.remove_nth(c_idx + 1, t)],
      (z, id_gen),
    )
    |> OptUtil.and_then(
         c_idx == last_inner_pos(t)
           ? ((z, id_gen)) =>
               z
               |> set_caret(Outer)
               |> move_outer(Right)
               |> Option.map(z => (z, id_gen))
           : Option.some,
       )
  /* Can't destruct inside delimiter */
  | (_, Inner(_), (_, None)) => None
  | (Left, Outer, (Some(t), _)) when String.length(t) > 1 =>
    replace_construct(Left, [StringUtil.remove_last(t)], (z, id_gen))
  | (Right, Outer, (_, Some(t))) when String.length(t) > 1 =>
    replace_construct(Right, [StringUtil.remove_first(t)], (z, id_gen))
  | (_, Outer, (Some(_), _)) /* t.length == 1 */
  | (_, Outer, (None, _)) => d_outer(z)
  };
};

let merge =
    ((l, r): (Token.t, Token.t), (z, id_gen): state): option(state) =>
  z
  |> set_caret(Inner(0, String.length(l) - 1))  // note monotile assumption
  |> move_outer(Right)
  |> OptUtil.and_then(select_outer(Left))
  |> OptUtil.and_then(select_outer(Left))
  |> Option.map(destruct_outer)
  |> Option.map(z => construct(Right, [l ++ r], z, id_gen));

let destruct_or_merge = (d: Direction.t, (z, id_gen): state): option(state) => {
  let* (z, id_gen) = destruct(d, (z, id_gen));
  let z_trimmed = update_siblings(Siblings.trim_whitespace_and_grout, z);
  switch (z.caret, neighbor_monotiles(z_trimmed.relatives.siblings)) {
  | (Outer, (Some(l), Some(r))) when Form.is_valid_token(l ++ r) =>
    merge((l, r), (z_trimmed, id_gen))
  | _ => Some((z, id_gen))
  };
};

let keyword_expand = ((z, _) as state: state): option(state) =>
  /* NOTE(andrew): We may want to allow editing of shards when only 1 of set
     is down (removing the rest of the set from backpack on edit) as something
     like this is necessary for backspace to act as undo after kw-expansion */
  switch (neighbor_monotiles(z.relatives.siblings)) {
  | (Some(kw), _) =>
    let (new_label, direction) = Molds.delayed_completion(kw, Left);
    replace_construct(direction, new_label, state);
  | _ => Some(state)
  };

let sibling_appendability: (string, Siblings.t) => appendability =
  (char, siblings) =>
    switch (neighbor_monotiles(siblings)) {
    | (Some(t), _) when Form.is_valid_token(t ++ char) => CanAddToLeft(t)
    | (_, Some(t)) when Form.is_valid_token(char ++ t) => CanAddToRight(t)
    | _ => CanAddToNeither
    };

let barf_or_construct =
    (new_token: string, direction_preference: Direction.t, z: t): IdGen.t(t) =>
  if (Backpack.is_first_matching(new_token, z.backpack)) {
    z |> put_down |> Option.get |> IdGen.return;
  } else {
    let (lbl, direction) =
      Molds.instant_completion(new_token, direction_preference);
    construct(direction, lbl, z);
  };

let insert_outer =
    (char: string, ({relatives: {siblings, _}, _} as z, id_gen): state)
    : option(state) =>
  switch (sibling_appendability(char, siblings)) {
  | CanAddToNeither =>
    (z, id_gen)
    |> keyword_expand
    |> Option.map(((z, id_gen)) => barf_or_construct(char, Left, z, id_gen))
  | CanAddToLeft(left_token) =>
    z
    |> remove_left_sib
    |> (z => barf_or_construct(left_token ++ char, Left, z, id_gen))
    |> Option.some
  | CanAddToRight(right_token) =>
    z
    |> remove_right_sib
    |> (z => barf_or_construct(char ++ right_token, Right, z, id_gen))
    |> Option.some
  };

let split =
    ((z, id_gen): state, char: string, idx: int, t: string): option(state) => {
  let (l, r) = StringUtil.split_nth(idx, t);
  let (lbl, direction) = Molds.instant_completion(char, Left);
  z
  |> set_caret(Outer)
  |> (z => replace_construct(Right, [r], (z, id_gen)))
  |> Option.map(((z, id_gen)) => construct(Left, [l], z, id_gen))
  |> OptUtil.and_then(keyword_expand)
  |> Option.map(((z, id_gen)) =>
       lbl == [Whitespace.space]
         //TODO(andrew): note temp hack to suppress whitespace next to infix grout
         //TODO(andrew): still need to address caret positioning
         ? (z, id_gen) : construct(direction, lbl, z, id_gen)
     );
};

let insert =
    (
      char: string,
      ({caret, relatives: {siblings, _}, _} as z, id_gen): state,
    )
    : option(state) =>
  switch (caret, neighbor_monotiles(siblings)) {
  | (Inner(d_idx, n), (_, Some(t))) =>
    let idx = n + 1;
    let new_t = StringUtil.insert_nth(idx, char, t);
    /* If inserting wouldn't produce a valid token, split */
    Form.is_valid_token(new_t)
      ? z
        |> set_caret(Inner(d_idx, idx))
        |> (z => replace_construct(Right, [new_t], (z, id_gen)))
      : split((z, id_gen), char, idx, t);
  /* Can't insert inside delimiter */
  | (Inner(_, _), (_, None)) => None
  | (Outer, (_, Some(_))) =>
    let caret =
      /* If we're adding to the right, move caret inside right nhbr */
      switch (sibling_appendability(char, siblings)) {
      | CanAddToRight(_) => Inner(0, 0) //Note: assumption of monotile
      | CanAddToNeither
      | CanAddToLeft(_) => Outer
      };
    (z, id_gen)
    |> insert_outer(char)
    |> Option.map(((z, id_gen)) => (set_caret(caret, z), id_gen));
  | (Outer, (_, None)) => insert_outer(char, (z, id_gen))
  };

let movability = (label, delim_idx): movability => {
  assert(delim_idx < List.length(label));
  switch (Settings.s.movement, label, delim_idx) {
  | (Char, _, _)
  | (Mono, [_], 0) =>
    let char_max = String.length(List.nth(label, delim_idx)) - 2;
    char_max < 0 ? CanMovePast : CanMoveInto(delim_idx, char_max);
  | (Token, _, _)
  | (Mono, _, _) => CanMovePast
  };
};

let neighbor_movability: t => (movability, movability) =
  ({relatives: {siblings, ancestors}, _}) => {
    let (supernhbr_l, supernhbr_r) =
      switch (ancestors) {
      | [] => (CantEven, CantEven)
      | [({children: (l_kids, _), label, _}, _), ..._] => (
          movability(label, List.length(l_kids)),
          movability(label, List.length(l_kids) + 1),
        )
      };
    let (l_nhbr, r_nhbr) = Siblings.neighbors(siblings);
    let l =
      switch (l_nhbr) {
      | Some(Tile({label, _})) => movability(label, List.length(label) - 1)
      | Some(_) => CanMovePast
      | _ => supernhbr_l
      };
    let r =
      switch (r_nhbr) {
      | Some(Tile({label, _})) => movability(label, 0)
      | Some(_) => CanMovePast
      | _ => supernhbr_r
      };
    (l, r);
  };

let move = (d: Direction.t, z: t): option(t) =>
  switch (d, z.caret, neighbor_movability(z)) {
  | _ when z.selection.content != [] =>
    /* this case maybe shouldn't be necessary but currently covers an edge
       (select an open parens to left of a multichar token and press left) */
    z |> set_caret(Outer) |> move_outer(d)
  | (Left, Outer, (CanMoveInto(d_init, c_max), _)) =>
    z |> set_caret(Inner(d_init, c_max)) |> move_outer(d)
  | (Left, Outer, _) => move_outer(d, z)
  | (Left, Inner(_), _) => Some(update_caret(decrement_caret, z))
  | (Right, Outer, (_, CanMoveInto(d_init, _))) =>
    Some(set_caret(Inner(d_init, 0), z))
  | (Right, Outer, _) => move_outer(d, z)
  | (Right, Inner(_, c), (_, CanMoveInto(_, c_max))) when c == c_max =>
    z |> set_caret(Outer) |> move_outer(d)
  | (Right, Inner(delim, c), _) => Some(set_caret(Inner(delim, c + 1), z))
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

let remold_regrout = (z: t): IdGen.t(t) => {
  assert(Selection.is_empty(z.selection));
  open IdGen.Syntax;
  let* state = IdGen.get;
  let ls_relatives =
    Relatives.remold(z.relatives)
    |> List.map(rs => Relatives.regrout(rs, state))
    |> List.sort(((rel, _), (rel', _)) => {
         open Relatives;
         let c = Int.compare(sort_rank(rel), sort_rank(rel'));
         c != 0 ? c : Int.compare(shape_rank(rel), shape_rank(rel'));
       });
  assert(ls_relatives != []);
  let (relatives, state) = List.hd(ls_relatives);
  let+ () = IdGen.put(state);
  {...z, relatives};
};

let representative_piece =
    (
      {
        selection: {content, focus},
        relatives: {siblings: (l_sibs, r_sibs), _},
        _,
      }: t,
    )
    : option((Piece.t, Direction.t)) => {
  /* The piece to the left of the caret, or if none exists, the piece to the right */
  let sibs =
    switch (focus) {
    | Left => (l_sibs, content @ r_sibs)
    | Right => (l_sibs @ content, r_sibs)
    };
  switch (Siblings.neighbors(sibs)) {
  | (Some(l), _) => Some((l, Left))
  | (_, Some(r)) => Some((r, Right))
  | _ => None
  };
};

let base_caret_point = (map: Measured.t, z: t): Measured.point => {
  switch (representative_piece(z)) {
  | Some((p, d)) =>
    let seg = Piece.disassemble(p);
    switch (d) {
    | Left =>
      let p = ListUtil.last(seg);
      let m = Measured.find_p(p, map);
      m.last;
    | Right =>
      let p = List.hd(seg);
      let m = Measured.find_p(p, map);
      m.origin;
    };
  | None => {row: 0, col: 0}
  };
};

let caret_point = (map: Measured.t, z: t): Measured.point => {
  let Measured.{row, col} = base_caret_point(map, z);
  {row, col: col + caret_offset(z.caret)};
};

type comparison =
  | Exact
  | Under
  | Over;

let comp = (current, target): comparison =>
  switch () {
  | _ when current == target => Exact
  | _ when current < target => Under
  | _ => Over
  };

let dcomp = (direction: Direction.t, a, b) =>
  switch (direction) {
  | Right => comp(a, b)
  | Left => comp(b, a)
  };

let rec do_towards =
        (
          f: t => option(t),
          d: Direction.t,
          cursorpos: t => Measured.point,
          goal: Measured.point,
          cur: t,
          prev: t,
        )
        : t => {
  let cur_p = cursorpos(cur);
  //Printf.printf("go_towards: current: %s\n", Measured.show_point(cur_p));
  switch (dcomp(d, cur_p.col, goal.col), dcomp(d, cur_p.row, goal.row)) {
  | (Exact, Exact) => cur
  | (_, Over) => prev
  | (_, Under)
  | (Under, Exact) =>
    switch (f(cur)) {
    | None => cur
    | Some(next) => do_towards(f, d, cursorpos, goal, next, cur)
    }
  | (Over, Exact) =>
    let comp = d == Right ? (<) : (>);
    comp(cursorpos(prev).row, goal.row) ? cur : prev;
  };
};

let do_vertical = (f: t => option(t), d: Direction.t, z: t): option(t) => {
  /* Here f should be a function which results in strict d-wards
     movement of the caret. Iterate f until we get to the closet
     caret position to a target derived from the initial position */
  let cursorpos = caret_point(snd(Measured.of_segment(zip(z))));
  let cur_p = cursorpos(z);
  let goal =
    Measured.{
      col: z.caret_col_target,
      row: cur_p.row + (d == Right ? 1 : (-1)),
    };
  //Printf.printf("select_vertical: cur: %s\n", Measured.show_point(cur));
  //Printf.printf("select_vertical: goal: %s\n", Measured.show_point(goal));
  let res = do_towards(f, d, cursorpos, goal, z, z);
  let res_p = cursorpos(res);
  /* Don't move if we end up on the same line as we started */
  res_p.row == cur_p.row ? None : Some(res);
};

let select_vertical = (d: Direction.t, z: t): option(t) =>
  do_vertical(select(d), d, z);

let move_vertical = (d: Direction.t, z: t): option(t) =>
  do_vertical(move(d), d, z);

let update_target = (z: t): t =>
  //TODO(andrew): $$$ this recomputes all measures
  {
    ...z,
    caret_col_target: caret_point(snd(Measured.of_segment(zip(z))), z).col,
  };

let directional_unselect = (d: Direction.t, z: t) => {
  let selection = {...z.selection, focus: Direction.toggle(d)};
  unselect({...z, selection});
};

let perform = (a: Action.t, (z, id_gen): state): Action.Result.t(state) =>
  switch (a) {
  | Move(d) =>
    //NOTE(andrew): not sure if this is best approach to unselection
    let z = directional_unselect(Direction.from_plane(d), z);
    /* Note: Don't update target on vertical movement */
    (
      switch (d) {
      | Direction.L => move(Left, z) |> Option.map(update_target)
      | Direction.R => move(Right, z) |> Option.map(update_target)
      | Direction.U => move_vertical(Left, z)
      | Direction.D => move_vertical(Right, z)
      }
    )
    |> Option.map(z => (z, id_gen))
    |> Result.of_option(~error=Action.Failure.Cant_move);
  | Select(d) =>
    /* Note: Don't update target on vertical selection */
    (
      switch (d) {
      | Direction.L => select(Left, z) |> Option.map(update_target)
      | Direction.R => select(Right, z) |> Option.map(update_target)
      | Direction.U => select_vertical(Left, z)
      | Direction.D => select_vertical(Right, z)
      }
    )
    |> Option.map(z => (z, id_gen))
    |> Option.map(((z, id_gen)) => (update_target(z), id_gen))
    |> Result.of_option(~error=Action.Failure.Cant_select)
  | Destruct(d) =>
    //TODO(andrew): there is currently a bug when backspacing with nonempty selection
    (z, id_gen)
    |> destruct_or_merge(d)
    |> Option.map(((z, id_gen)) => (update_target(z), id_gen))
    |> Option.map(((z, id_gen)) => remold_regrout(z, id_gen))
    |> Result.of_option(~error=Action.Failure.Cant_destruct)
  | Insert(char) =>
    (z, id_gen)
    |> insert(char)
    |> Option.map(((z, id_gen)) => (update_target(z), id_gen))
    |> Option.map(((z, id_gen)) => remold_regrout(z, id_gen))
    |> Result.of_option(~error=Action.Failure.Cant_insert)
  | Pick_up => Ok(remold_regrout(pick_up(z), id_gen))
  | Put_down =>
    z
    |> put_down
    |> Option.map(z => remold_regrout(z, id_gen))
    |> Option.map(((z, id_gen)) => (update_target(z), id_gen))
    |> Result.of_option(~error=Action.Failure.Cant_put_down)
  | RotateBackpack =>
    Ok(({...z, backpack: Util.ListUtil.rotate(z.backpack)}, id_gen))
  };
