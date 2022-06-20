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
  | CanAddToLeft(Token.t)
  | CanAddToRight(Token.t)
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

[@deriving (show, sexp)]
type chunkiness =
  | ByChar
  | MonoByChar
  | ByToken;

[@deriving (show, sexp)]
type plane_move =
  | Up
  | Down
  | Left(chunkiness)
  | Right(chunkiness);

[@deriving (show, sexp)]
type move =
  | Extreme(plane_move)
  | Local(plane_move);

module Action = {
  [@deriving (show, sexp)]
  type t =
    | Move(move)
    | Select(move)
    | Destruct(Direction.t)
    | Insert(string)
    | RotateBackpack
    | MoveToBackpackTarget(plane_move)
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

let update_relatives = (f: Relatives.t => Relatives.t, z: t): t => {
  ...z,
  relatives: f(z.relatives),
};

let update_siblings: (Siblings.t => Siblings.t, t) => t =
  f => update_relatives(rs => {...rs, siblings: f(rs.siblings)});

let pop_backpack = (z: t) =>
  Backpack.pop(Relatives.local_incomplete_tiles(z.relatives), z.backpack);

module Outer = {
  let unselect = (z: t): t => {
    let relatives =
      z.relatives
      |> Relatives.prepend(z.selection.focus, z.selection.content)
      |> Relatives.reassemble;
    let selection = Selection.clear(z.selection);
    {...z, selection, relatives};
  };

  let update_selection = (selection: Selection.t, z: t): (Selection.t, t) => {
    let old = z.selection;
    // used to be necessary to unselect when selection update
    // included remold/regrout, now no longer necessary if needs
    // to be changed but keeping for now to minimize change
    let z = unselect({...z, selection});
    (old, z);
  };

  let put_selection = (sel: Selection.t, z: t): t =>
    snd(update_selection(sel, z));

  let grow_selection = (z: t): option(t) => {
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

  let directional_unselect = (d: Direction.t, z: t): option(t) => {
    let selection = {...z.selection, focus: Direction.toggle(d)};
    Some(unselect({...z, selection}));
  };

  let move = (d: Direction.t, z: t): option(t) =>
    if (Selection.is_empty(z.selection)) {
      // let balanced = !Backpack.is_balanced(z.backpack);
      let+ (p, relatives) = Relatives.pop(d, z.relatives);
      let relatives =
        relatives
        |> Relatives.push(Direction.toggle(d), p)
        |> Relatives.reassemble;
      {...z, relatives};
    } else {
      directional_unselect(d, z);
    };

  let select = (d: Direction.t, z: t): option(t) =>
    d == z.selection.focus ? grow_selection(z) : shrink_selection(z);

  let pick_up = (z: t): t => {
    let (selected, z) = update_selection(Selection.empty, z);
    let selection =
      selected.content
      |> Segment.trim_grout_around_whitespace(Left)
      |> Segment.trim_grout_around_whitespace(Right)
      |> Selection.mk(selected.focus);
    let backpack = Backpack.push(selection, z.backpack);
    {...z, backpack};
  };

  let destruct = (z: t): t => {
    let (selected, z) = update_selection(Selection.empty, z);
    let (to_pick_up, to_remove) =
      Segment.incomplete_tiles(selected.content)
      |> List.partition(t =>
           Siblings.contains_matching(t, z.relatives.siblings)
           || Ancestors.parent_matches(t, z.relatives.ancestors)
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

  let directional_remove = (d: Direction.t, z: t): option(t) =>
    z |> select(d) |> Option.map(destruct);

  let put_down = (z: t): option(t) => {
    let z = destruct(z);
    let+ (_, popped, backpack) = pop_backpack(z);
    IncompleteBidelim.set(popped.content);
    {...z, backpack} |> put_selection(popped) |> unselect;
  };

  let construct = (from: Direction.t, label: Label.t, z: t): IdGen.t(t) => {
    IdGen.Syntax.(
      switch (label) {
      | [content] when Form.is_whitespace(content) =>
        let+ id = IdGen.fresh;
        z
        |> update_siblings(((l, r)) =>
             (l @ [Piece.Whitespace({id, content})], r)
           );
      | _ =>
        let z = destruct(z);
        let molds = Molds.get(label);
        assert(molds != []);
        // initial mold to typecheck, will be remolded
        let mold = List.hd(molds);

        let+ id = IdGen.fresh;
        let selections =
          Tile.split_shards(id, label, mold, List.mapi((i, _) => i, label))
          |> List.map(Segment.of_tile)
          |> List.map(Selection.mk(from))
          |> ListUtil.rev_if(from == Right);
        let backpack = Backpack.push_s(selections, z.backpack);
        Option.get(put_down({...z, backpack}));
      }
    );
  };

  let replace =
      (d: Direction.t, l: Label.t, (z, id_gen): state): option(state) =>
    /* i.e. destruct and construct */
    z
    |> select(d)
    |> Option.map(destruct)
    |> Option.map(z => construct(d, l, z, id_gen));
};

let parent = (z: t): option(Piece.t) =>
  Relatives.parent(~sel=z.selection.content, z.relatives);

let zip = (z: t): Segment.t =>
  Relatives.zip(~sel=z.selection.content, z.relatives);

let unselect_and_zip = (z: t): Segment.t => z |> Outer.unselect |> zip;

let sibs_with_sel =
    (
      {
        selection: {content, focus},
        relatives: {siblings: (l_sibs, r_sibs), _},
        _,
      }: t,
    )
    : Siblings.t =>
  switch (focus) {
  | Left => (l_sibs, content @ r_sibs)
  | Right => (l_sibs @ content, r_sibs)
  };

let representative_piece = (z: t): option((Piece.t, Direction.t)) => {
  /* The piece to the left of the caret, or if none exists, the piece to the right */
  switch (Siblings.neighbors(sibs_with_sel(z))) {
  | (Some(l), _) => Some((l, Left))
  | (_, Some(r)) => Some((r, Right))
  | _ => None
  };
};

let indicated_piece = (z: t): option((Piece.t, Direction.t)) => {
  let ws = Piece.is_whitespace;
  /* Returns the piece currently indicated (if any) and which side of
     that piece the caret is on. We favor indicating the piece to the
     (R)ight, but may end up indicating the (P)arent or the (L)eft.
     We don't indicate whitespace tiles. This function ignores whether
     or not there is a selection so this can be used to get the caret
     direction, but the caller shouldn't indicate if there's a selection */
  switch (Siblings.neighbors(sibs_with_sel(z)), parent(z)) {
  /* Non-empty selection => no indication */
  //| _ when z.selection.content != [] => None
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
  /* L is not whitespace and caret is outer => indicate L */
  | ((Some(l), _), _) when !ws(l) && z.caret == Outer => Some((l, Left))
  /* No L, some P, and caret is outer => indicate R */
  | ((None, _), Some(parent)) when z.caret == Outer => Some((parent, Left))
  /* R is not whitespace, either no L or L is whitespace or caret is inner => indicate R */
  | ((_, Some(r)), _) => Some((r, Right))
  /* No R and there is a P => indicate P */
  | ((_, None), Some(parent)) => Some((parent, Right))
  /* There is an L but no R and no P => indicate L */
  | ((Some(l), None), None) => Some((l, Right))
  };
};

let update_caret = (f: caret => caret, z: t): t => {
  ...z,
  caret: f(z.caret),
};

let set_caret = (caret: caret): (t => t) => update_caret(_ => caret);

let decrement_caret: caret => caret =
  fun
  | Outer
  | Inner(_, 0) => Outer
  | Inner(d, c) => Inner(d, c - 1);

let caret_offset: caret => int =
  fun
  | Outer => 0
  | Inner(_, c) => c + 1;

let caret_direction = (z: t): option(Direction.t) =>
  /* Direction the caret is facing in */
  switch (z.caret) {
  | Inner(_) => None
  | Outer =>
    switch (Siblings.neighbors(sibs_with_sel(z))) {
    | (Some(l), Some(r))
        when Piece.is_whitespace(l) && Piece.is_whitespace(r) =>
      None
    | _ => Siblings.direction_between(sibs_with_sel(z))
    }
  };

let neighbor_monotiles: Siblings.t => (option(Token.t), option(Token.t)) =
  siblings =>
    switch (Siblings.neighbors(siblings)) {
    | (Some(l), Some(r)) => (Piece.monotile(l), Piece.monotile(r))
    | (Some(l), None) => (Piece.monotile(l), None)
    | (None, Some(r)) => (None, Piece.monotile(r))
    | (None, None) => (None, None)
    };

let destruct =
    (
      d: Direction.t,
      ({caret, relatives: {siblings: (l_sibs, r_sibs), _}, _} as z, id_gen): state,
    )
    : option(state) => {
  /* Could add checks on valid tokens (all of these hold assuming substring) */
  let last_inner_pos = t => Token.length(t) - 2;
  switch (d, caret, neighbor_monotiles((l_sibs, r_sibs))) {
  /* When there's a selection, defer to Outer */
  | _ when z.selection.content != [] =>
    z |> Outer.destruct |> IdGen.id(id_gen) |> Option.some
  | (Left, Inner(_, c_idx), (_, Some(t))) =>
    let z = update_caret(decrement_caret, z);
    Outer.replace(Right, [Token.rm_nth(c_idx, t)], (z, id_gen));
  | (Right, Inner(_, c_idx), (_, Some(t))) when c_idx == last_inner_pos(t) =>
    Outer.replace(Right, [Token.rm_nth(c_idx + 1, t)], (z, id_gen))
    |> OptUtil.and_then(((z, id_gen)) =>
         z
         |> set_caret(Outer)
         |> Outer.move(Right)
         |> Option.map(IdGen.id(id_gen))
       )
  /* If not on last inner position */
  | (Right, Inner(_, c_idx), (_, Some(t))) =>
    Outer.replace(Right, [Token.rm_nth(c_idx + 1, t)], (z, id_gen))
  /* Can't subdestruct in delimiter, so just destruct on whole delimiter */
  | (Left, Inner(_), (_, None))
  | (Right, Inner(_), (_, None)) =>
    /* Note: Counterintuitve, but yes, these cases are identically handled */
    z
    |> set_caret(Outer)
    |> Outer.directional_remove(Right)
    |> Option.map(IdGen.id(id_gen))
  //| (_, Inner(_), (_, None)) => None
  | (Left, Outer, (Some(t), _)) when Token.length(t) > 1 =>
    //Option.map(IdGen.id(id_gen)
    Outer.replace(Left, [Token.rm_last(t)], (z, id_gen))
  | (Right, Outer, (_, Some(t))) when Token.length(t) > 1 =>
    Outer.replace(Right, [Token.rm_first(t)], (z, id_gen))
  | (_, Outer, (Some(_), _)) /* t.length == 1 */
  | (_, Outer, (None, _)) =>
    z |> Outer.directional_remove(d) |> Option.map(IdGen.id(id_gen))
  };
};

let merge =
    ((l, r): (Token.t, Token.t), (z, id_gen): state): option(state) =>
  z
  |> set_caret(Inner(0, Token.length(l) - 1))  // note monotile assumption
  |> Outer.directional_remove(Left)
  |> OptUtil.and_then(Outer.directional_remove(Right))
  |> Option.map(z => Outer.construct(Right, [l ++ r], z, id_gen));

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
    Outer.replace(direction, new_label, state);
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
    (new_token: Token.t, direction_preference: Direction.t, z: t): IdGen.t(t) => {
  let barfed =
    Backpack.is_first_matching(new_token, z.backpack)
      ? Outer.put_down(z) : None;
  switch (barfed) {
  | Some(z) => IdGen.return(z)
  | None =>
    let (lbl, direction) =
      Molds.instant_completion(new_token, direction_preference);
    Outer.construct(direction, lbl, z);
  };
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
    |> Outer.directional_remove(Left)
    |> Option.map(z => barf_or_construct(left_token ++ char, Left, z, id_gen))
  | CanAddToRight(right_token) =>
    z
    |> Outer.directional_remove(Right)
    |> Option.map(z =>
         barf_or_construct(char ++ right_token, Right, z, id_gen)
       )
  };

let remold_regrout = (d: Direction.t, z: t): IdGen.t(t) => {
  assert(Selection.is_empty(z.selection));
  open IdGen.Syntax;
  let* state = IdGen.get;
  let ls_relatives =
    Relatives.remold(z.relatives)
    |> List.map(rs => Relatives.regrout(d, rs, state))
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

let opt_regrold = d =>
  Option.map(((z, id_gen)) => remold_regrout(d, z, id_gen));

let split =
    ((z, id_gen): state, char: string, idx: int, t: Token.t): option(state) => {
  let (l, r) = Token.split_nth(idx, t);
  let (lbl, direction) = Molds.instant_completion(char, Left);
  z
  |> set_caret(Outer)
  |> (z => Outer.replace(Right, [r], (z, id_gen)))
  |> Option.map(((z, id_gen)) => Outer.construct(Left, [l], z, id_gen))
  |> OptUtil.and_then(keyword_expand)
  |> Option.map(((z, id_gen))
       /* NOTE(andrew): Temp hack to suppress whitespace next
          to infix grout. We verify both that whitespace is being
          inserted, and that there will be a shape mismatch. The
          latter check is necessary for cases like let|x, where
          there won't actually be an infix grout inserted, so we
          want there to be a space. */
       =>
         lbl == [Whitespace.space]
         && Siblings.is_mismatch(z.relatives.siblings)
           ? (z, id_gen) : Outer.construct(direction, lbl, z, id_gen)
       );
};

let insert =
    (
      char: string,
      ({caret, relatives: {siblings, _}, _} as z, id_gen): state,
    )
    : option(state) => {
  /* If there's a selection, delete it before proceeding */
  let z = z.selection.content != [] ? Outer.destruct(z) : z;
  switch (caret, neighbor_monotiles(siblings)) {
  | (Inner(d_idx, n), (_, Some(t))) =>
    let idx = n + 1;
    let new_t = Token.insert_nth(idx, char, t);
    /* If inserting wouldn't produce a valid token, split */
    Form.is_valid_token(new_t)
      ? z
        |> set_caret(Inner(d_idx, idx))
        |> (z => Outer.replace(Right, [new_t], (z, id_gen)))
        |> opt_regrold(Left)
      : split((z, id_gen), char, idx, t) |> opt_regrold(Right);
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
    |> Option.map(((z, id_gen)) => (set_caret(caret, z), id_gen))
    |> opt_regrold(Left);
  | (Outer, (_, None)) =>
    insert_outer(char, (z, id_gen)) |> opt_regrold(Left)
  };
};

let movability = (chunkiness: chunkiness, label, delim_idx): movability => {
  assert(delim_idx < List.length(label));
  switch (chunkiness, label, delim_idx) {
  | (ByChar, _, _)
  | (MonoByChar, [_], 0) =>
    let char_max = Token.length(List.nth(label, delim_idx)) - 2;
    char_max < 0 ? CanMovePast : CanMoveInto(delim_idx, char_max);
  | (ByToken, _, _)
  | (MonoByChar, _, _) => CanMovePast
  };
};

let neighbor_movability =
    (chunkiness: chunkiness, {relatives: {siblings, ancestors}, _}: t)
    : (movability, movability) => {
  let movability = movability(chunkiness);
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

let move = (chunkiness: chunkiness, d: Direction.t, z: t): option(t) =>
  switch (d, z.caret, neighbor_movability(chunkiness, z)) {
  | _ when z.selection.content != [] =>
    /* this case maybe shouldn't be necessary but currently covers an edge
       (select an open parens to left of a multichar token and press left) */
    z |> set_caret(Outer) |> Outer.move(d)
  | (Left, Outer, (CanMoveInto(d_init, c_max), _)) =>
    z |> set_caret(Inner(d_init, c_max)) |> Outer.move(d)
  | (Left, Outer, _) => Outer.move(d, z)
  | (Left, Inner(_), _) when chunkiness == ByToken =>
    Some(z |> set_caret(Outer))
  | (Left, Inner(_), _) => Some(update_caret(decrement_caret, z))
  | (Right, Outer, (_, CanMoveInto(d_init, _))) =>
    Some(set_caret(Inner(d_init, 0), z))
  | (Right, Outer, _) => Outer.move(d, z)
  | (Right, Inner(_, c), (_, CanMoveInto(_, c_max))) when c == c_max =>
    z |> set_caret(Outer) |> Outer.move(d)
  | (Right, Inner(_), _) when chunkiness == ByToken =>
    z |> set_caret(Outer) |> Outer.move(d)
  | (Right, Inner(delim, c), _) => Some(set_caret(Inner(delim, c + 1), z))
  };

let select = (d: Direction.t, z: t): option(t) =>
  if (z.caret == Outer) {
    Outer.select(d, z);
  } else if (d == Left) {
    z
    |> set_caret(Outer)
    |> Outer.move(Right)
    |> OptUtil.and_then(Outer.select(d));
  } else {
    z |> set_caret(Outer) |> Outer.select(d);
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
    let d_cur = abs(cur_p.col - goal.col);
    let d_prev = abs(cursorpos(prev).col - goal.col);
    switch () {
    | _ when d_cur < d_prev => cur
    | _ when d_prev < d_cur => prev
    | _ => cur /* default to going over */
    };
  };
};

let do_vertical = (f: t => option(t), d: Direction.t, z: t): option(t) => {
  /* Here f should be a function which results in strict d-wards
     movement of the caret. Iterate f until we get to the closet
     caret position to a target derived from the initial position */
  let cursorpos = caret_point(Measured.of_segment(unselect_and_zip(z)));
  let cur_p = cursorpos(z);
  let goal =
    Measured.{
      col: z.caret_col_target,
      row: cur_p.row + (d == Right ? 1 : (-1)),
    };
  // Printf.printf("do_vertical: cur: %s\n", Measured.show_point(cur_p));
  // Printf.printf("do_vertical: goal: %s\n", Measured.show_point(goal));
  let res = do_towards(f, d, cursorpos, goal, z, z);
  let res_p = cursorpos(res);
  Measured.point_equals(res_p, cur_p) ? None : Some(res);
};

let from_plane: plane_move => Direction.t =
  fun
  | Left(_) => Left
  | Right(_) => Right
  | Up => Left
  | Down => Right;

let do_extreme = (f: t => option(t), d: plane_move, z: t): option(t) => {
  let cursorpos = caret_point(Measured.of_segment(unselect_and_zip(z)));
  let cur_p = cursorpos(z);
  let goal: Measured.point =
    switch (d) {
    | Right(_) => {col: Int.max_int, row: cur_p.row}
    | Left(_) => {col: 0, row: cur_p.row}
    | Up => {col: 0, row: 0}
    | Down => {col: Int.max_int, row: Int.max_int}
    };
  let res = do_towards(f, from_plane(d), cursorpos, goal, z, z);
  Measured.point_equals(cursorpos(res), cursorpos(z)) ? None : Some(res);
};

let select_vertical = (d: Direction.t, z: t): option(t) =>
  do_vertical(select(d), d, z);

let move_vertical = (d: Direction.t, z: t): option(t) =>
  z.selection.content == []
    ? do_vertical(move(ByChar, d), d, z) : Outer.directional_unselect(d, z);

let update_target = (z: t): t =>
  //NOTE(andrew): $$$ this recomputes all measures
  {
    ...z,
    caret_col_target:
      caret_point(Measured.of_segment(unselect_and_zip(z)), z).col,
  };

let put_down = (z: t): option(t) =>
  /* Alternatively, putting down inside token could eiter merge-in or split */
  switch (z.caret) {
  | Inner(_) => None
  | Outer => Outer.put_down(z)
  };

let targets_within_row = (map: Measured.t, z: t): list(t) => {
  let caret = caret_point(map);
  let init = caret(z);
  let rec go = (d: Direction.t, z: t) => {
    switch (move(ByChar, d, z)) {
    | None => []
    | Some(z) =>
      if (caret(z).row != init.row) {
        [];
      } else {
        switch (pop_backpack(z)) {
        | None => go(d, z)
        | Some(_) => [z, ...go(d, z)]
        };
      }
    };
  };
  let curr =
    switch (pop_backpack(z)) {
    | None => []
    | Some(_) => [z]
    };
  List.rev(go(Left, z)) @ curr @ go(Right, z);
};

// TODO(d): unify this logic with rest of movement logic
let rec move_to_backpack_target = (d: plane_move, map, z: t): option(t) => {
  let caret_point = caret_point(map);
  let done_or_try_again = (d, z) =>
    switch (pop_backpack(z)) {
    | None => move_to_backpack_target(d, map, z)
    | Some(_) => Some(z)
    };
  switch (d) {
  | Left(chunk) =>
    let* z = Option.map(update_target, move(chunk, Left, z));
    done_or_try_again(d, z);
  | Right(chunk) =>
    let* z = Option.map(update_target, move(chunk, Right, z));
    done_or_try_again(d, z);
  | Up =>
    let* z = move_vertical(Left, z);
    let zs =
      targets_within_row(map, z)
      |> List.sort((z1, z2) => {
           let dist1 = caret_point(z1).col - z.caret_col_target;
           let dist2 = caret_point(z2).col - z.caret_col_target;
           let c = Int.compare(abs(dist1), abs(dist2));
           // favor left
           c != 0 ? c : Int.compare(dist1, dist2);
         });
    switch (zs) {
    | [] => move_to_backpack_target(d, map, z)
    | [z, ..._] => Some(z)
    };
  | Down =>
    let* z = move_vertical(Right, z);
    let zs =
      targets_within_row(map, z)
      |> List.sort((z1, z2) => {
           let dist1 = caret_point(z1).col - z.caret_col_target;
           let dist2 = caret_point(z2).col - z.caret_col_target;
           let c = Int.compare(abs(dist1), abs(dist2));
           // favor right
           c != 0 ? c : - Int.compare(dist1, dist2);
         });
    switch (zs) {
    | [] => move_to_backpack_target(d, map, z)
    | [z, ..._] => Some(z)
    };
  };
};

let perform = (a: Action.t, (z, id_gen): state): Action.Result.t(state) => {
  IncompleteBidelim.clear();
  switch (a) {
  | Move(d) =>
    switch (d) {
    | Extreme(d) =>
      do_extreme(move(ByToken, from_plane(d)), d, z)
      |> Option.map(IdGen.id(id_gen))
      |> Result.of_option(~error=Action.Failure.Cant_move)
    | Local(d) =>
      /* Note: Don't update target on vertical movement */
      z
      |> (
        z =>
          switch (d) {
          | Left(chunk) => move(chunk, Left, z) |> Option.map(update_target)
          | Right(chunk) =>
            move(chunk, Right, z) |> Option.map(update_target)
          | Up => move_vertical(Left, z)
          | Down => move_vertical(Right, z)
          }
      )
      |> Option.map(IdGen.id(id_gen))
      |> Result.of_option(~error=Action.Failure.Cant_move)
    }
  | Select(d) =>
    let selected =
      switch (d) {
      | Extreme(d) => do_extreme(select(from_plane(d)), d, z)
      | Local(d) =>
        /* Note: Don't update target on vertical selection */
        switch (d) {
        | Left(_) => select(Left, z) |> Option.map(update_target)
        | Right(_) => select(Right, z) |> Option.map(update_target)
        | Up => select_vertical(Left, z)
        | Down => select_vertical(Right, z)
        }
      };
    selected
    |> Option.map(IdGen.id(id_gen))
    |> Result.of_option(~error=Action.Failure.Cant_select);
  | Destruct(d) =>
    (z, id_gen)
    |> destruct_or_merge(d)
    |> Option.map(((z, id_gen)) => (update_target(z), id_gen))
    |> Option.map(((z, id_gen)) => remold_regrout(Left, z, id_gen))
    |> Result.of_option(~error=Action.Failure.Cant_destruct)
  | Insert(char) =>
    (z, id_gen)
    |> insert(char)
    |> Option.map(((z, id_gen)) => (update_target(z), id_gen))
    //|> Option.map(((z, id_gen)) => remold_regrout(Right, z, id_gen))
    |> Result.of_option(~error=Action.Failure.Cant_insert)
  | Pick_up => Ok(remold_regrout(Left, Outer.pick_up(z), id_gen))
  | Put_down =>
    z
    |> put_down
    |> Option.map(z => remold_regrout(Left, z, id_gen))
    |> Option.map(((z, id_gen)) => (update_target(z), id_gen))
    |> Result.of_option(~error=Action.Failure.Cant_put_down)
  | RotateBackpack =>
    Ok(({...z, backpack: Util.ListUtil.rotate(z.backpack)}, id_gen))
  | MoveToBackpackTarget(d) =>
    let map = Measured.of_segment(unselect_and_zip(z));
    move_to_backpack_target(d, map, z)
    |> Option.map(IdGen.id(id_gen))
    |> Result.of_option(~error=Action.Failure.Cant_move);
  };
};
