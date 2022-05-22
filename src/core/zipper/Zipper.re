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
      | Nothing_to_put_down;
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

let neighbors: Siblings.t => (option(Piece.t), option(Piece.t)) =
  ((l, r)) => (
    l == [] ? None : Some(List.hd(l)),
    r == [] ? None : Some(List.hd(r)),
  );

let has_grout_neighbor: Siblings.t => bool =
  siblings =>
    switch (neighbors(siblings)) {
    | (Some(Grout(_)), _)
    | (_, Some(Grout(_))) => true
    | _ => false
    };

let monotile: Base.Piece.t => option(string) =
  fun
  | Tile({label: [t], _}) => Some(t)
  | _ => None;

let neighbor_monotiles = siblings =>
  switch (neighbors(siblings)) {
  | (Some(l), Some(r)) => (monotile(l), monotile(r))
  | (Some(l), None) => (monotile(l), None)
  | (None, Some(r)) => (None, monotile(r))
  | (None, None) => (None, None)
  };

let is_length_one_monotile: Base.Piece.t => bool =
  p =>
    switch (monotile(p)) {
    | Some(t) => String.length(t) == 1
    | None => false
    };

let unselect = (z: t): t => {
  let relatives =
    z.relatives
    |> Relatives.prepend(z.selection.focus, z.selection.content)
    |> Relatives.reassemble;
  let selection = Selection.clear(z.selection);
  {...z, selection, relatives};
};

let convex = (z: t): bool => {
  let z = unselect(z);
  let (pre, suf) = Relatives.disassemble(z.relatives);
  Segment.convex(List.rev(pre) @ suf);
};

let update_selection = (selection: Selection.t, z: t): (Selection.t, t) => {
  let old = z.selection;
  let z = unselect({...z, selection});
  let ls_relatives =
    Relatives.remold(z.relatives)
    |> List.sort((rel, rel') => {
         open Relatives;
         let c = Int.compare(sort_rank(rel), sort_rank(rel'));
         c != 0 ? c : Int.compare(shape_rank(rel), shape_rank(rel'));
       });
  assert(ls_relatives != []);
  let relatives = ls_relatives |> List.hd |> Relatives.regrout;
  (old, {...z, relatives});
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
    let balanced = !Backpack.is_balanced(z.backpack);
    let+ (p, relatives) = Relatives.pop(~balanced, d, z.relatives);
    let relatives =
      relatives
      |> Relatives.push(Direction.toggle(d), p)
      |> Relatives.reassemble;
    {...z, relatives};
  } else if (d != z.selection.focus
             || Selection.is_balanced(z.selection)
             || Backpack.is_balanced(z.backpack)) {
    let selection = {...z.selection, focus: Direction.toggle(d)};
    Some(unselect({...z, selection}));
  } else {
    None;
  };

let select_outer = (d: Direction.t, z: t): option(t) =>
  d == z.selection.focus ? grow_selection(z) : shrink_selection(z);

let pick_up = (z: t): t => {
  let (selected, z) = update_selection(Selection.empty, z);
  let selections =
    selected.content
    |> Segment.split_by_grout
    |> Aba.get_a
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
  let+ (popped, backpack) = Backpack.pop(z.backpack);
  {...z, backpack} |> put_selection(popped) |> unselect;
};

let construct = (from: Direction.t, label: Tile.Label.t, z: t): t => {
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

let replace_construct = (d: Direction.t, l: Tile.Label.t, z: t): option(t) =>
  z
  |> select_outer(d)
  |> Option.map(destruct_outer)
  |> Option.map(construct(d, l));

let can_merge_through: Base.Piece.t => bool =
  p => Base.is_grout(p) || is_length_one_monotile(p);

let merge_candidates: (caret, Siblings.t) => option((Token.t, Token.t)) =
  (caret, (l_sibs, r_sibs)) =>
    switch (l_sibs) {
    | [l_nhbr, ...ps] when can_merge_through(l_nhbr) =>
      switch (caret, neighbor_monotiles((ps, r_sibs))) {
      | (Outer, (Some(l_2nd_nhbr), Some(r_nhbr)))
          when Molds.has_mold(l_2nd_nhbr ++ r_nhbr) =>
        Some((l_2nd_nhbr, r_nhbr))
      | _ => None
      }
    | _ => None
    };

/* merge precondition: ... <Monotile> <DontCare> | <Monotile> ... */
let merge = (z: t, (l, r): (Token.t, Token.t)): option(t) =>
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
  //TODO(andrew): more checks on valid tokens
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
  switch (d, merge_candidates(caret, siblings)) {
  | (_, None) => destruct(d, z)
  | (Left, Some(candidates)) => merge(z, candidates)
  | (Right, Some(_)) => failwith("TODO(andrew): delete: destruct_or_merge")
  };

let instant_completion: (string, Direction.t) => (list(Token.t), Direction.t) =
  (s, direction_preference) =>
    /* Completions which can or must be executed immediately */
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

let barf_or_construct =
    (new_token: string, direction_preference: Direction.t, z: t) =>
  if (Backpack.is_first_matching(new_token, z.backpack)) {
    put_down(z);
  } else {
    let (new_label, direction) =
      instant_completion(new_token, direction_preference);
    Some(construct(direction, new_label, z));
  };

let insert_space_grout =
    (
      _char: string,
      {relatives: {siblings: (l_sibs, r_sibs), _}, _} as z: t,
    ) =>
  if (has_grout_neighbor((l_sibs, r_sibs))) {
    z;
  } else {
    let new_grout =
      switch (l_sibs, r_sibs) {
      | ([], []) => failwith("insert_space_grout: impossible")
      | ([], [_, ..._]) => Base.Piece.Grout((Convex, Nib.Shape.concave()))
      | ([p, ..._], _) =>
        let nib_shape_r = p |> Base.nib_shapes |> snd;
        Grout((Nib.Shape.flip(nib_shape_r), nib_shape_r));
      };
    update_siblings(((l, r)) => ([new_grout] @ l, r), z)
    |> update_relatives(Relatives.regrout);
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

let insert_outer =
    (char: string, {relatives: {siblings, _}, _} as z: t): option(t) => {
  switch (char) {
  | _ when Token.is_whitespace(char) =>
    z |> keyword_expand |> Option.map(insert_space_grout(char))
  | _ when Token.is_symbol(char) || Token.is_alphanum(char) =>
    switch (sibling_appendability(char, siblings)) {
    | CanAddToNeither =>
      z |> keyword_expand |> OptUtil.and_then(barf_or_construct(char, Left))
    | CanAddToLeft(left_token) =>
      z |> remove_left_sib |> barf_or_construct(left_token ++ char, Left)
    | CanAddToRight(right_token) =>
      z |> remove_right_sib |> barf_or_construct(char ++ right_token, Right)
    }
  | _ => None
  };
};

let split = (z: t, char: string, idx: int, t: string): option(t) => {
  let (l, r) = StringUtil.split_nth(idx, t);
  let (lbl, direction) = instant_completion(char, Left);
  z
  |> set_caret(Outer)
  |> replace_construct(Right, [r])
  |> Option.map(construct(Left, [l]))
  |> OptUtil.and_then(keyword_expand)
  |> OptUtil.and_then(z =>
       Token.is_whitespace(char)
         ? z |> insert_space_grout(char) |> move_outer(Right)
         : Some(construct(direction, lbl, z))
     );
};

let insert =
    (char: string, {caret, relatives: {siblings, _}, _} as z: t): option(t) =>
  switch (caret, neighbor_monotiles(siblings)) {
  | (Inner(n), (_, Some(t))) =>
    let idx = n + 1;
    let new_t = StringUtil.insert_nth(idx, char, t);
    /* If inserting wouldn't produce a valid token, split */
    Molds.has_mold(new_t)
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
  | (Left, Inner(0), _) => Some(set_caret(Outer, z))
  | (Left, Inner(n), _) => Some(set_caret(Inner(n - 1), z))
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
    //hey david: noticed delete doesnt move thru grout like bksp does
    Result.of_option(
      ~error=Action.Failure.Cant_move,
      destruct_or_merge(d, z),
    )
  | Insert(char) =>
    Result.of_option(~error=Action.Failure.Cant_insert, insert(char, z))
  //| Construct(from, label) => Ok(construct(from, label, z))
  | Pick_up => Ok(pick_up(z))
  | Put_down =>
    z.caret != Outer
      ? Error(Action.Failure.Cant_put_down_inside_token)
      : Result.of_option(
          ~error=Action.Failure.Nothing_to_put_down,
          put_down(z),
        )
  };
