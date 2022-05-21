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
    | Destruct
    | Insert(string)
    // `Construct(d, lbl)` constructs `lbl` starting from `d` side
    | Construct(Direction.t, Tile.Label.t)
    | Pick_up
    | Put_down;

  module Failure = {
    [@deriving (show, sexp)]
    type t =
      | Cant_move
      | Cant_insert
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

let select = (d: Direction.t, z: t): option(t) =>
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

let token_len: Base.Piece.t => option(string) =
  fun
  | Tile({label: [t], _}) => Some(t)
  | _ => None;

let neighbors_tokens = siblings =>
  switch (neighbors(siblings)) {
  | (Some(l), Some(r)) => (token_len(l), token_len(r))
  | (Some(l), None) => (token_len(l), None)
  | (None, Some(r)) => (None, token_len(r))
  | (None, None) => (None, None)
  };

let move =
    (
      d: Direction.t,
      {caret, relatives: {siblings: (l_sibs, r_sibs), _}, _} as z: t,
    )
    : option(t) =>
  //TODO(andrew): cleanup
  switch (d, caret, neighbors_tokens((l_sibs, r_sibs))) {
  | (Left, Outer, (Some(t), _)) when String.length(t) > 1 =>
    z |> set_caret(Inner(String.length(t) - 2)) |> move_outer(d)
  | (Left, Outer, _) => move_outer(d, z) /* non-enerterable */
  | (Left, Inner(0), _) => Some(set_caret(Outer, z))
  | (Left, Inner(n), _) => Some(set_caret(Inner(n - 1), z))
  | (Right, Outer, (_, Some(t))) when String.length(t) > 1 =>
    Some(set_caret(Inner(0), z))
  | (Right, Outer, _) => move_outer(d, z) /* non-enerterable */
  | (Right, Inner(n), (_, Some(t))) when n == String.length(t) - 2 =>
    z |> set_caret(Outer) |> move_outer(d)
  | (Right, Inner(n), _) => Some(set_caret(Inner(n + 1), z))
  };

let map_hd = (f: 'a => 'b, xs: list('a)): list('b) =>
  switch (xs) {
  | [] => []
  | [x, ...xss] => [f(x), ...xss]
  };

let reconstruct_monotile = (f, p: Base.Piece.t): Base.Piece.t =>
  switch (p) {
  | Tile({label: [t], _} as tile) => Tile({...tile, label: [f(t)]})
  | _ => p
  };

let reconstruct_right_monotile = (f: Token.t => Token.t, z: t): t =>
  z
  |> update_siblings(PairUtil.map_snd(map_hd(reconstruct_monotile(f))))
  |> update_selection(z.selection)
  |> snd;

let reconstruct_left_monotile = (f: Token.t => Token.t, z: t): t =>
  z
  |> update_siblings(PairUtil.map_fst(map_hd(reconstruct_monotile(f))))
  |> update_selection(z.selection)
  |> snd;

let is_grout: Base.Piece.t => bool =
  fun
  | Grout(_) => true
  | _ => false;

let is_length_one_monotile: Base.Piece.t => bool =
  p =>
    switch (token_len(p)) {
    | Some(t) => String.length(t) == 1
    | None => false
    };

let can_merge: (caret, Siblings.t) => option((string, string)) =
  (caret, (l_sibs, r_sibs)) =>
    switch (l_sibs) {
    | [Grout(_), ...l_sibs_rest] =>
      switch (caret, neighbors_tokens((l_sibs_rest, r_sibs))) {
      | (Outer, (Some(l), Some(r))) when Molds.has_mold(l ++ r) =>
        Some((l, r))
      | _ => None
      }
    | _ => None
    };

let can_merge_single: (caret, Siblings.t) => option((string, string)) =
  (caret, (l_sibs, r_sibs)) =>
    switch (l_sibs) {
    | [p, ...l_sibs_rest] when is_length_one_monotile(p) =>
      switch (caret, neighbors_tokens((l_sibs_rest, r_sibs))) {
      | (Outer, (Some(l), Some(r))) when Molds.has_mold(l ++ r) =>
        Some((l, r))
      | _ => None
      }
    | _ => None
    };

/* merge precondition: ... <Monotile> Grout | <Monotile> */
let merge = (z, (l, r)) => {
  z
  |> set_caret(Inner(String.length(l) - 1))
  |> select(Right)
  |> Option.map(destruct_outer)
  |> OptUtil.and_then(select(Left))
  |> Option.map(destruct_outer)
  |> Option.map(construct(Left, [l ++ r]))
  |> OptUtil.and_then(move_outer(Left));
};

// TODO(andrew): cleanup/combine with above
let merge_single = (z, (l, r)) => {
  z
  |> set_caret(Inner(String.length(l) - 1))
  |> select(Right)
  |> Option.map(destruct_outer)
  |> OptUtil.and_then(select(Left))
  |> OptUtil.and_then(select(Left))  // to get operator
  |> Option.map(destruct_outer)
  |> Option.map(construct(Left, [l ++ r]))
  |> OptUtil.and_then(move_outer(Left));
};

let decrement_caret: caret => caret =
  fun
  | Outer
  | Inner(0) => Outer
  | Inner(k) => Inner(k - 1);

let destruct =
    ({caret, relatives: {siblings: (l_sibs, r_sibs), _}, _} as z: t)
    : option(t) => {
  //TODO(andrew): check if result is valid token
  let d_outer = z => z |> select(Left) |> Option.map(destruct_outer);
  switch (caret, neighbors_tokens((l_sibs, r_sibs))) {
  | (Inner(k), (_, Some(_t))) =>
    z
    |> reconstruct_right_monotile(StringUtil.remove_nth(k))
    |> update_caret(decrement_caret)
    |> Option.some
  | (Inner(_), (_, None)) => failwith("destruct: impossible 2")
  | (Outer, (Some(t), _)) when String.length(t) > 1 =>
    z
    |> reconstruct_left_monotile(
         StringUtil.remove_nth(String.length(t) - 1),
       )
    |> Option.some
  | (Outer, (Some(_), _)) /* t.length == 1 */
  | (Outer, (None, _)) => d_outer(z)
  };
};

let destruct_or_merge =
    ({caret, relatives: {siblings, _}, _} as z: t): option(t) =>
  switch (can_merge_single(caret, siblings)) {
  | Some(lr) => merge_single(z, lr)
  | None =>
    switch (can_merge(caret, siblings)) {
    | Some(lr) => merge(z, lr)
    | None => destruct(z)
    }
  };

type side_dispatch =
  | CanAddToLeft(string)
  | CanAddToRight(string)
  | CanAddToNeither;

let multilabels: (string, Direction.t) => (list(Token.t), Direction.t) =
  (s, direction_preference) =>
    switch (s) {
    | "(" => (["(", ")"], Left)
    | ")" => (["(", ")"], Right)
    | "[" => (["[", "]"], Left)
    | "]" => (["[", "]"], Right)
    | "?" => (["?", ":"], Left)
    | "fun" => (["fun", "=>"], Left)
    | "in" => (["let", "=", "in"], Right)
    | "let" => (["let", "=", "in"], Left)
    | "=>" => (["fun", "=>"], Right)
    | t => ([t], direction_preference)
    };

let nib_shapes = (p: Base.Piece.t): (Nib.Shape.t, Nib.Shape.t) =>
  switch (p) {
  | Grout(nibs) => nibs
  | Shard({nibs: (l, r), _}) => (l.shape, r.shape)
  | Tile({mold, _}) =>
    let (l, r) = Mold.outer_nibs(mold);
    (l.shape, r.shape);
  };

let has_grout_neighbor: Siblings.t => bool =
  siblings =>
    switch (neighbors(siblings)) {
    | (Some(Grout(_p)), _) /*when Grout.is_space(p)*/ => true
    | (_, Some(Grout(_p))) /*when Grout.is_space(p)*/ => true
    | _ => false
    };

let insert_space_grout =
    (
      _char: string,
      {relatives: {siblings: (l_sibs, r_sibs), _}, _} as z: t,
    ) => {
  let new_grout =
    switch (l_sibs, r_sibs) {
    | ([], []) => failwith("TODO(andrew): insert_space_grout: impossible??")
    | ([], [_, ..._]) => Base.Piece.Grout((Convex, Nib.Shape.concave()))
    | ([p, ..._], _) =>
      let nib_shape_r = p |> nib_shapes |> snd;
      Grout((Nib.Shape.flip(nib_shape_r), nib_shape_r));
    };
  update_siblings(((l, r)) => ([new_grout] @ l, r), z)
  |> update_relatives(Relatives.regrout); //regrout TODO(andrew): do i want to do this?
};

let check_sibs: (string, Siblings.t) => side_dispatch =
  (char, siblings) =>
    switch (neighbors_tokens(siblings)) {
    | (Some(t), _) when Token.is_valid(t ++ char) => CanAddToLeft(t)
    | (_, Some(t)) when Token.is_valid(char ++ t) => CanAddToRight(t)
    | _ => CanAddToNeither
    };

let barf_or_construct =
    (new_token: string, direction_preference: Direction.t, z: t) =>
  if (Backpack.is_first_matching(new_token, z.backpack)) {
    put_down(z);
  } else {
    let (new_label, direction) =
      multilabels(new_token, direction_preference);
    Some(construct(direction, new_label, z));
  };

let insert_outer =
    (char: string, {relatives: {siblings, _}, _} as z: t): option(t) => {
  switch (char) {
  | _ when Token.is_whitespace(char) && !has_grout_neighbor(siblings) =>
    Some(insert_space_grout(char, z))
  //None //TODO(andrew)
  | _ when Token.is_symbol(char) || Token.is_alphanum(char) =>
    switch (check_sibs(char, siblings)) {
    | CanAddToNeither => barf_or_construct(char, Left, z)
    | CanAddToLeft(left_token) =>
      barf_or_construct(left_token ++ char, Left, remove_left_sib(z))
    | CanAddToRight(right_token) =>
      //TODO(andrew): will need to move now in this case?
      barf_or_construct(char ++ right_token, Right, remove_right_sib(z))
    }
  | _ => None
  };
};

let split = (z: t, char: string, idx: int, t: string): option(t) => {
  let (l, r) = StringUtil.split_nth(idx, t);
  z
  |> set_caret(Outer)
  |> select(Right)
  |> Option.map(destruct_outer)
  |> Option.map(construct(Right, [r]))
  |> Option.map(construct(Left, [l]))
  |> OptUtil.and_then(z =>
       Token.is_whitespace(char)
         ? z |> insert_space_grout(char) |> move_outer(Right)
         : z |> construct(Left, [char]) |> Option.some
     );
};

let insert =
    (
      char: string,
      {caret, relatives: {siblings: (l_sibs, r_sibs), _}, _} as z: t,
    )
    : option(t) =>
  switch (caret, neighbors_tokens((l_sibs, r_sibs))) {
  | (Inner(n), (_, Some(t))) =>
    let idx = n + 1;
    let new_t = StringUtil.insert_nth(idx, char, t);
    if (Molds.has_mold(new_t)) {
      z
      |> reconstruct_right_monotile(_ => new_t)
      |> set_caret(Inner(idx))
      |> Option.some;
    } else {
      /* if inserting wouldn't produce a valid token, split */
      split(
        z,
        char,
        idx,
        t,
      );
    };
  | (Inner(_k), (_, None)) =>
    failwith("insert: caret is inner, but left nhbr has no inner positions")
  | (Outer, (_, Some(_))) =>
    z
    |> insert_outer(char)
    |> Option.map(
         set_caret(
           //TODO(andrew): unfuck this (hacky dispatch to proper side)
           switch (check_sibs(char, (l_sibs, r_sibs))) {
           | CanAddToNeither => Outer //TODO(andrew): ?
           | CanAddToLeft(_) => Outer
           | CanAddToRight(_) => Inner(0)
           },
         ),
       )
  | (Outer, (_, None)) => insert_outer(char, z)
  };

let perform = (a: Action.t, z: t): Action.Result.t(t) =>
  switch (a) {
  | Move(d) => Result.of_option(~error=Action.Failure.Cant_move, move(d, z))
  | Select(d) =>
    Result.of_option(~error=Action.Failure.Cant_move, select(d, z))
  | Destruct =>
    Result.of_option(~error=Action.Failure.Cant_move, destruct_or_merge(z))
  | Insert(char) =>
    Result.of_option(~error=Action.Failure.Cant_insert, insert(char, z))
  | Construct(from, label) => Ok(construct(from, label, z))
  | Pick_up => Ok(pick_up(z))
  | Put_down =>
    Result.of_option(~error=Action.Failure.Nothing_to_put_down, put_down(z))
  };
