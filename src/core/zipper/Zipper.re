open Util;
open Sexplib.Std;

// assuming single backpack, shards may appear in selection, backpack, or siblings
[@deriving show]
type t = {
  id_gen: IdGen.t,
  selection: Selection.t,
  backpack: Backpack.t,
  relatives: Relatives.t,
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

let unselect = (z: t): t => {
  let relatives =
    z.relatives
    |> Relatives.prepend(
         Direction.toggle(z.selection.focus),
         z.selection.content,
       )
    |> Relatives.reassemble;
  let selection = Selection.clear(z.selection);
  {...z, selection, relatives};
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

let move = (d: Direction.t, z: t): option(t) =>
  if (Selection.is_empty(z.selection)) {
    open OptUtil.Syntax;
    let balanced = !Backpack.is_balanced(z.backpack);
    let+ (p, relatives) = Relatives.pop(~balanced, d, z.relatives);
    let relatives =
      relatives
      |> Relatives.push(Direction.toggle(d), p)
      |> Relatives.reassemble;
    {...z, relatives};
  } else {
    // TODO restore logic attempting to move d
    Some(unselect(z));
  };

let select = (d: Direction.t, z: t): option(t) =>
  d == z.selection.focus ? grow_selection(z) : shrink_selection(z);

let destruct = (z: t): t => {
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

let pick_up = (z: t): t => {
  let (selected, z) = update_selection(Selection.empty, z);
  let selections =
    selected.content
    |> Segment.split_by_grout
    |> Aba.get_a
    |> List.filter((!=)(Segment.empty))
    |> List.map(Selection.mk(z.selection.focus));
  let backpack =
    Backpack.push_s(
      (z.selection.focus == Left ? Fun.id : List.rev)(selections),
      z.backpack,
    );
  {...z, backpack};
};

let put_down = (z: t): option(t) => {
  open OptUtil.Syntax;
  let z = destruct(z);
  let+ (popped, backpack) = Backpack.pop(z.backpack);
  {...z, backpack} |> put_selection(popped) |> unselect;
};

let construct = (from: Direction.t, label: Tile.Label.t, z: t): t => {
  let z = destruct(z);
  print_endline("construct: label:");
  print_endline(String.concat(" ", label));
  let molds = Molds.get(label);
  assert(molds != []);
  // initial mold to typecheck, will be remolded
  let mold = List.hd(molds);
  let (id, id_gen) = IdGen.next(z.id_gen);
  let selections =
    Shard.mk_s(id, label, mold)
    |> List.map(Segment.of_shard)
    |> List.map(Selection.mk(Direction.toggle(from)))
    |> ListUtil.rev_if(from == Right);
  let backpack = Backpack.push_s(selections, z.backpack);
  Option.get(put_down({...z, id_gen, backpack}));
};

/*
  ROUGH NOTES ON INSERTION
  char can be alphanum or symbol or whitespace
  dont do whitespace for now
  want to either make new piece or add to left or right adjancent token
  we make assumption for now that multi-token tile tokens are inviolate,
  so only consider focal segment
  if there's nothing to left/right, can't add.
  if there is only one adjacency, we can only add to that; no decision to be made
  if thing to left/right is shard, can't add.
  if thing is grout, cant add.
  so only care if tile
  if multitoken tile cant add
  so only care if single token tile
  that that if we are between two pieces
  the only situations we care about here are up to symmetry:
  1. >Bin< | <Op>
  2. <Pre< | <Op>
  3. >Post> | >Bin<
  4. <Pre< | <Pre<
  5. >Post> | >Post>
  (1-2) are not ambiguous because of alphanum/symbol disjointness
  (3-5) require length/disjointness critera
  we are good for now though as currently these are all single-char,
  so for nor (3-5) will always be inserts
  so for now we can just do:
  if char symbol, new piece
  if char alphanum, and op to left, add to op string, and goto check
  if char alphanum, and op to right, add to op string, and goto check
  check: if resulting string is_delim_kw, remove existing token
  and call construct on string
  otherwise, just replace string of current token with new string
 */

let neighbors: Siblings.t => (option(Piece.t), option(Piece.t)) =
  ((l, r)) => (
    l == [] ? None : Some(List.hd(l)),
    r == [] ? None : Some(List.hd(r)),
  );

type side_decision =
  | CanAddToLeft(string)
  | CanAddToRight(string)
  | CanAddToNeither;

let check_sibs: Siblings.t => side_decision =
  siblings =>
    switch (neighbors(siblings)) {
    | (Some(Tile({label: [t], mold: {shape: Op, _}, _})), _) =>
      CanAddToLeft(t)
    | (_, Some(Tile({label: [t], mold: {shape: Op, _}, _}))) =>
      CanAddToRight(t)
    | _ => CanAddToNeither
    };

let multilabels: (string, Direction.t) => (list(Token.t), Direction.t) =
  (s, direction_preference) =>
    switch (s) {
    | "(" => (["(", ")"], Left)
    | ")" => (["(", ")"], Right)
    | "[" => (["[", "]"], Left)
    | "]" => (["[", "]"], Right)
    | "?" => (["?", ":"], Left)
    | "let" => (["let", "=", "in"], Left)
    | "fun" => (["fun", "=>"], Left)
    | t => ([t], direction_preference)
    };

let insert =
    (char: string, {relatives: {siblings, _}, _} as z: t): option(t) => {
  switch (char) {
  | _ when Token.is_whitespace(char) => None //TODO(andrew): implement space
  | _ when Token.is_symbol(char) =>
    let (new_label, direction) = multilabels(char, Left);
    Some(construct(direction, new_label, z));
  | _ when Token.is_alphanum(char) =>
    let z =
      switch (check_sibs(siblings)) {
      | CanAddToLeft(left_token) =>
        let (new_label, direction) = multilabels(left_token ++ char, Left);
        z |> remove_left_sib |> construct(direction, new_label);
      | CanAddToRight(right_token) =>
        let (new_label, direction) = multilabels(char ++ right_token, Right);
        z |> remove_right_sib |> construct(direction, new_label);
      | CanAddToNeither =>
        let (new_label, direction) = multilabels(char, Left);
        construct(direction, new_label, z);
      };
    Some(z);
  | _ => None
  };
};

let perform = (a: Action.t, z: t): Action.Result.t(t) =>
  switch (a) {
  | Move(d) => Result.of_option(~error=Action.Failure.Cant_move, move(d, z))
  | Select(d) =>
    Result.of_option(~error=Action.Failure.Cant_move, select(d, z))
  | Destruct => Ok(destruct(z))
  | Insert(char) =>
    Result.of_option(~error=Action.Failure.Cant_insert, insert(char, z))
  | Construct(from, label) => Ok(construct(from, label, z))
  | Pick_up => Ok(pick_up(z))
  | Put_down =>
    Result.of_option(~error=Action.Failure.Nothing_to_put_down, put_down(z))
  };
