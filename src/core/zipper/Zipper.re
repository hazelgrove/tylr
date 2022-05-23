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

let remove_right_sib: t => t =
  update_siblings(((l, r)) => (l, r == [] ? [] : List.tl(r)));
let remove_left_sib: t => t =
  update_siblings(((l, r)) => (l == [] ? [] : List.tl(l), r));

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
// else if (d != z.selection.focus
//            || Selection.is_balanced(z.selection)
//            || Backpack.is_balanced(z.backpack)) {
//   let selection = {...z.selection, focus: Direction.toggle(d)};
//   Some(unselect({...z, selection}));
// } else {
//   None;
// };

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
  // |> Backpack.push_s(
  //      to_pick_up
  //      |> List.map(s => {
  //           let (pre, suf) = z.relatives.siblings;
  //           Segment.(
  //             contains_matching(s, pre) || contains_matching(s, suf)
  //           )
  //             ? ([s], s) : ([], s);
  //         })
  //      |> List.map(((meta, s)) =>
  //           (meta, Selection.mk(z.selection.focus, Segment.of_shard(s)))
  //         ),
  //    );
  {...z, backpack};
};

let pick_up = (z: t): t => {
  let (selected, z) = update_selection(Selection.empty, z);
  let selections =
    selected.content
    |> Segment.split_by_grout
    |> Aba.get_as
    |> List.filter((!=)(Segment.empty))
    |> List.map(Selection.mk(selected.focus));
  // |> List.map((sel: Selection.t) => {
  //      let meta =
  //        Segment.shards(sel.content)
  //        |> List.filter((s: Shard.t) => {
  //             let (pre, suf) = z.relatives.siblings;
  //             Segment.(
  //               contains_matching(s, pre) || contains_matching(s, suf)
  //             );
  //           });
  //      (meta, sel);
  //    });
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
  let+ (_, popped, backpack) =
    Backpack.pop(Siblings.shards(z.relatives.siblings), z.backpack);
  // let (pre, suf) = Siblings.shards(z.relatives.siblings);
  // let can_put_down =
  //   meta
  //   |> List.for_all((s: Shard.t) => {
  //        let contains_matching =
  //          List.exists((s': Shard.t) => s.tile_id == s'.tile_id);
  //        let valid_segment =
  //          contains_matching(pre) || contains_matching(suf);
  //        let valid_order =
  //          List.for_all(s' => Shard.index(s') < Shard.index(s), pre)
  //          && List.for_all(s' => Shard.index(s') > Shard.index(s), suf);
  //        valid_segment && valid_order;
  //      });
  // can_put_down
  //   ? Some({...z, backpack} |> put_selection(popped) |> unselect) : None;
  {...z, backpack} |> put_selection(popped) |> unselect;
};

let construct = (from: Direction.t, label: Tile.Label.t, z: t): t => {
  let z = destruct(z);
  let molds = Molds.get(label);
  assert(molds != []);
  // initial mold to typecheck, will be remolded
  let mold = List.hd(molds);
  let (id, id_gen) = IdGen.next(z.id_gen);
  let selections =
    Shard.mk_s(id, label, mold)
    |> ListUtil.rev_if(from == Right)
    |> List.map(Segment.of_shard)
    |> List.map(Selection.mk(from));
  // |> List.mapi((i, s) => i == 0 ? ([], s) : ([s], s))
  // |> List.map(((meta, s)) =>
  //      (meta, Selection.mk(from, Segment.of_shard(s)))
  //    );
  let backpack = Backpack.push_s(selections, z.backpack);
  Option.get(put_down({...z, id_gen, backpack}));
};

let neighbors: Siblings.t => (option(Piece.t), option(Piece.t)) =
  ((l, r)) => (
    l == [] ? None : Some(List.hd(l)),
    r == [] ? None : Some(List.hd(r)),
  );

type side_decision =
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

let check_sibs_alphanum: (string, Siblings.t) => side_decision =
  /* NOTE: logic here could probably be based on character classes
       as opposed to mold shape. Actually, the shape part might
       not even be necessary now that we're checking validity
       TODO(andrew): think about this
     */
  (char, siblings) =>
    switch (neighbors(siblings)) {
    | (Some(Tile({label: [t], _ /*mold,*/})), _)
        when /*mold.shape == Op &&*/ Token.is_valid(t ++ char) =>
      CanAddToLeft(t)
    | (_, Some(Tile({label: [t], _ /*mold,*/})))
        when /*mold.shape == Op &&*/ Token.is_valid(char ++ t) =>
      CanAddToRight(t)
    | _ => CanAddToNeither
    };

let check_sibs_symbol: (string, Siblings.t) => side_decision =
  (char, siblings) =>
    /* NOTE: this is slightly more iffy than the alphanum case,
         as it if post/pre draw from the same char set as infix,
         both left and right could be valid add targets. right now
         we favor right
       */
    switch (neighbors(siblings)) {
    | (Some(Tile({label: [t], _ /*mold,*/})), _)
        when /*mold.shape != Op &&*/ Token.is_valid(t ++ char) =>
      CanAddToLeft(t)
    | (_, Some(Tile({label: [t], _ /*mold,*/})))
        when /*mold.shape != Op &&*/ Token.is_valid(char ++ t) =>
      CanAddToRight(t)
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

let nib_shapes = (p: Base.Piece.t): (Nib.Shape.t, Nib.Shape.t) =>
  switch (p) {
  | Grout(nibs) => nibs
  | Shard({nibs: (l, r), _}) => (l.shape, r.shape)
  | Tile({mold, _}) =>
    let (l, r) = Mold.outer_nibs(mold);
    (l.shape, r.shape);
  };

let is_neighboring_space: Siblings.t => bool =
  siblings =>
    switch (neighbors(siblings)) {
    | (Some(Grout(p)), _) when Grout.is_space(p) => true
    | (_, Some(Grout(p))) when Grout.is_space(p) => true
    | _ => false
    };

let insert_space_grout =
    (
      _char: string,
      {relatives: {siblings: (l_sibs, r_sibs), _}, _} as z: t,
    ) => {
  let new_grout =
    switch (l_sibs, r_sibs) {
    | ([], []) => failwith("TODO(andrew): is this impossible?")
    | ([], [_, ..._]) => Base.Piece.Grout((Convex, Nib.Shape.concave()))
    | ([p, ..._], _) =>
      let nib_shape_r = p |> nib_shapes |> snd;
      Grout((Nib.Shape.flip(nib_shape_r), nib_shape_r));
    };
  update_siblings(((l, r)) => ([new_grout] @ l, r), z);
};

let insert =
    (char: string, {relatives: {siblings, _}, _} as z: t): option(t) => {
  switch (char) {
  | _ when Token.is_whitespace(char) && !is_neighboring_space(siblings) =>
    Some(insert_space_grout(char, z))
  //None //TODO(andrew)
  | _ when Token.is_symbol(char) =>
    switch (check_sibs_symbol(char, siblings)) {
    | CanAddToNeither => barf_or_construct(char, Left, z)
    | CanAddToLeft(left_token) =>
      barf_or_construct(left_token ++ char, Left, remove_left_sib(z))
    | CanAddToRight(right_token) =>
      barf_or_construct(char ++ right_token, Right, remove_right_sib(z))
    }
  | _ when Token.is_alphanum(char) =>
    switch (check_sibs_alphanum(char, siblings)) {
    | CanAddToNeither => barf_or_construct(char, Left, z)
    | CanAddToLeft(left_token) =>
      barf_or_construct(left_token ++ char, Left, remove_left_sib(z))
    | CanAddToRight(right_token) =>
      barf_or_construct(char ++ right_token, Right, remove_right_sib(z))
    }
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
    Result.of_option(~error=Action.Failure.Cant_put_down, put_down(z))
  };
