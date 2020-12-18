open Util;
open Core;

// TODO unify
let space = 1;

type open_child_profile = (int, Decoration.OpenChild.profile);
type tile_profile = (int, Decoration.Tile.profile);
type tile_profiles = list(tile_profile);

type term_profile = (tile_profile, list(open_child_profile));

let shift = n => List.map(PairUtil.map_fst((+)(n)));

let shift_term_profile =
    (offset: int, (tile_profile, open_children_profiles)) => {
  let tile_profile = PairUtil.map_fst((+)(offset), tile_profile);
  let open_children_profiles = shift(offset, open_children_profiles);
  (tile_profile, open_children_profiles);
};

module type COMMON_INPUT = {
  module T: Tile.S;
  module Z: ZTile.S with module T := T;

  let length_of_tile: T.t => int;

  let offset_tile: ((ZPath.child_step, ZPath.t), T.t) => int;
  let offset_ztile: Z.ztile => int;

  let is_operand_hole: T.operand => bool;
  let is_operator_hole: T.binop => bool;

  let open_children_of_tile: T.t => list((int, int));
  let closed_children_of_tile: T.t => list((int, int));

  let empty_holes_of_children: T.t => list(list(int));

  let child_length: (ZPath.child_step, T.t) => int;

  let tile_profiles_in_tile:
    (
      ~apply_shadow: bool,
      ~highlight: bool,
      (ZPath.child_step, ZPath.steps),
      T.t
    ) =>
    tile_profiles;

  let term_profile_of_tile:
    ((ZPath.child_step, ZPath.t), T.t) => term_profile;

  let selecting_tiles_in_tile:
    (~side: Direction.t, (ZPath.child_step, ZPath.t), T.t) =>
    (tile_profiles, tile_profiles);
};

module type COMMON = {
  module T: Tile.S;
  module Z: ZTile.S with module T := T;

  let length: T.s => int;

  let children_offsets: (~filter: [ | `Open | `Closed]=?, T.t) => list(int);

  let offset_unzipped: Z.unzipped => int;
  let offset: (ZPath.t, T.s) => int;
  let offset_zipper: (ZPath.t, Z.zipper) => int;

  let empty_holes: T.s => list(int);

  let profile_of_tile:
    (~highlight: bool, ~apply_shadow: bool, T.t) => Decoration.Tile.profile;

  let term_profile: (ZPath.t, T.s) => term_profile;
  let term_profile_in_zipper: (ZPath.t, Z.zipper) => term_profile;

  let tile_profiles_at:
    (~apply_shadow: bool, ~highlight: bool, ZPath.steps, T.s) => tile_profiles;
  let tile_profiles_in_zipper:
    (~apply_shadow: bool, ~highlight: bool, ZPath.steps, Z.zipper) =>
    tile_profiles;

  let selecting_tiles:
    (ZPath.ordered_selection, T.s) => (tile_profiles, tile_profiles);
  let selecting_tiles_in_zipper:
    (~side: Direction.t, ZPath.t, Z.zipper) => (tile_profiles, tile_profiles);

  let restructuring_tiles:
    (ZPath.ordered_selection, ZPath.t, T.s) => (tile_profiles, tile_profiles);
};
module Common =
       (
         T: Tile.S,
         // TODO internalize inner tiles into tile sig
         I: {type t;},
         Z: ZTile.S with module T := T,
         P:
           ZPath.COMMON with
             module T := T and module Z := Z and type inner_tiles := I.t,
         Sort_specific: COMMON_INPUT with module T := T and module Z := Z,
       )
       : (COMMON with module T := T and module Z := Z) => {
  let length_of_tile = Sort_specific.length_of_tile;

  // -1 when ts is empty, which is consistent
  // for use in computing offsets
  let length = (ts: T.s): int =>
    ts
    |> List.map(length_of_tile)
    |> List.map((+)(space))
    |> List.fold_left((+), - space);

  let children_offsets =
      (~filter: option([ | `Open | `Closed])=?, tile): list(int) => {
    P.children(~filter?, tile)
    |> List.map(child_step =>
         Sort_specific.offset_tile((child_step, ([], 0)), tile)
       );
  };

  let offset_unzipped =
    fun
    | None => 0
    | Some(ztile) => Sort_specific.offset_ztile(ztile);

  let offset = ((steps, j): ZPath.t, ts: T.s) =>
    switch (steps) {
    | [] =>
      let (prefix, _) = ListUtil.split_n(j, ts);
      length(prefix);
    | [(tile_step, child_step), ...steps] =>
      let (prefix, tile, _) = ListUtil.split_nth(tile_step, ts);
      length(prefix)
      + space
      + Sort_specific.offset_tile((child_step, (steps, j)), tile);
    };

  let offset_zipper = (path, (zipped, unzipped)) =>
    offset_unzipped(unzipped) + offset(path, zipped);

  let empty_holes_of_tile = (t: T.t): list(int) =>
    switch (t) {
    | Operand(op) when Sort_specific.is_operand_hole(op) => [0]
    | BinOp(bin) when Sort_specific.is_operator_hole(bin) => [0]
    | _ =>
      Sort_specific.empty_holes_of_children(t)
      |> List.combine(children_offsets(t))
      |> List.map(((offset, origins)) =>
           List.map((+)(offset + space), origins)
         )
      |> List.flatten
    };

  let empty_holes = (ts: T.s): list(int) => {
    let (_, holes) =
      ts
      |> ListUtil.fold_left_map(
           (start, tile: T.t) => {
             let origins = empty_holes_of_tile(tile);
             (
               start + length_of_tile(tile) + space,
               List.map((+)(start), origins),
             );
           },
           0,
         );
    List.concat(holes);
  };

  let profile_of_tile =
      (~highlight, ~apply_shadow, t: T.t): Decoration.Tile.profile =>
    Decoration.Tile.{
      shape:
        switch (t) {
        | Operand(_) => Operand()
        | PreOp(_) => PreOp()
        | PostOp(_) => PostOp()
        | BinOp(_) => BinOp()
        },
      len: length_of_tile(t),
      open_children: Sort_specific.open_children_of_tile(t),
      closed_children: Sort_specific.closed_children_of_tile(t),
      empty_holes: empty_holes_of_tile(t),
      sort: T.sort,
      highlight,
      apply_shadow,
    };
  let tile_profiles = (~highlight, ~apply_shadow, ts) =>
    ts
    |> ListUtil.fold_left_map(
         (offset, tile) => {
           let profile = profile_of_tile(~highlight, ~apply_shadow, tile);
           (offset + profile.len + space, (offset, profile));
         },
         0,
       )
    |> snd;

  let tile_profiles_at =
      (~apply_shadow, ~highlight, steps: ZPath.steps, ts: T.s) =>
    switch (steps) {
    | [] => tile_profiles(~apply_shadow, ~highlight, ts)
    | [(tile_step, child_step), ...steps] =>
      let (prefix, tile, _) = ListUtil.split_nth(tile_step, ts);
      Sort_specific.tile_profiles_in_tile(
        ~apply_shadow,
        ~highlight,
        (child_step, steps),
        tile,
      )
      |> shift(length(prefix) + space);
    };
  let tile_profiles_in_zipper =
      (~apply_shadow, ~highlight, steps, (zipped, unzipped)) =>
    tile_profiles_at(~apply_shadow, ~highlight, steps, zipped)
    |> shift(offset_unzipped(unzipped));

  module Ts = Tiles.Make(T);

  let term_profile_of_root = {
    let profile_of_tile =
      profile_of_tile(~apply_shadow=true, ~highlight=true);
    let open_child_profile = (side, len) =>
      Decoration.OpenChild.{sort: T.sort, side, len};
    Tile.get(
      op => ((0, profile_of_tile(Operand(op))), []),
      ((pre, r)) => {
        let pre_profile = profile_of_tile(Tile.PreOp(pre));
        let r_profile = open_child_profile(Right, length(r));
        ((0, pre_profile), [(pre_profile.len + space, r_profile)]);
      },
      ((l, post)) => {
        let l_profile = open_child_profile(Left, length(l));
        (
          (l_profile.len + space, profile_of_tile(PostOp(post))),
          [(0, l_profile)],
        );
      },
      ((l, bin, r)) => {
        let l_profile = open_child_profile(Left, length(l));
        let bin_profile = profile_of_tile(Tile.BinOp(bin));
        let r_profile = open_child_profile(Right, length(r));
        (
          (l_profile.len + space, bin_profile),
          [
            (0, l_profile),
            (l_profile.len + space + bin_profile.len + space, r_profile),
          ],
        );
      },
    );
  };

  let term_profile = ((steps, j), ts) =>
    switch (steps) {
    | [] =>
      let n = j == List.length(ts) ? j - 1 : j;
      let ZList.{prefix, z, _} = Ts.nth_root(n, ts);
      shift_term_profile(length(prefix) + space, term_profile_of_root(z));
    | [(tile_step, child_step)]
        when
          j
          == Sort_specific.child_length(child_step, List.nth(ts, tile_step)) =>
      let ZList.{prefix, z, _} = Ts.nth_root(tile_step, ts);
      shift_term_profile(length(prefix) + space, term_profile_of_root(z));
    | [(tile_step, child_step), ...steps] =>
      let (prefix, tile, _) = ListUtil.split_nth(tile_step, ts);
      shift_term_profile(
        length(prefix) + space,
        Sort_specific.term_profile_of_tile((child_step, (steps, j)), tile),
      );
    };
  let term_profile_in_zipper = (path, (zipped, unzipped)) =>
    term_profile(path, zipped)
    |> shift_term_profile(offset_unzipped(unzipped));

  let selecting_tiles = (((steps_l, j_l), (steps_r, j_r)), ts) => {
    let tile_profiles = tile_profiles(~apply_shadow=false, ~highlight=false);
    switch (steps_l, steps_r) {
    | ([], []) =>
      let (prefix, selected, suffix) = ListUtil.split_sublist(j_l, j_r, ts);
      let (prefix_len, selected_len) =
        TupleUtil.map2(length, (prefix, selected));
      let prefix = tile_profiles(prefix);
      let selected = tile_profiles(selected) |> shift(prefix_len + space);
      let suffix =
        tile_profiles(suffix)
        |> shift(prefix_len + space + selected_len + space);
      (selected, prefix @ suffix);
    | ([], [(tile_step_r, child_step_r), ...steps_r]) =>
      let (prefix, tile, _) = ListUtil.split_nth(tile_step_r, ts);
      let (prefix, selected) = ListUtil.split_n(j_l, prefix);
      let (prefix_len, selected_len) =
        TupleUtil.map2(length, (prefix, selected));
      let prefix = tile_profiles(prefix);
      let selected = tile_profiles(selected) |> shift(prefix_len + space);
      let (selected_r, targets_r) =
        Sort_specific.selecting_tiles_in_tile(
          ~side=Left,
          (child_step_r, (steps_r, j_r)),
          tile,
        )
        |> TupleUtil.map2(shift(prefix_len + space + selected_len + space));
      (selected @ selected_r, prefix @ targets_r);
    | ([(tile_step_l, child_step_l), ...steps_l], []) =>
      let (prefix, tile, suffix) = ListUtil.split_nth(tile_step_l, ts);
      let (selected, suffix) =
        ListUtil.split_n(j_r - List.length(prefix) - 1, suffix);
      let (prefix_len, selected_len) =
        TupleUtil.map2(length, (prefix, selected));
      let tile_len = Sort_specific.length_of_tile(tile);
      let (targets_l, selected_l) =
        Sort_specific.selecting_tiles_in_tile(
          ~side=Right,
          (child_step_l, (steps_l, j_l)),
          tile,
        )
        |> TupleUtil.map2(shift(prefix_len + space));
      let selected =
        tile_profiles(selected)
        |> shift(prefix_len + space + tile_len + space);
      let suffix =
        tile_profiles(suffix)
        |> shift(prefix_len + space + tile_len + space + selected_len + space);
      (selected_l @ selected, targets_l @ suffix);
    | (
        [(tile_step_l, child_step_l), ...steps_l],
        [(tile_step_r, child_step_r), ...steps_r],
      ) =>
      let (prefix, tile_r, _) = ListUtil.split_nth(tile_step_r, ts);
      let (prefix, tile_l, selected) =
        ListUtil.split_nth(tile_step_l, prefix);
      let (prefix_len, selected_len) =
        TupleUtil.map2(length, (prefix, selected));
      let (l_len, r_len) =
        TupleUtil.map2(Sort_specific.length_of_tile, (tile_l, tile_r));
      let (targets_l, selected_l) =
        Sort_specific.selecting_tiles_in_tile(
          ~side=Right,
          (child_step_l, (steps_l, j_l)),
          tile_l,
        )
        |> TupleUtil.map2(shift(prefix_len + space));
      let selected =
        tile_profiles(selected) |> shift(prefix_len + space + l_len + space);
      let (selected_r, targets_r) =
        Sort_specific.selecting_tiles_in_tile(
          ~side=Right,
          (child_step_r, (steps_r, j_r)),
          tile_r,
        )
        |> TupleUtil.map2(
             shift(
               prefix_len
               + space
               + l_len
               + space
               + selected_len
               + space
               + r_len
               + space,
             ),
           );
      (selected_l @ selected @ selected_r, targets_l @ targets_r);
    };
  };

  let selecting_tiles_in_zipper =
      (~side: Direction.t, path, (zipped, unzipped)) => {
    let selection =
      switch (side) {
      | Left => (([], 0), path)
      | Right => (path, ([], List.length(zipped)))
      };
    selecting_tiles(selection, zipped)
    |> TupleUtil.map2(shift(offset_unzipped(unzipped)));
  };

  let restructuring_tiles =
      (
        ((steps_l, _), (steps_r, _)) as selection,
        (steps_t, _) as target,
        ts,
      ) =>
    if (steps_t != steps_l && steps_t != steps_r) {
      let (selected_tiles, _) = selecting_tiles(selection, ts);
      let target_tiles =
        tile_profiles_at(
          ~apply_shadow=false,
          ~highlight=false,
          fst(target),
          ts,
        );
      (selected_tiles, target_tiles);
    } else {
      selecting_tiles(selection, ts);
    };
};

module type ERR_HOLE_INPUT = {
  module T: Tile.S;
  let get_hole_status: T.s => HoleStatus.t;
  let err_holes_of_children:
    T.t => list(list((int, Decoration.ErrHole.profile)));
  let err_holes_of_children_z:
    ((ZPath.child_step, ZPath.t), T.t) =>
    list(list((int, Decoration.ErrHole.profile)));
};

module type ERR_HOLE = {
  module T: Tile.S;
  let err_holes: T.s => list((int, Decoration.ErrHole.profile));
  let err_holes_z: (ZPath.t, T.s) => list((int, Decoration.ErrHole.profile));
};
module ErrHole =
       (
         T: Tile.S,
         Z: ZTile.S with module T := T,
         S: {
           include COMMON_INPUT with module T := T and module Z := Z;
           include COMMON with module T := T and module Z := Z;
           include ERR_HOLE_INPUT with module T := T;
         },
       )
       : (ERR_HOLE with module T := T) => {
  module Ts = Tiles.Make(T);

  let root_tile =
    fun
    | Tile.Operand(op) => Tile.Operand(op)
    | PreOp((pre, _)) => PreOp(pre)
    | PostOp((_, post)) => PostOp(post)
    | BinOp((_, bin, _)) => BinOp(bin);

  let shift = n => List.map(PairUtil.map_fst((+)(n + space)));

  let rec err_holes = (ts: T.s) => {
    let outer_hole: list((int, Decoration.ErrHole.profile)) =
      switch (S.get_hole_status(ts)) {
      | NotInHole => []
      | InHole => [(0, {expanded: false, len: S.length(ts)})]
      };
    let inner_holes = {
      let root = Ts.root(ts);
      let tile = root_tile(root);
      let tile_holes =
        S.err_holes_of_children(tile)
        |> List.combine(S.children_offsets(tile))
        |> List.map(((offset, err_holes)) =>
             List.map(PairUtil.map_fst((+)(offset)), err_holes)
           )
        |> List.flatten;
      switch (Ts.root(ts)) {
      | Operand(_) => tile_holes
      | PreOp((_, r)) =>
        let r_holes = shift(S.length_of_tile(tile), err_holes(r));
        tile_holes @ r_holes;
      | PostOp((l, _)) =>
        let l_holes = err_holes(l);
        let tile_holes = shift(S.length(l), tile_holes);
        l_holes @ tile_holes;
      | BinOp((l, _, r)) =>
        let l_holes = err_holes(l);
        let tile_holes = shift(S.length(l), tile_holes);
        let r_holes =
          shift(
            S.length(l) + space + S.length_of_tile(tile),
            err_holes(r),
          );
        l_holes @ tile_holes @ r_holes;
      };
    };
    outer_hole @ inner_holes;
  };

  let rec err_holes_z = ((steps, j): ZPath.t, ts: T.s) => {
    let outer_hole: list((int, Decoration.ErrHole.profile)) =
      switch (S.get_hole_status(ts)) {
      | NotInHole => []
      | InHole => [(0, {expanded: true, len: S.length(ts)})]
      };
    let inner_holes = {
      let root = Ts.root(ts);
      let tile = root_tile(root);
      let position = children_holes =>
        children_holes
        |> List.combine(S.children_offsets(tile))
        |> List.map(((offset, err_holes)) =>
             List.map(PairUtil.map_fst((+)(offset)), err_holes)
           )
        |> List.flatten;
      switch (steps) {
      | [] =>
        let tile_holes = position(S.err_holes_of_children(tile));
        switch (root) {
        | Operand(_) => tile_holes
        | PreOp((_, r)) =>
          let err_holes = j == 0 ? err_holes : err_holes_z(([], j - 1));
          tile_holes @ shift(S.length_of_tile(tile), err_holes(r));
        | PostOp((l, _)) =>
          let err_holes =
            j == List.length(l) ? err_holes : err_holes_z(([], j));
          err_holes(l) @ shift(S.length(l), tile_holes);
        | BinOp((l, _, r)) =>
          let n = List.length(l);
          let (err_holes_l, err_holes_r) =
            if (j < n) {
              (err_holes_z(([], j)), err_holes);
            } else if (j > n) {
              (err_holes, err_holes_z(([], j - (n + 1))));
            } else {
              (err_holes, err_holes);
            };
          err_holes_l(l)
          @ shift(S.length(l), tile_holes)
          @ shift(
              S.length(l) + space + S.length_of_tile(tile),
              err_holes_r(r),
            );
        };
      | [(tile_step, child_step) as two_step, ...steps] =>
        let in_tile =
          switch (root) {
          | Operand(_) =>
            assert(tile_step == 0);
            true;
          | PreOp(_) => tile_step == 0
          | PostOp((l, _))
          | BinOp((l, _, _)) => tile_step == List.length(l)
          };
        let tile_holes =
          position(
            in_tile
              ? S.err_holes_of_children_z((child_step, (steps, j)), tile)
              : S.err_holes_of_children(tile),
          );
        switch (root) {
        | Operand(_) => tile_holes
        | PreOp((_, r)) =>
          tile_holes
          @ shift(
              S.length_of_tile(tile),
              in_tile
                ? err_holes(r)
                : err_holes_z(
                    ([(tile_step - 1, child_step), ...steps], j),
                    r,
                  ),
            )
        | PostOp((l, _)) =>
          (
            in_tile
              ? err_holes(l) : err_holes_z(([two_step, ...steps], j), l)
          )
          @ shift(S.length(l), tile_holes)
        | BinOp((l, _, r)) =>
          let n = List.length(l);
          let (err_holes_l, err_holes_r) =
            if (tile_step < n) {
              (err_holes_z(([two_step, ...steps], j)), err_holes);
            } else if (tile_step > n) {
              (
                err_holes,
                err_holes_z((
                  [(tile_step - (n + 1), child_step), ...steps],
                  j,
                )),
              );
            } else {
              (err_holes, err_holes);
            };
          err_holes_l(l)
          @ shift(S.length(l), tile_holes)
          @ shift(
              S.length(l) + space + S.length_of_tile(tile),
              err_holes_r(r),
            );
        };
      };
    };
    outer_hole @ inner_holes;
  };
};

module type TYP = {
  include COMMON_INPUT with module T := HTyp.T and module Z := ZTyp;
  include COMMON with module T := HTyp.T and module Z := ZTyp;
};
module rec Typ: TYP = {
  open HTyp.T;
  open ZTyp;

  let length_of_tile: HTyp.T.t => int =
    Tile.get(
      fun
      | OperandHole => 1
      | Num => 3
      | Bool => 4
      | Paren(body) => 1 + space + Typ.length(body) + space + 1,
      () => raise(Void_PreOp),
      () => raise(Void_PostOp),
      fun
      | OperatorHole
      | Arrow => 1,
    );

  let offset_tile = ((child_step, path), tile) => {
    let `Typ(zipper) = ZPath.Typ.unzip_tile(child_step, tile, ZTyp.mk());
    Typ.offset_zipper(path, zipper);
  };

  let offset_ztile =
    Tile.get(
      fun
      | ParenZ_body(_) => 1 + space,
      () => raise(Void_ZPreOp),
      fun
      | AnnZ_ann(_) => raise(ZPath.Unzip_rezip_changes_sort),
      () => raise(Void_ZBinOp),
    );

  let is_operand_hole =
    fun
    | OperandHole => true
    | _ => false;
  let is_operator_hole =
    fun
    | OperatorHole => true
    | _ => false;
  let open_children_of_tile = tile =>
    tile
    |> Tile.get(
         fun
         | OperandHole
         | Num
         | Bool => []
         | Paren(body) => [Typ.length(body)],
         () => raise(Void_PreOp),
         () => raise(Void_PostOp),
         fun
         | Arrow
         | OperatorHole => [],
       )
    |> List.combine(Typ.children_offsets(~filter=`Open, tile));
  let closed_children_of_tile = tile =>
    tile
    |> Tile.get(
         fun
         | OperandHole
         | Num
         | Bool
         | Paren(_) => [],
         () => raise(HTyp.T.Void_PreOp),
         () => raise(HTyp.T.Void_PostOp),
         fun
         | Arrow
         | OperatorHole => [],
       )
    |> List.combine(Typ.children_offsets(~filter=`Closed, tile));

  let empty_holes_of_children =
    Tile.get(
      fun
      | OperandHole
      | Num
      | Bool => []
      | Paren(body) => [Typ.empty_holes(body)],
      () => raise(Void_PreOp),
      () => raise(Void_PostOp),
      fun
      | OperatorHole
      | Arrow => [],
    );

  let term_profile_of_tile = ((child_step, path), tile) => {
    let `Typ(zipper) = ZPath.Typ.unzip_tile(child_step, tile, ZTyp.mk());
    Typ.term_profile_in_zipper(path, zipper);
  };

  let tile_profiles_in_tile =
      (~apply_shadow, ~highlight, (child_step, steps), tile) => {
    let `Typ(zipper) = ZPath.Typ.unzip_tile(child_step, tile, ZTyp.mk());
    Typ.tile_profiles_in_zipper(~apply_shadow, ~highlight, steps, zipper);
  };

  let child_length = (child_step, tile) => {
    let `Typ(ty, _) = ZPath.Typ.unzip_tile(child_step, tile, ZTyp.mk());
    List.length(ty);
  };

  let selecting_tiles_in_tile =
      (~side: Direction.t, (child_step, path), tile) => {
    let `Typ(zipper) = ZPath.Typ.unzip_tile(child_step, tile, ZTyp.mk());
    Typ.selecting_tiles_in_zipper(~side, path, zipper);
  };

  include Common(HTyp.T, HTyp.Inner, ZTyp, ZPath.Typ, Typ);
};

module type PAT = {
  include COMMON_INPUT with module T := HPat.T and module Z := ZPat;
  include COMMON with module T := HPat.T and module Z := ZPat;
  include ERR_HOLE_INPUT with module T := HPat.T;
  include ERR_HOLE with module T := HPat.T;
};
module rec Pat: PAT = {
  open HPat.T;
  open ZPat;

  let length_of_tile: HPat.T.t => int =
    Tile.get(
      fun
      | OperandHole => 1
      | Var(x) => String.length(x)
      | Paren(body) => 1 + space + Pat.length(body) + space + 1,
      () => raise(Void_PreOp),
      fun
      | Ann(_, ann) => 1 + space + Typ.length(ann) + space,
      fun
      | OperatorHole => 1,
    );

  let offset_ztile =
    Tile.get(
      fun
      | ParenZ_body(_) => 1 + space,
      fun
      | LamZ_pat(_)
      | LetZ_pat(_) => raise(ZPath.Unzip_rezip_changes_sort),
      () => raise(Void_ZPostOp),
      () => raise(Void_ZBinOp),
    );

  let offset_tile = ((child_step, path), tile) =>
    switch (ZPath.Pat.unzip_tile(child_step, tile, ZPat.mk())) {
    | `Typ(zipper) => Typ.offset_zipper(path, zipper)
    | `Pat(zipper) => Pat.offset_zipper(path, zipper)
    };

  let is_operand_hole =
    fun
    | OperandHole => true
    | _ => false;
  let is_operator_hole =
    fun
    | OperatorHole => true;
  let open_children_of_tile = tile =>
    tile
    |> Tile.get(
         fun
         | OperandHole
         | Var(_) => []
         | Paren(body) => [Pat.length(body)],
         () => raise(HPat.T.Void_PreOp),
         fun
         | Ann(_) => [],
         fun
         | OperatorHole => [],
       )
    |> List.combine(Pat.children_offsets(~filter=`Open, tile));
  let closed_children_of_tile = tile =>
    tile
    |> Tile.get(
         fun
         | OperandHole
         | Var(_)
         | Paren(_) => [],
         () => raise(Void_PreOp),
         fun
         | Ann(_, ann) => [Typ.length(ann)],
         fun
         | OperatorHole => [],
       )
    |> List.combine(Pat.children_offsets(~filter=`Closed, tile));

  let empty_holes_of_children =
    Tile.get(
      fun
      | OperandHole
      | Var(_) => []
      | Paren(body) => [Pat.empty_holes(body)],
      () => raise(Void_PreOp),
      fun
      | Ann(_, ty) => [Typ.empty_holes(ty)],
      fun
      | OperatorHole => [],
    );

  let child_length = (child_step, tile) =>
    switch (ZPath.Pat.unzip_tile(child_step, tile, ZPat.mk())) {
    | `Typ(ty, _) => List.length(ty)
    | `Pat(p, _) => List.length(p)
    };

  let term_profile_of_tile = ((child_step, path), tile) =>
    switch (ZPath.Pat.unzip_tile(child_step, tile, ZPat.mk())) {
    | `Typ(zipper) => Typ.term_profile_in_zipper(path, zipper)
    | `Pat(zipper) => Pat.term_profile_in_zipper(path, zipper)
    };

  let tile_profiles_in_tile =
      (~apply_shadow, ~highlight, (child_step, steps), tile) =>
    switch (ZPath.Pat.unzip_tile(child_step, tile, ZPat.mk())) {
    | `Typ(zipper) =>
      Typ.tile_profiles_in_zipper(~apply_shadow, ~highlight, steps, zipper)
    | `Pat(zipper) =>
      Pat.tile_profiles_in_zipper(~apply_shadow, ~highlight, steps, zipper)
    };

  let selecting_tiles_in_tile = (~side, (child_step, path), tile) =>
    switch (ZPath.Pat.unzip_tile(child_step, tile, ZPat.mk())) {
    | `Typ(zipper) => Typ.selecting_tiles_in_zipper(~side, path, zipper)
    | `Pat(zipper) => Pat.selecting_tiles_in_zipper(~side, path, zipper)
    };

  let get_hole_status = HPat.get_hole_status;

  let err_holes_of_children =
    Tile.get(
      fun
      | OperandHole
      | Var(_) => []
      | Paren(body) => [Pat.err_holes(body)],
      () => raise(Void_PreOp),
      fun
      | Ann(_) => [[]],
      fun
      | OperatorHole => [],
    );

  let err_holes_of_children_z = ((child_step, path), tile) =>
    switch (ZPath.Pat.unzip_tile(child_step, tile, ZPat.mk())) {
    | `Typ(_, unzipped) =>
      Option.get(unzipped)
      |> Tile.get(
           fun
           | ZTyp.ParenZ_body(_) => raise(ZPath.Unzip_rezip_changes_sort),
           () => raise(ZTyp.Void_ZPreOp),
           fun
           | ZTyp.AnnZ_ann(_) => [[]],
           () => raise(ZTyp.Void_ZBinOp),
         )
    | `Pat(p, unzipped) =>
      Option.get(unzipped)
      |> Tile.get(
           fun
           | ZPat.ParenZ_body(_) => [Pat.err_holes_z(path, p)],
           fun
           | ZPat.LamZ_pat(_)
           | LetZ_pat(_) => raise(ZPath.Unzip_rezip_changes_sort),
           () => raise(ZPat.Void_ZPostOp),
           () => raise(ZPat.Void_ZBinOp),
         )
    };

  include Common(HPat.T, HPat.Inner, ZPat, ZPath.Pat, Pat);
  include ErrHole(HPat.T, ZPat, Pat);
};

module type EXP = {
  include COMMON_INPUT with module T := HExp.T and module Z := ZExp;
  include COMMON with module T := HExp.T and module Z := ZExp;
  include ERR_HOLE_INPUT with module T := HExp.T;
  include ERR_HOLE with module T := HExp.T;
};
module rec Exp: EXP = {
  open HExp.T;
  open ZExp;

  let length_of_tile =
    Tile.get(
      fun
      | OperandHole => 1
      | Var(_, x) => String.length(x)
      | Num(_, n) => String.length(string_of_int(n))
      | Paren(body) => 1 + space + Exp.length(body) + space + 1,
      fun
      | Lam(_, p) => 1 + space + Pat.length(p) + space + 1
      | Let(p, def) =>
        3
        + space
        + Pat.length(p)
        + space
        + 1
        + space
        + Exp.length(def)
        + space
        + 2,
      fun
      | Ap(_, arg) => 1 + space + Exp.length(arg) + space + 1,
      fun
      | OperatorHole
      | Plus(_) => 1,
    );

  let offset_ztile =
    Tile.get(
      fun
      | ParenZ_body(_) => 1 + space,
      fun
      | LetZ_def(p, _) => 3 + space + Pat.length(p) + 1 + space,
      fun
      | ApZ_arg(_) => 1 + space,
      () =>
      raise(Void_ZBinOp)
    );

  let offset_tile = ((child_step, path), tile) =>
    switch (ZPath.Exp.unzip_tile(child_step, tile, ZExp.mk())) {
    | `Pat(zipper) => Pat.offset_zipper(path, zipper)
    | `Exp(zipper) => Exp.offset_zipper(path, zipper)
    };

  let is_operand_hole =
    fun
    | OperandHole => true
    | _ => false;
  let is_operator_hole =
    fun
    | OperatorHole => true
    | _ => false;
  let open_children_of_tile = tile =>
    tile
    |> Tile.get(
         fun
         | OperandHole
         | Var(_)
         | Num(_) => []
         | Paren(body) => [Exp.length(body)],
         fun
         | Lam(_) => []
         | Let(_, def) => [Exp.length(def)],
         fun
         | Ap(_, arg) => [Exp.length(arg)],
         fun
         | Plus(_)
         | OperatorHole => [],
       )
    |> List.combine(Exp.children_offsets(~filter=`Open, tile));
  let closed_children_of_tile = tile =>
    tile
    |> Tile.get(
         fun
         | OperandHole
         | Var(_)
         | Num(_)
         | Paren(_) => [],
         fun
         | Lam(_, p) => [Pat.length(p)]
         | Let(p, _) => [Pat.length(p)],
         fun
         | Ap(_) => [],
         fun
         | Plus(_)
         | OperatorHole => [],
       )
    |> List.combine(Exp.children_offsets(~filter=`Closed, tile));

  let empty_holes_of_children =
    Tile.get(
      fun
      | OperandHole
      | Num(_)
      | Var(_) => []
      | Paren(body) => [Exp.empty_holes(body)],
      fun
      | Lam(_, p) => [Pat.empty_holes(p)]
      | Let(p, def) => [Pat.empty_holes(p), Exp.empty_holes(def)],
      fun
      | Ap(_, arg) => [Exp.empty_holes(arg)],
      fun
      | OperatorHole
      | Plus(_) => [],
    );

  let child_length = (child_step, tile) =>
    switch (ZPath.Exp.unzip_tile(child_step, tile, ZExp.mk())) {
    | `Exp(e, _) => List.length(e)
    | `Pat(p, _) => List.length(p)
    };

  let term_profile_of_tile = ((child_step, path), tile) =>
    switch (ZPath.Exp.unzip_tile(child_step, tile, ZExp.mk())) {
    | `Exp(zipper) => Exp.term_profile_in_zipper(path, zipper)
    | `Pat(zipper) => Pat.term_profile_in_zipper(path, zipper)
    };

  let tile_profiles_in_tile =
      (~apply_shadow, ~highlight, (child_step, steps), tile) =>
    switch (ZPath.Exp.unzip_tile(child_step, tile, ZExp.mk())) {
    | `Exp(zipper) =>
      Exp.tile_profiles_in_zipper(~apply_shadow, ~highlight, steps, zipper)
    | `Pat(zipper) =>
      Pat.tile_profiles_in_zipper(~apply_shadow, ~highlight, steps, zipper)
    };

  let selecting_tiles_in_tile = (~side, (child_step, path), tile) =>
    switch (ZPath.Exp.unzip_tile(child_step, tile, ZExp.mk())) {
    | `Exp(zipper) => Exp.selecting_tiles_in_zipper(~side, path, zipper)
    | `Pat(zipper) => Pat.selecting_tiles_in_zipper(~side, path, zipper)
    };

  let get_hole_status = HExp.get_hole_status;

  let err_holes_of_children =
    Tile.get(
      fun
      | OperandHole
      | Num(_)
      | Var(_) => []
      | Paren(body) => [Exp.err_holes(body)],
      fun
      | Lam(_, p) => [Pat.err_holes(p)]
      | Let(p, def) => [Pat.err_holes(p), Exp.err_holes(def)],
      fun
      | Ap(_, arg) => [Exp.err_holes(arg)],
      fun
      | OperatorHole
      | Plus(_) => [],
    );

  let err_holes_of_children_z = ((child_step, path), tile) =>
    switch (ZPath.Exp.unzip_tile(child_step, tile, ZExp.mk())) {
    | `Pat(p, unzipped) =>
      Option.get(unzipped)
      |> Tile.get(
           fun
           | ZPat.ParenZ_body(_) => raise(ZPath.Unzip_rezip_changes_sort),
           fun
           | ZPat.LamZ_pat(_) => [Pat.err_holes_z(path, p)]
           | LetZ_pat(_, def) => [
               Pat.err_holes_z(path, p),
               Exp.err_holes(def),
             ],
           () => raise(ZPat.Void_ZPostOp),
           () => raise(ZPat.Void_ZBinOp),
         )
    | `Exp(e, unzipped) =>
      Option.get(unzipped)
      |> Tile.get(
           (
             fun
             | ZExp.ParenZ_body(_) => [Exp.err_holes_z(path, e)]
           ),
           (
             fun
             | ZExp.LetZ_def(p, _) => [
                 Pat.err_holes(p),
                 Exp.err_holes_z(path, e),
               ]
           ),
           (
             fun
             | ZExp.ApZ_arg(_) => [Exp.err_holes_z(path, e)]
           ),
           () =>
           raise(ZExp.Void_ZBinOp)
         )
    };

  include Common(HExp.T, HExp.Inner, ZExp, ZPath.Exp, Exp);
  include ErrHole(HExp.T, ZExp, Exp);
};
