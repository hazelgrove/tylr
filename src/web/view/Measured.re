open Util;
open Core;

// TODO unify
let space = 1;

module type COMMON = {
  module T: Tile.S;

  let length_of_tile: T.t => int;
  let length: T.s => int;

  let children_offsets: (~filter: [ | `Open | `Closed]=?, T.t) => list(int);
  let offset: (ZPath.t, T.s) => int;

  let empty_holes: T.s => list(int);

  let profile_of_tile: T.t => Decoration.Tile.profile;
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
         Sort_specific: {
           let length_of_tile: T.t => int;
           let offset_tile: ((ZPath.child_step, ZPath.t), T.t) => int;
           let is_operand_hole: T.operand => bool;
           let is_operator_hole: T.binop => bool;
           let open_children_of_tile: T.t => list((int, int));
           let closed_children_of_tile: T.t => list((int, int));
           let inner_empty_holes_of_tile: T.t => list(list(int));
         },
       ) => {
  let length_of_tile = Sort_specific.length_of_tile;

  let length = (ts: T.s): int =>
    switch (ts) {
    | [] => 0
    | [_, ..._] =>
      ts
      |> List.map(length_of_tile)
      |> List.map((+)(1))
      |> List.fold_left((+), -1)
    };

  let children_offsets =
      (~filter: option([ | `Open | `Closed])=?, tile): list(int) => {
    P.children(~filter?, tile)
    |> List.map(child_step =>
         Sort_specific.offset_tile((child_step, ([], 0)), tile)
       );
  };

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

  let empty_holes = (ts: T.s): list(int) => {
    let (_, holes) =
      ts
      |> ListUtil.fold_left_map(
           (start, tile: T.t) => {
             let origins =
               switch (tile) {
               | Operand(op) when Sort_specific.is_operand_hole(op) => [0]
               | BinOp(bin) when Sort_specific.is_operator_hole(bin) => [0]
               | _ =>
                 Sort_specific.inner_empty_holes_of_tile(tile)
                 |> List.combine(children_offsets(tile))
                 |> List.map(((offset, origins)) =>
                      List.map((+)(offset), origins)
                    )
                 |> List.flatten
               };
             (
               start + length_of_tile(tile) + space,
               List.map((+)(start), origins),
             );
           },
           0,
         );
    List.concat(holes);
  };

  let profile_of_tile = (t: T.t) =>
    Decoration.Tile.{
      shape:
        switch (t) {
        | Operand(operand) =>
          `Operand(Sort_specific.is_operand_hole(operand))
        | PreOp(_) => `PreOp
        | PostOp(_) => `PostOp
        | BinOp(binop) => `BinOp(Sort_specific.is_operator_hole(binop))
        },
      len: length_of_tile(t),
      open_children: Sort_specific.open_children_of_tile(t),
      closed_children: Sort_specific.closed_children_of_tile(t),
    };
};

module type ERR_HOLE = {
  module T: Tile.S;
  let err_holes: T.s => list((int, Decoration.ErrHole.profile));
  let err_holes_z: (ZPath.t, T.s) => list((int, Decoration.ErrHole.profile));
};
module ErrHole =
       (
         T: Tile.S,
         C: COMMON with module T := T,
         Sort_specific: {
           let get_hole_status: T.s => HoleStatus.t;
           let inner_err_holes:
             T.t => list(list((int, Decoration.ErrHole.profile)));
           let inner_err_holes_z:
             ((ZPath.child_step, ZPath.t), T.t) =>
             list(list((int, Decoration.ErrHole.profile)));
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
      switch (Sort_specific.get_hole_status(ts)) {
      | NotInHole => []
      | InHole => [(0, {expanded: false, len: C.length(ts)})]
      };
    let inner_holes = {
      let root = Ts.root(ts);
      let tile = root_tile(root);
      let tile_holes =
        Sort_specific.inner_err_holes(tile)
        |> List.combine(C.children_offsets(tile))
        |> List.map(((offset, err_holes)) =>
             List.map(PairUtil.map_fst((+)(offset)), err_holes)
           )
        |> List.flatten;
      switch (Ts.root(ts)) {
      | Operand(_) => tile_holes
      | PreOp((_, r)) =>
        let r_holes = shift(C.length_of_tile(tile), err_holes(r));
        tile_holes @ r_holes;
      | PostOp((l, _)) =>
        let l_holes = err_holes(l);
        let tile_holes = shift(C.length(l), tile_holes);
        l_holes @ tile_holes;
      | BinOp((l, _, r)) =>
        let l_holes = err_holes(l);
        let tile_holes = shift(C.length(l), tile_holes);
        let r_holes =
          shift(
            C.length(l) + space + C.length_of_tile(tile),
            err_holes(r),
          );
        l_holes @ tile_holes @ r_holes;
      };
    };
    outer_hole @ inner_holes;
  };

  let rec err_holes_z = ((steps, j): ZPath.t, ts: T.s) => {
    let outer_hole: list((int, Decoration.ErrHole.profile)) =
      switch (Sort_specific.get_hole_status(ts)) {
      | NotInHole => []
      | InHole => [(0, {expanded: true, len: C.length(ts)})]
      };
    let inner_holes = {
      let root = Ts.root(ts);
      let tile = root_tile(root);
      let position = children_holes =>
        children_holes
        |> List.combine(C.children_offsets(tile))
        |> List.map(((offset, err_holes)) =>
             List.map(PairUtil.map_fst((+)(offset)), err_holes)
           )
        |> List.flatten;
      switch (steps) {
      | [] =>
        let tile_holes = position(Sort_specific.inner_err_holes(tile));
        switch (root) {
        | Operand(_) => tile_holes
        | PreOp((_, r)) =>
          let err_holes = j == 0 ? err_holes : err_holes_z(([], j - 1));
          tile_holes @ shift(C.length_of_tile(tile), err_holes(r));
        | PostOp((l, _)) =>
          let err_holes =
            j == List.length(l) ? err_holes : err_holes_z(([], j));
          err_holes(l) @ shift(C.length(l), tile_holes);
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
          @ shift(C.length(l), tile_holes)
          @ shift(
              C.length(l) + space + C.length_of_tile(tile),
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
              ? Sort_specific.inner_err_holes_z(
                  (child_step, (steps, j)),
                  tile,
                )
              : Sort_specific.inner_err_holes(tile),
          );
        switch (root) {
        | Operand(_) => tile_holes
        | PreOp((_, r)) =>
          tile_holes
          @ shift(
              C.length_of_tile(tile),
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
          @ shift(C.length(l), tile_holes)
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
          @ shift(C.length(l), tile_holes)
          @ shift(
              C.length(l) + space + C.length_of_tile(tile),
              err_holes_r(r),
            );
        };
      };
    };
    outer_hole @ inner_holes;
  };
};

module type TYP = COMMON with module T := HTyp.T;
module rec Typ: TYP = {
  module Sort_specific = {
    open HTyp.T;

    let length_of_tile: t => int =
      Tile.get(
        fun
        | OperandHole => 1
        | Num => 3
        | Paren(body) => 1 + space + Typ.length(body) + space + 1,
        () => raise(Void_PreOp),
        () => raise(Void_PostOp),
        fun
        | OperatorHole
        | Arrow => 1,
      );

    let offset_tile = ((child_step, path), tile) => {
      let `Typ(ty, unzipped) =
        ZPath.Typ.unzip_tile(child_step, tile, ZTyp.mk());
      switch (Option.get(unzipped)) {
      | Operand(ParenZ_body(_)) => 1 + space + Typ.offset(path, ty)
      | PreOp () => raise(ZTyp.Void_ZPreOp)
      | PostOp(AnnZ_ann(_)) => raise(ZPath.Unzip_rezip_changes_sort)
      | BinOp () => raise(ZTyp.Void_ZBinOp)
      };
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
           | Num => []
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
           | Paren(_) => [],
           () => raise(HTyp.T.Void_PreOp),
           () => raise(HTyp.T.Void_PostOp),
           fun
           | Arrow
           | OperatorHole => [],
         )
      |> List.combine(Typ.children_offsets(~filter=`Closed, tile));

    let inner_empty_holes_of_tile =
      Tile.get(
        fun
        | OperandHole
        | Num => []
        | Paren(body) => [Typ.empty_holes(body)],
        () => raise(Void_PreOp),
        () => raise(Void_PostOp),
        fun
        | OperatorHole
        | Arrow => [],
      );
  };
  include Common(HTyp.T, HTyp.Inner, ZTyp, ZPath.Typ, Sort_specific);
};

module type PAT = {
  include COMMON with module T := HPat.T;
  include ERR_HOLE with module T := HPat.T;
};
module rec Pat: PAT = {
  module Sort_specific = {
    open HPat.T;

    let length_of_tile: t => int =
      Tile.get(
        fun
        | OperandHole => 1
        | Var(x) => String.length(x)
        | Paren(body) => 1 + space + Pat.length(body) + space + 1,
        () => raise(Void_PreOp),
        fun
        | Ann(_, ann) => 1 + space + Typ.length(ann),
        fun
        | OperatorHole => 1,
      );

    let offset_tile = ((child_step, path), tile) =>
      switch (ZPath.Pat.unzip_tile(child_step, tile, ZPat.mk())) {
      | `Typ(ty, unzipped) =>
        switch (Option.get(unzipped)) {
        | Operand(ParenZ_body(_)) => raise(ZPath.Unzip_rezip_changes_sort)
        | PreOp () => raise(ZTyp.Void_ZPreOp)
        | PostOp(AnnZ_ann(_)) => 1 + space + Typ.offset(path, ty)
        | BinOp () => raise(ZTyp.Void_ZBinOp)
        }
      | `Pat(p, unzipped) =>
        switch (Option.get(unzipped)) {
        | Operand(ParenZ_body(_)) => 1 + space + Pat.offset(path, p)
        | PreOp(LamZ_pat(_) | LetZ_pat(_)) =>
          raise(ZPath.Unzip_rezip_changes_sort)
        | PostOp () => raise(ZPat.Void_ZPostOp)
        | BinOp () => raise(ZPat.Void_ZBinOp)
        }
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

    let inner_empty_holes_of_tile =
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

    let get_hole_status = HPat.get_hole_status;

    let inner_err_holes =
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

    let inner_err_holes_z = ((child_step, path), tile) =>
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
  };
  include Common(HPat.T, HPat.Inner, ZPat, ZPath.Pat, Sort_specific);
  include ErrHole(HPat.T, Pat, Sort_specific);
};

module type EXP = {
  include COMMON with module T := HExp.T;
  include ERR_HOLE with module T := HExp.T;
};
module rec Exp: EXP = {
  module Sort_specific = {
    open HExp.T;

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

    let offset_tile = ((child_step, path), tile) =>
      switch (ZPath.Exp.unzip_tile(child_step, tile, ZExp.mk())) {
      | `Pat(p, unzipped) =>
        switch (Option.get(unzipped)) {
        | Operand(ParenZ_body(_)) => raise(ZPath.Unzip_rezip_changes_sort)
        | PreOp(LamZ_pat(_)) => 1 + space + Pat.offset(path, p)
        | PreOp(LetZ_pat(_)) => 3 + space + Pat.offset(path, p)
        | PostOp () => raise(ZPat.Void_ZPostOp)
        | BinOp () => raise(ZPat.Void_ZBinOp)
        }
      | `Exp(e, unzipped) =>
        switch (Option.get(unzipped)) {
        | Operand(ParenZ_body(_))
        | PostOp(ApZ_arg(_)) => 1 + space + Exp.offset(path, e)
        | PreOp(LetZ_def(p, _)) =>
          3 + space + Pat.length(p) + space + 1 + space + Exp.offset(path, e)
        | BinOp () => raise(ZExp.Void_ZBinOp)
        }
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

    let inner_empty_holes_of_tile =
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

    let get_hole_status = HExp.get_hole_status;

    let inner_err_holes =
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

    let inner_err_holes_z = ((child_step, path), tile) =>
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
  };
  include Common(HExp.T, HExp.Inner, ZExp, ZPath.Exp, Sort_specific);
  include ErrHole(HExp.T, Exp, Sort_specific);
};
