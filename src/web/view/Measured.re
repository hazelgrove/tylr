open Util;
open Core;

// TODO unify
let space = 1;

module type COMMON = {
  module T: Tile.S;

  let length_of_tile: T.t => int;
  let length: T.s => int;
  let offset: (ZPath.t, T.s) => int;
  let empty_holes: T.s => list(int);

  let profile_of_tile: T.t => Decoration.Tile.profile;
};
module Common =
       (
         T: Tile.S,
         Sort_specific: {
           let length_of_tile: T.t => int;
           let offset_tile: ((ZPath.child_step, ZPath.t), T.t) => int;
           let is_operand_hole: T.operand => bool;
           let is_operator_hole: T.binop => bool;
           let open_children_of_tile: T.t => list((int, int));
           let closed_children_of_tile: T.t => list((int, int));
           let empty_holes_of_tile: T.t => list(int);
         },
       ) => {
  let length = (ts: T.s): int =>
    ts
    |> List.map(Sort_specific.length_of_tile)
    |> List.map((+)(1))
    |> List.fold_left((+), -1);

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
           (start, tile) => {
             let origins =
               Sort_specific.empty_holes_of_tile(tile)
               |> List.map((+)(start));
             (start + Sort_specific.length_of_tile(tile) + space, origins);
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
      len: Sort_specific.length_of_tile(t),
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
             T.s => list((int, Decoration.ErrHole.profile));
           let inner_err_holes_z:
             (ZPath.t, T.s) => list((int, Decoration.ErrHole.profile));
         },
       ) => {
  let err_holes = (ts: T.s) => {
    let outer_hole: list((int, Decoration.ErrHole.profile)) =
      switch (Sort_specific.get_hole_status(ts)) {
      | NotInHole => []
      | InHole => [(0, {expanded: false, len: C.length(ts)})]
      };
    let inner_holes = Sort_specific.inner_err_holes(ts);
    outer_hole @ inner_holes;
  };

  let err_holes_z = (path: ZPath.t, ts: T.s) => {
    let outer_hole: list((int, Decoration.ErrHole.profile)) =
      switch (Sort_specific.get_hole_status(ts)) {
      | NotInHole => []
      | InHole => [(0, {expanded: true, len: C.length(ts)})]
      };
    let inner_holes = Sort_specific.inner_err_holes_z(path, ts);
    outer_hole @ inner_holes;
  };
};

module type TYP = COMMON with module T := HTyp.Tile;
module rec Typ: TYP = {
  module Sort_specific = {
    open HTyp.Tile;

    let length_of_tile: t => int =
      Tile.map(
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
    let open_children_of_tile =
      Tile.map(
        fun
        | OperandHole
        | Num => []
        | Paren(body) => [(1 + space, Typ.length(body))],
        () => raise(HTyp.Tile.Void_PreOp),
        () => raise(HTyp.Tile.Void_PostOp),
        fun
        | Arrow
        | OperatorHole => [],
      );
    let closed_children_of_tile =
      Tile.map(
        fun
        | OperandHole
        | Num
        | Paren(_) => [],
        () => raise(HTyp.Tile.Void_PreOp),
        () => raise(HTyp.Tile.Void_PostOp),
        fun
        | Arrow
        | OperatorHole => [],
      );

    let empty_holes_of_tile = {
      let shift = n => List.map((+)(n + space));
      Tile.map(
        fun
        | OperandHole => [0]
        | Num => []
        | Paren(body) => shift(1, Typ.empty_holes(body)),
        () => raise(Void_PreOp),
        () => raise(Void_PostOp),
        fun
        | OperatorHole => [0]
        | Arrow => [],
      );
    };
  };
  include Common(HTyp.Tile, Sort_specific);
  let length_of_tile = Sort_specific.length_of_tile;
};

module type PAT = {
  include COMMON with module T := HPat.Tile;
  include ERR_HOLE with module T := HPat.Tile;
};
module rec Pat: PAT = {
  module Sort_specific = {
    open HPat.Tile;

    let length_of_tile: t => int =
      Tile.map(
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
        | PreOp(LamZ_pat(_)) => raise(ZPath.Unzip_rezip_changes_sort)
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
    let open_children_of_tile =
      Tile.map(
        fun
        | OperandHole
        | Var(_) => []
        | Paren(body) => [(1 + space, Pat.length(body))],
        () => raise(HPat.Tile.Void_PreOp),
        fun
        | Ann(_) => [],
        fun
        | OperatorHole => [],
      );
    let closed_children_of_tile =
      Tile.map(
        fun
        | OperandHole
        | Var(_)
        | Paren(_) => [],
        () => raise(HPat.Tile.Void_PreOp),
        fun
        | Ann(_, ann) => [(1 + space, Typ.length(ann))],
        fun
        | OperatorHole => [],
      );

    let empty_holes_of_tile = {
      let shift = n => List.map((+)(n + space));
      Tile.map(
        fun
        | OperandHole => [0]
        | Var(_) => []
        | Paren(body) => shift(1, Pat.empty_holes(body)),
        () => raise(Void_PreOp),
        fun
        | Ann(_, ty) => shift(1, Typ.empty_holes(ty)),
        fun
        | OperatorHole => [0],
      );
    };

    let get_hole_status = HPat.get_hole_status;
    let inner_err_holes = (p: HPat.t) => {
      let shift = n => List.map(PairUtil.map_fst((+)(n + space)));
      switch (HPat.root(p)) {
      | Operand(OperandHole | Var(_)) => []
      | Operand(Paren(body)) => shift(1, Pat.err_holes(body))
      | PreOp(((), _)) => raise(HPat.Tile.Void_PreOp)
      | PostOp((subj, Ann(_))) => Pat.err_holes(subj)
      | BinOp((l, OperatorHole as binop, r)) =>
        let l_holes = Pat.err_holes(l);
        let r_holes =
          shift(
            Pat.length(l) + space + Pat.length_of_tile(BinOp(binop)),
            Pat.err_holes(r),
          );
        l_holes @ r_holes;
      };
    };
    let inner_err_holes_z = ((steps, j): ZPath.t, p: HPat.t) => {
      let shift = n => List.map(PairUtil.map_fst((+)(n + space)));
      switch (steps) {
      | [] =>
        switch (HPat.root(p)) {
        | Operand(OperandHole | Var(_)) => []
        | Operand(Paren(body)) => shift(1, Pat.err_holes(body))
        | PreOp(((), _)) => raise(HPat.Tile.Void_PreOp)
        | PostOp((subj, Ann(_))) =>
          j == List.length(subj)
            ? Pat.err_holes(subj) : Pat.err_holes_z(([], j), subj)
        | BinOp((l, OperatorHole as binop, r)) =>
          let n = List.length(l);
          let (err_holes_l, err_holes_r) =
            if (j < n) {
              (Pat.err_holes_z(([], j)), Pat.err_holes);
            } else if (j > n) {
              (Pat.err_holes, Pat.err_holes_z(([], j - (n + 1))));
            } else {
              (Pat.err_holes, Pat.err_holes);
            };
          err_holes_l(l)
          @ shift(
              Pat.length(l) + space + Pat.length_of_tile(BinOp(binop)),
              err_holes_r(r),
            );
        }
      | [(tile_step, child_step) as two_step, ...steps] =>
        switch (HPat.root(p)) {
        | Operand(OperandHole | Var(_)) => raise(ZPath.Out_of_sync)
        | Operand(Paren(body)) =>
          shift(1, Pat.err_holes_z((steps, j), body))
        | PreOp(((), _)) => raise(HPat.Tile.Void_PreOp)
        | PostOp((subj, Ann(_))) =>
          let in_postop = tile_step == List.length(subj);
          in_postop
            ? Pat.err_holes(subj)
            : Pat.err_holes_z(([two_step, ...steps], j), subj);
        | BinOp((l, OperatorHole as binop, r)) =>
          let n = List.length(l);
          let (err_holes_l, err_holes_r) =
            if (tile_step < n) {
              (Pat.err_holes_z(([two_step, ...steps], j)), Pat.err_holes);
            } else if (tile_step > n) {
              (
                Pat.err_holes,
                Pat.err_holes_z((
                  [(tile_step - (n + 1), child_step), ...steps],
                  j,
                )),
              );
            } else {
              raise(ZPath.Out_of_sync);
            };
          err_holes_l(l)
          @ shift(
              Pat.length(l) + space + Pat.length_of_tile(BinOp(binop)),
              err_holes_r(r),
            );
        }
      };
    };
  };
  include Common(HPat.Tile, Sort_specific);
  include ErrHole(HPat.Tile, Pat, Sort_specific);
  let length_of_tile = Sort_specific.length_of_tile;
};

module type EXP = {
  include COMMON with module T := HExp.Tile;
  include ERR_HOLE with module T := HExp.Tile;
};
module rec Exp: EXP = {
  module Sort_specific = {
    open HExp.Tile;

    let length_of_tile =
      Tile.map(
        fun
        | OperandHole => 1
        | Var(_, x) => String.length(x)
        | Num(_, n) => String.length(string_of_int(n))
        | Paren(body) => 1 + space + Exp.length(body) + space + 1,
        fun
        | Lam(_, p) => 1 + space + Pat.length(p) + space + 1,
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
        | PostOp () => raise(ZPat.Void_ZPostOp)
        | BinOp () => raise(ZPat.Void_ZBinOp)
        }
      | `Exp(e, unzipped) =>
        switch (Option.get(unzipped)) {
        | Operand(ParenZ_body(_))
        | PostOp(ApZ_arg(_)) => 1 + space + Exp.offset(path, e)
        | PreOp () => raise(ZExp.Void_ZPreOp)
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
    let open_children_of_tile =
      Tile.map(
        fun
        | OperandHole
        | Var(_)
        | Num(_) => []
        | Paren(body) => [(1 + space, Exp.length(body))],
        fun
        | Lam(_) => [],
        fun
        | Ap(_, arg) => [(1 + space, Exp.length(arg))],
        fun
        | Plus(_)
        | OperatorHole => [],
      );
    let closed_children_of_tile =
      Tile.map(
        fun
        | OperandHole
        | Var(_)
        | Num(_)
        | Paren(_) => [],
        fun
        | Lam(_, p) => [(1 + space, Pat.length(p))],
        fun
        | Ap(_) => [],
        fun
        | Plus(_)
        | OperatorHole => [],
      );

    let empty_holes_of_tile = {
      let shift = n => List.map((+)(n + space));
      Tile.map(
        fun
        | OperandHole => [0]
        | Num(_)
        | Var(_) => []
        | Paren(body) => shift(1, Exp.empty_holes(body)),
        fun
        | Lam(_, p) => shift(1, Pat.empty_holes(p)),
        fun
        | Ap(_, arg) => shift(1, Exp.empty_holes(arg)),
        fun
        | OperatorHole => [0]
        | Plus(_) => [],
      );
    };

    let get_hole_status = HExp.get_hole_status;

    let inner_err_holes = (e: HExp.t) => {
      let shift = n => List.map(PairUtil.map_fst((+)(n + space)));
      switch (HExp.root(e)) {
      | Operand(OperandHole | Num(_) | Var(_)) => []
      | Operand(Paren(body)) => shift(1, Exp.err_holes(body))
      | PreOp((Lam(_, p) as preop, body)) =>
        let pat_holes = shift(1, Pat.err_holes(p));
        let body_holes =
          shift(Exp.length_of_tile(PreOp(preop)), Exp.err_holes(body));
        pat_holes @ body_holes;
      | PostOp((fn, Ap(_, arg))) =>
        let fn_holes = Exp.err_holes(fn);
        let arg_holes =
          shift(Exp.length(fn) + space + 1, Exp.err_holes(arg));
        fn_holes @ arg_holes;
      | BinOp((l, (OperatorHole | Plus(_)) as binop, r)) =>
        let l_holes = Exp.err_holes(l);
        let r_holes =
          shift(
            Exp.length(l) + space + Exp.length_of_tile(BinOp(binop)),
            Exp.err_holes(r),
          );
        l_holes @ r_holes;
      };
    };
    let inner_err_holes_z = ((steps, j): ZPath.t, e: HExp.t) => {
      let shift = n => List.map(PairUtil.map_fst((+)(n + space)));
      switch (steps) {
      | [] =>
        switch (HExp.root(e)) {
        | Operand(OperandHole | Num(_) | Var(_)) => []
        | Operand(Paren(body)) => shift(1, Exp.err_holes(body))
        | PreOp((Lam(_, p) as preop, body)) =>
          let p_holes = shift(1, Pat.err_holes(p));
          let body_holes = {
            let err_holes =
              j == 0 ? Exp.err_holes : Exp.err_holes_z(([], j - 1));
            shift(Exp.length_of_tile(PreOp(preop)), err_holes(body));
          };
          p_holes @ body_holes;
        | PostOp((fn, Ap(_, arg))) =>
          let fn_holes =
            j == List.length(fn)
              ? Exp.err_holes(fn) : Exp.err_holes_z(([], j), fn);
          let arg_holes = shift(Exp.length(fn), Exp.err_holes(arg));
          fn_holes @ arg_holes;
        | BinOp((l, (OperatorHole | Plus(_)) as binop, r)) =>
          let n = List.length(l);
          let (err_holes_l, err_holes_r) =
            if (j < n) {
              (Exp.err_holes_z(([], j)), Exp.err_holes);
            } else if (j > n) {
              (Exp.err_holes, Exp.err_holes_z(([], j - (n + 1))));
            } else {
              (Exp.err_holes, Exp.err_holes);
            };
          err_holes_l(l)
          @ shift(
              Exp.length(l) + space + Exp.length_of_tile(BinOp(binop)),
              err_holes_r(r),
            );
        }
      | [(tile_step, child_step) as two_step, ...steps] =>
        switch (HExp.root(e)) {
        | Operand(OperandHole | Num(_) | Var(_)) => raise(ZPath.Out_of_sync)
        | Operand(Paren(body)) =>
          shift(1, Exp.err_holes_z((steps, j), body))
        | PreOp((Lam(_, p) as preop, body)) =>
          let in_preop = tile_step == 0;
          let p_holes = {
            let err_holes =
              in_preop ? Pat.err_holes_z((steps, j)) : Pat.err_holes;
            shift(1, err_holes(p));
          };
          let body_holes = {
            let err_holes =
              in_preop
                ? Exp.err_holes
                : Exp.err_holes_z((
                    [(tile_step - 1, child_step), ...steps],
                    j,
                  ));
            shift(Exp.length_of_tile(PreOp(preop)), err_holes(body));
          };
          p_holes @ body_holes;
        | PostOp((fn, Ap(_, arg))) =>
          let in_postop = tile_step == List.length(fn);
          let fn_holes =
            in_postop
              ? Exp.err_holes(fn)
              : Exp.err_holes_z(([two_step, ...steps], j), fn);
          let arg_holes = {
            let err_holes =
              in_postop ? Exp.err_holes_z((steps, j)) : Exp.err_holes;
            shift(Exp.length(fn), err_holes(arg));
          };
          fn_holes @ arg_holes;
        | BinOp((l, (OperatorHole | Plus(_)) as binop, r)) =>
          let n = List.length(l);
          let (err_holes_l, err_holes_r) =
            if (tile_step < n) {
              (Exp.err_holes_z(([two_step, ...steps], j)), Exp.err_holes);
            } else if (tile_step > n) {
              (
                Exp.err_holes,
                Exp.err_holes_z((
                  [(tile_step - (n + 1), child_step), ...steps],
                  j,
                )),
              );
            } else {
              raise(ZPath.Out_of_sync);
            };
          err_holes_l(l)
          @ shift(
              Exp.length(l) + space + Exp.length_of_tile(BinOp(binop)),
              err_holes_r(r),
            );
        }
      };
    };
  };
  include Common(HExp.Tile, Sort_specific);
  include ErrHole(HExp.Tile, Exp, Sort_specific);
  let length_of_tile = Sort_specific.length_of_tile;
};
