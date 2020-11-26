open Sexplib.Std;
open Util;
open OptUtil.Syntax;

[@deriving sexp]
type tile_step = int;
[@deriving sexp]
type child_step = int;
[@deriving sexp]
type caret_step = int;
[@deriving sexp]
type two_step = (tile_step, child_step);
[@deriving sexp]
type t = (list(two_step), caret_step);

exception Out_of_sync;

let cons = (two_step, (steps, j)) => ([two_step, ...steps], j);

let compare_two_step = ((tile_step, child_step), (tile_step', child_step')) => {
  let c = Int.compare(tile_step, tile_step');
  c == 0 ? Int.compare(child_step, child_step') : c;
};

let rec compare = ((steps, j), (steps', j')) =>
  switch (steps, steps') {
  | ([], []) => Int.compare(j, j')
  | ([], [(tile_step, _), ..._]) => j <= tile_step ? (-1) : 1
  | ([(tile_step, _), ..._], []) => j' <= tile_step ? 1 : (-1)
  | ([two_step, ...steps], [two_step', ...steps']) =>
    let c = compare_two_step(two_step, two_step');
    c == 0 ? compare((steps, j), (steps', j')) : c;
  };

[@deriving sexp]
type anchored_selection = {
  anchor: t,
  focus: t,
};
[@deriving sexp]
type ordered_selection = (t, t);

let mk_ordered_selection =
    ({anchor, focus}: anchored_selection): (ordered_selection, Direction.t) =>
  compare(anchor, focus) <= 0
    ? ((anchor, focus), Right) : ((focus, anchor), Left);

let cons_anchored_selection = (two_step, {anchor, focus}) => {
  anchor: cons(two_step, anchor),
  focus: cons(two_step, focus),
};

let cons_ordered_selection = (two_step, (l, r)) => (
  cons(two_step, l),
  cons(two_step, r),
);

module Common =
       (
         T: Tile.S,
         I: {
           type t;
           let wrap: T.s => t;
           let unwrap: t => option(T.s);
         },
         P: {
           type ztile;
           type zipped;
           type unzipped;
           type zipper = (T.s, option(ztile));

           let sort: Sort.t;
           let sort_at: (t, T.s) => Sort.t;

           let zip_ztile: (T.s, ztile) => (two_step, zipped);
           let unzip: (two_step, zipper) => unzipped;
           let unzip_cis: (two_step, T.s) => option(ZList.t(T.s, T.t));

           let enter_from:
             (Direction.t, T.t) => option((child_step, caret_step));
           let move:
             (
               ~move: (
                        Direction.t as 'd,
                        t as 't,
                        (T.s, option(ztile)) as 'zipper
                      ) =>
                      option((t, option((two_step, zipped)))),
               'd,
               't,
               unzipped
             ) =>
             option((t, bool));

           let insert_tiles:
             (
               ~insert_tiles: (I.t as 'inner, t as 't, T.s as 'ts) =>
                              (option((ordered_selection, T.s)) as 'r),
               'inner,
               (two_step, 't),
               'ts
             ) =>
             'r;
           let remove_tiles:
             (
               ~remove_tiles: (ordered_selection as 'os, T.s as 'ts) =>
                              (option((I.t, t, T.s)) as 'r),
               (two_step, 'os),
               'ts
             ) =>
             'r;
           let restructure:
             (
               ~place_cursor: [ | `Selection | `Other] as 'pc,
               ~restructure: (
                               ~place_cursor: 'pc=?,
                               ordered_selection as 'os,
                               t as 't,
                               T.s as 'ts
                             ) =>
                             (option((t, T.s)) as 'r),
               (two_step, 'os, 't),
               'ts
             ) =>
             'r;
           let delete_selection:
             (
               ~delete_selection: (ordered_selection as 'os, T.s as 'ts) =>
                                  (option((t, T.s)) as 'r),
               (two_step, 'os),
               'ts
             ) =>
             'r;
         },
       ) => {
  module Ts = Tiles.Make(T);

  type did_it_zip = option((two_step, P.zipped));

  let rec move =
          (
            d: Direction.t,
            (steps, j): t,
            (ts, zrest) as zipper: (T.s, option(P.ztile)),
          )
          : option((t, did_it_zip)) => {
    switch (steps) {
    | [] =>
      let next_tile_step = d == Left ? j - 1 : j;
      switch (ListUtil.nth_opt(next_tile_step, ts), zrest) {
      | (None, None) => None
      | (Some(tile), _) =>
        let path =
          switch (P.enter_from(Direction.toggle(d), tile)) {
          | None => ([], d == Left ? j - 1 : j + 1)
          | Some((child_step, j)) => ([(next_tile_step, child_step)], j)
          };
        Some((path, None));
      | (_, Some(ztile)) =>
        let ((tile_step, _) as two_step, zipped) = P.zip_ztile(ts, ztile);
        let path = ([], d == Left ? tile_step : tile_step + 1);
        Some((path, Some((two_step, zipped))));
      };
    | [two_step, ...steps] =>
      let unzipped = P.unzip(two_step, zipper);
      let+ (moved, did_it_zip) = P.move(~move, d, (steps, j), unzipped);
      let path = did_it_zip ? moved : cons(two_step, moved);
      (path, None);
    };
  };

  let select =
      (d: Direction.t, anchor: t, zipper: P.zipper)
      : option((anchored_selection, did_it_zip)) => {
    let+ (next, did_it_zip) = move(d, anchor, zipper);
    switch (did_it_zip) {
    | None => ({anchor, focus: next}, did_it_zip)
    | Some((two_step, _)) =>
      let anchor = cons(two_step, anchor);
      ({anchor, focus: next}, did_it_zip);
    };
  };

  /* assumes l, r are maximally unzipped */
  let round_selection =
      ((l, r): ordered_selection, ts: T.s): ordered_selection => {
    let rec next_cis_path = (d: Direction.t, current: t): t =>
      if (P.sort_at(current, ts) == P.sort) {
        current;
      } else {
        let (next, _) = Option.get(move(d, current, (ts, None)));
        next_cis_path(d, next);
      };
    let l = next_cis_path(Left, l);
    let r = next_cis_path(Right, r);
    (l, r);
  };

  let rec insert_tiles =
          (tiles: I.t, (steps, j): t, ts: T.s)
          : option((ordered_selection, T.s)) =>
    switch (steps) {
    | [] =>
      let+ tiles = I.unwrap(tiles);
      let (prefix, suffix) = ListUtil.split_n(j, ts);
      let suffix_len = {
        let (_, suffix) = Ts.fix_empty_holes(prefix @ tiles, suffix);
        List.length(suffix);
      };
      let (prefix_len, ts) = {
        let (prefix, suffix) = Ts.fix_empty_holes(prefix, tiles @ suffix);
        (List.length(prefix), prefix @ suffix);
      };
      let inserted = (
        ([], prefix_len),
        ([], List.length(ts) - suffix_len),
      );
      (inserted, ts);
    | [two_step, ...steps] =>
      P.insert_tiles(~insert_tiles, tiles, (two_step, (steps, j)), ts)
    };

  let rec remove_tiles =
          ((l, r): ordered_selection, ts: T.s): option((I.t, t, T.s)) =>
    switch (l, r) {
    | (([], j_l), ([], j_r)) =>
      let (prefix, removed, suffix) = ListUtil.split_sublist(j_l, j_r, ts);
      let (prefix, suffix) = Ts.fix_empty_holes(prefix, suffix);
      Some((I.wrap(removed), ([], List.length(prefix)), prefix @ suffix));
    | (([], _), ([_, ..._], _))
    | (([_, ..._], _), ([], _)) => None
    | (([two_step_l, ...steps_l], j_l), ([two_step_r, ...steps_r], j_r)) =>
      if (two_step_l != two_step_r) {
        None;
      } else {
        let selection = ((steps_l, j_l), (steps_r, j_r));
        P.remove_tiles(~remove_tiles, (two_step_l, selection), ts);
      }
    };

  let rec restructure =
          (
            ~place_cursor: [ | `Selection | `Other]=`Selection,
            (l, r): ordered_selection,
            target: t,
            ts: T.s,
          )
          : option((t, T.s)) =>
    if (compare(l, target) < 0 && compare(target, r) < 0) {
      None;
    } else {
      let cursor_path = ((inserted_l, inserted_r), removed_path) =>
        switch (place_cursor) {
        | `Selection => inserted_l
        | `Other => compare(target, l) <= 0 ? inserted_r : removed_path
        };
      // target before l or r before target
      let (steps_l, j_l) = l;
      let (steps_r, j_r) = r;
      let (steps_target, j_target) = target;
      switch (steps_l, steps_r, steps_target) {
      | ([_, ..._], [], [])
      | ([], [_, ..._], [_, ..._]) =>
        restructure(~place_cursor=`Other, (r, target), l, ts)
      | ([], [_, ..._], [])
      | ([_, ..._], [], [_, ..._]) =>
        restructure(~place_cursor=`Other, (target, l), r, ts)
      | ([], [], []) =>
        let (prefix, removed, suffix) = ListUtil.split_sublist(j_l, j_r, ts);
        let (prefix, suffix) =
          if (j_target <= j_l) {
            let (prefix_l, prefix_r) = ListUtil.split_n(j_target, prefix);
            switch (place_cursor) {
            | `Selection => (prefix_l, removed @ prefix_r @ suffix)
            | `Other => (prefix_l @ removed, prefix_r @ suffix)
            };
          } else {
            let (suffix_l, suffix_r) =
              ListUtil.split_n(
                j_target - (List.length(prefix) + List.length(removed)),
                suffix,
              );
            switch (place_cursor) {
            | `Selection => (prefix @ suffix_l, removed @ suffix_r)
            | `Other => (prefix, suffix_l @ removed @ suffix_r)
            };
          };
        let (prefix, suffix) = Ts.fix_empty_holes(prefix, suffix);
        Some((([], List.length(prefix)), prefix @ suffix));
      | ([], [], [(tile_step, child_step), ...steps]) =>
        let (prefix, removed, suffix) = ListUtil.split_sublist(j_l, j_r, ts);
        let (fixed_prefix, fixed_suffix) =
          Ts.fix_empty_holes(prefix, suffix);
        let ts = fixed_prefix @ fixed_suffix;
        if (tile_step < j_l) {
          let+ ((inserted_l, inserted_r), ts) =
            insert_tiles(I.wrap(removed), target, ts);
          switch (place_cursor) {
          | `Selection => (inserted_l, ts)
          | `Other => (inserted_r, ts)
          };
        } else {
          // tile_step >= j_r
          let insert_path = {
            let tile_step =
              tile_step
              - List.length(removed)
              + (List.length(fixed_prefix) - List.length(prefix))
              + (List.length(fixed_suffix) - List.length(suffix));
            ([(tile_step, child_step), ...steps], j_target);
          };
          let+ ((inserted_l, _), ts) =
            insert_tiles(I.wrap(removed), insert_path, ts);
          switch (place_cursor) {
          | `Selection => (inserted_l, ts)
          | `Other => (([], List.length(fixed_prefix)), ts)
          };
        };
      | ([two_step_l, ..._], [two_step_r, ..._], []) =>
        if (two_step_l != two_step_r) {
          None;
        } else {
          let* (removed_tiles, removed_path, ts) = remove_tiles((l, r), ts);
          let+ (inserted_selection, ts) =
            insert_tiles(removed_tiles, target, ts);
          (cursor_path(inserted_selection, removed_path), ts);
        }
      | (
          [two_step_l, ...steps_l],
          [two_step_r, ...steps_r],
          [two_step_t, ...steps_t],
        ) =>
        if (two_step_l == two_step_r && two_step_r == two_step_t) {
          let l = (steps_l, j_l);
          let r = (steps_r, j_r);
          let target = (steps_t, j_target);
          P.restructure(
            ~place_cursor,
            ~restructure,
            (two_step_l, (l, r), target),
            ts,
          );
        } else if (two_step_l == two_step_r) {
          let* (removed_tiles, removed_path, ts) = remove_tiles((l, r), ts);
          let+ (inserted_selection, ts) =
            insert_tiles(removed_tiles, target, ts);
          (cursor_path(inserted_selection, removed_path), ts);
        } else if (two_step_t == two_step_l) {
          restructure(~place_cursor=`Other, (r, target), l, ts);
        } else if (two_step_r == two_step_t) {
          restructure(~place_cursor=`Other, (target, l), r, ts);
        } else {
          None;
        }
      };
    };

  let rec delete_selection =
          ((l, r): ordered_selection, ts: T.s): option((t, T.s)) =>
    switch (l, r) {
    | (([], j_l), ([], j_r)) =>
      let (prefix, _, suffix) = ListUtil.split_sublist(j_l, j_r, ts);
      Some((l, prefix @ suffix));
    | (([], j_l), ([two_step_r, ...steps_r], j_r)) =>
      let* {prefix, z: ts, suffix} = P.unzip_cis(two_step_r, ts);
      let+ (path, ts) = delete_selection((([], 0), (steps_r, j_r)), ts);
      let (prefix, _deleted) = ListUtil.split_n(j_l, prefix);
      let path = {
        let n = List.length(prefix);
        switch (path) {
        | ([], j) => ([], j + n)
        | ([(tile_step, child_step), ...steps], j) => (
            [(tile_step + n, child_step), ...steps],
            j,
          )
        };
      };
      (path, prefix @ ts @ suffix);
    | (([(tile_step, _) as two_step_l, ...steps_l], j_l), ([], j_r)) =>
      let* {prefix, z: ts, suffix} = P.unzip_cis(two_step_l, ts);
      let+ (path, ts) =
        delete_selection(((steps_l, j_l), ([], List.length(ts))), ts);
      let (_deleted, suffix) =
        ListUtil.split_n(j_r - (tile_step + 1), suffix);
      let path = {
        let n = List.length(prefix) + List.length(ts);
        switch (path) {
        | ([], j) => ([], j + n)
        | ([(tile_step, child_step), ...steps], j) => (
            [(tile_step + n, child_step), ...steps],
            j,
          )
        };
      };
      (path, prefix @ ts @ suffix);
    | (([two_step_l, ...steps_l], j_l), ([two_step_r, ...steps_r], j_r)) =>
      if (two_step_l == two_step_r) {
        let selection = ((steps_l, j_l), (steps_r, j_r));
        P.delete_selection(~delete_selection, (two_step_l, selection), ts);
      } else {
        let* {z: ts_l, _} = P.unzip_cis(two_step_l, ts);
        let* {z: ts_r, _} = P.unzip_cis(two_step_r, ts);
        let+ (path, ts_l) =
          delete_selection(
            ((steps_l, j_l), ([], List.length(ts_l))),
            ts_l,
          )
        and+ (_, ts_r) =
          delete_selection((([], 0), (steps_r, j_r)), ts_r);
        let (prefix, _, suffix) =
          ListUtil.split_sublist(fst(two_step_l), fst(two_step_r) + 1, ts);
        let path = {
          let n = List.length(prefix);
          switch (path) {
          | ([], j) => ([], j + n)
          | ([(tile_step, child_step), ...steps], j) => (
              [(tile_step + n, child_step), ...steps],
              j,
            )
          };
        };
        (path, prefix @ ts_l @ ts_r @ suffix);
      }
    };
};

module rec Typ: {
  type zipped = [ | `Typ(ZTyp.zipper) | `Pat(ZPat.zipper)];
  type did_it_zip = option((two_step, zipped));

  let zip: (HTyp.t, ZTyp.t) => (tile_step, ZTyp.zipper);
  let zip_ztile: (HTyp.t, ZTyp.ztile) => (two_step, zipped);

  type unzipped = [ | `Typ(ZTyp.zipper)];

  let unzip_tile: (child_step, HTyp.Tile.t, ZTyp.t) => unzipped;
  let unzip: (two_step, ZTyp.zipper) => unzipped;

  let sort_at: (t, HTyp.t) => Sort.t;

  /**
   * `move(d, zipper, path)` first attempts to returns the next path
   * from `path` in direction `d` within the focused term of `zipper`.
   * If no such path exists (i.e. the cursor is at one of the ends of
   * the focused term), then it attempts to zip `zipper` and try once
   * more.
   */
  let move: (Direction.t, t, ZTyp.zipper) => option((t, did_it_zip));

  let select:
    (Direction.t, t, ZTyp.zipper) => option((anchored_selection, did_it_zip));

  let delete_selection: (ordered_selection, HTyp.t) => option((t, HTyp.t));

  let round_selection: (ordered_selection, HTyp.t) => ordered_selection;

  let remove_tiles:
    (ordered_selection, HTyp.t) => option((HTyp.Inner.t, t, HTyp.t));
  let insert_tiles:
    (HTyp.Inner.t, t, HTyp.t) => option((ordered_selection, HTyp.t));
  let restructure:
    (~place_cursor: [ | `Selection | `Other]=?, (t, t), t, HTyp.t) =>
    option((t, HTyp.t));
} = {
  type zipped = [ | `Typ(ZTyp.zipper) | `Pat(ZPat.zipper)];

  let zip = (ty: HTyp.t, zty: ZTyp.t): (tile_step, ZTyp.zipper) => (
    List.length(zty.prefix),
    (zty.prefix @ ty @ zty.suffix, zty.z),
  );
  let zip_ztile = (subject: HTyp.t, ztile: ZTyp.ztile): (two_step, zipped) =>
    switch (ztile) {
    | Operand(ParenZ_body(zty)) =>
      let (tile_step, (ty, zrest)) =
        zip([Tile.Operand(HTyp.Tile.Paren(subject))], zty);
      ((tile_step, 0), `Typ((ty, zrest)));
    | PreOp(_) => raise(ZTyp.Void_ZPreOp)
    | PostOp(AnnZ_ann(status, zp)) =>
      let (tile_step, (p, zrest)) =
        Pat.zip([Tile.PostOp(HPat.Tile.Ann(status, subject))], zp);
      ((tile_step, 0), `Pat((p, zrest)));
    | BinOp(_) => raise(ZTyp.Void_ZBinOp)
    };

  type unzipped = [ | `Typ(ZTyp.zipper)];

  let unzip_tile = (r: child_step, tile: HTyp.Tile.t, zty: ZTyp.t): unzipped =>
    switch (tile) {
    | Operand(Paren(body)) when r == 0 =>
      `Typ((body, Some(Tile.Operand(ParenZ_body(zty)))))
    | _ => raise(Invalid_argument("ZPath.Typ.unzip_tile"))
    };
  let unzip =
      ((l, r): two_step, (ty: HTyp.t, zrest: option(ZTyp.ztile))): unzipped => {
    let (tile, ze) = {
      let (prefix, tile, suffix) = ListUtil.split_nth(l, ty);
      (tile, ZTyp.mk(~prefix, ~z=?zrest, ~suffix, ()));
    };
    unzip_tile(r, tile, ze);
  };

  let sort_at = (_, _) => Sort.Typ;

  module P = {
    type ztile = ZTyp.ztile;
    type zipper = ZTyp.zipper;

    type nonrec zipped = zipped;
    type nonrec unzipped = unzipped;

    let sort = Sort.Typ;
    let sort_at = sort_at;

    let zip_ztile = zip_ztile;
    let unzip = unzip;
    let unzip_cis = (two_step, ty) => {
      let `Typ(ty, unzipped) = unzip(two_step, (ty, None));
      switch (Option.get(unzipped)) {
      | Operand(ParenZ_body({prefix, suffix, _})) =>
        Some(ZList.{prefix, z: ty, suffix})
      | PreOp(_) => raise(ZTyp.Void_ZPreOp)
      | PostOp(AnnZ_ann(_)) =>
        failwith("rezipping unzipped result would change sorts")
      | BinOp(_) => raise(ZTyp.Void_ZBinOp)
      };
    };

    let enter_from =
        (d: Direction.t, tile: HTyp.Tile.t)
        : option((child_step, caret_step)) =>
      switch (tile) {
      | Operand(OperandHole | Num)
      | BinOp(OperatorHole | Arrow) => None
      | Operand(Paren(ty)) => Some((0, d == Left ? List.length(ty) : 0))
      | PreOp(_) => raise(HTyp.Tile.Void_PreOp)
      | PostOp(_) => raise(HTyp.Tile.Void_PostOp)
      };
    let move = (~move, d, path, `Typ(unzipped)) => {
      let+ (moved, did_it_zip) = move(d, path, unzipped);
      (moved, Option.is_some(did_it_zip));
    };

    let insert_tiles =
        (~insert_tiles, tiles: HTyp.Inner.t, (two_step, target), ty) => {
      let `Typ(ty, unzipped) = unzip(two_step, (ty, None));
      let+ (selection, inserted) = insert_tiles(tiles, target, ty);
      switch (Typ.zip_ztile(inserted, Option.get(unzipped))) {
      | (_, `Pat(_)) => failwith("unzipping and rezipping changed sorts")
      | (two_step, `Typ(rezipped, _)) => (
          cons_ordered_selection(two_step, selection),
          rezipped,
        )
      };
    };

    let remove_tiles =
        (~remove_tiles, (two_step, selection), ty)
        : option((HTyp.Inner.t, t, HTyp.t)) => {
      let `Typ(ty, zrest) = unzip(two_step, (ty, None));
      let+ (removed, removed_path, ty) = remove_tiles(selection, ty);
      switch (zip_ztile(ty, Option.get(zrest))) {
      | (_, `Pat(_)) => failwith("unzipping and rezipping changed sort")
      | (two_step, `Typ(ty, _none)) => (
          removed,
          cons(two_step, removed_path),
          ty,
        )
      };
    };

    let restructure =
        (
          ~place_cursor: [ | `Selection | `Other] as 'pc,
          ~restructure: (~place_cursor: 'pc=?, _, _, _) => _,
          (two_step, (l, r), target),
          ty: HTyp.t,
        )
        : option((t, HTyp.t)) => {
      let `Typ(ty, zrest) = unzip(two_step, (ty, None));
      let+ (path, ty) = restructure(~place_cursor, (l, r), target, ty);
      switch (zip_ztile(ty, Option.get(zrest))) {
      | (_, `Pat(_)) => failwith("unzipping and rezipping changed sort")
      | (_, `Typ(ty, _)) => (cons(two_step, path), ty)
      };
    };

    let delete_selection =
        (~delete_selection, (two_step, selection), ty): option((t, HTyp.t)) => {
      let `Typ(ty, unzipped) = unzip(two_step, (ty, None));
      let+ (path, ty) = delete_selection(selection, ty);
      switch (zip_ztile(ty, Option.get(unzipped))) {
      | (_, `Pat(_)) => failwith("unzipping and rezipping changed sort")
      | (_, `Typ(ty, _)) => (cons(two_step, path), ty)
      };
    };
  };
  include Common(HTyp.Tile, HTyp.Inner, P);
}
and Pat: {
  type zipped = [ | `Pat(ZPat.zipper) | `Exp(ZExp.zipper)];
  type did_it_zip = option((two_step, zipped));

  let zip: (HPat.t, ZPat.t) => (tile_step, ZPat.zipper);
  let zip_ztile: (HPat.t, ZPat.ztile) => (two_step, zipped);

  type unzipped = [ | `Pat(ZPat.zipper) | `Typ(ZTyp.zipper)];

  let unzip_tile: (child_step, HPat.Tile.t, ZPat.t) => unzipped;
  let unzip: (two_step, ZPat.zipper) => unzipped;

  let sort_at: (t, HPat.t) => Sort.t;

  let move: (Direction.t, t, ZPat.zipper) => option((t, did_it_zip));

  let select:
    (Direction.t, t, ZPat.zipper) => option((anchored_selection, did_it_zip));

  let delete_selection: (ordered_selection, HPat.t) => option((t, HPat.t));

  let round_selection: (ordered_selection, HPat.t) => ordered_selection;

  let remove_tiles:
    (ordered_selection, HPat.t) => option((HPat.Inner.t, t, HPat.t));
  let insert_tiles:
    (HPat.Inner.t, t, HPat.t) => option((ordered_selection, HPat.t));
  let restructure:
    (
      ~place_cursor: [ | `Selection | `Other]=?,
      ordered_selection,
      t,
      HPat.t
    ) =>
    option((t, HPat.t));
} = {
  type zipped = [ | `Pat(ZPat.zipper) | `Exp(ZExp.zipper)];

  let zip = (p: HPat.t, zp: ZPat.t): (tile_step, ZPat.zipper) => (
    List.length(zp.prefix),
    (zp.prefix @ p @ zp.suffix, zp.z),
  );
  let zip_ztile = (p: HPat.t, ztile: ZPat.ztile): (two_step, zipped) =>
    switch (ztile) {
    | Operand(ParenZ_body(zp)) =>
      let (tile_step, (p, zrest)) =
        zip([Tile.Operand(HPat.Tile.Paren(p))], zp);
      ((tile_step, 0), `Pat((p, zrest)));
    | PreOp(LamZ_pat(status, ze)) =>
      let (tile_step, (e, zrest)) =
        Exp.zip([Tile.PreOp(HExp.Tile.Lam(status, p))], ze);
      ((tile_step, 0), `Exp((e, zrest)));
    | PostOp(_) => raise(ZPat.Void_ZPostOp)
    | BinOp(_) => raise(ZPat.Void_ZBinOp)
    };

  type unzipped = [ | `Pat(ZPat.zipper) | `Typ(ZTyp.zipper)];

  let unzip_tile = (r: child_step, tile: HPat.Tile.t, zp: ZPat.t): unzipped =>
    switch (tile) {
    | Operand(Paren(body)) when r == 0 =>
      `Pat((body, Some(Tile.Operand(ZPat.ParenZ_body(zp)))))
    | PostOp(Ann(status, ann)) when r == 0 =>
      `Typ((ann, Some(Tile.PostOp(ZTyp.AnnZ_ann(status, zp)))))
    | _ => raise(Invalid_argument("ZPath.Pat.unzip_tile"))
    };
  let unzip =
      ((l, r): two_step, (p: HPat.t, zrest: option(ZPat.ztile))): unzipped => {
    let (tile, zp) = {
      let (prefix, tile, suffix) = ListUtil.split_nth(l, p);
      (tile, ZPat.mk(~prefix, ~z=?zrest, ~suffix, ()));
    };
    unzip_tile(r, tile, zp);
  };

  // TODO fix output type
  let rec sort_at = ((steps, j): t, p: HPat.t): Sort.t =>
    switch (steps) {
    | [] => Pat
    | [two_step, ...steps] =>
      let path = (steps, j);
      switch (unzip(two_step, (p, None))) {
      | `Typ(ty, _) => Typ.sort_at(path, ty)
      | `Pat(p, _) => sort_at(path, p)
      };
    };

  module P = {
    type ztile = ZPat.ztile;
    type zipper = ZPat.zipper;

    type nonrec zipped = zipped;
    type nonrec unzipped = unzipped;

    let sort = Sort.Pat;
    let sort_at = sort_at;

    let zip_ztile = zip_ztile;
    let unzip = unzip;
    let unzip_cis = (two_step, p) =>
      switch (unzip(two_step, (p, None))) {
      | `Typ(_) => None
      | `Pat(p, unzipped) =>
        switch (Option.get(unzipped)) {
        | Operand(ParenZ_body({prefix, suffix, _})) =>
          Some(ZList.{prefix, z: p, suffix})
        | PreOp(LamZ_pat(_)) => None
        | PostOp(_) => raise(ZPat.Void_ZPostOp)
        | BinOp(_) => raise(ZPat.Void_ZBinOp)
        }
      };

    let enter_from =
        (d: Direction.t, tile: HPat.Tile.t)
        : option((child_step, caret_step)) =>
      switch (tile) {
      | Operand(OperandHole | Var(_))
      | BinOp(OperatorHole) => None
      | Operand(Paren(p)) => Some((0, d == Left ? List.length(p) : 0))
      | PostOp(Ann(_, ty)) => Some((0, d == Left ? List.length(ty) : 0))
      | PreOp(_) => raise(HPat.Tile.Void_PreOp)
      };
    let move = (~move, d, path, unzipped) =>
      switch (unzipped) {
      | `Pat(unzipped) =>
        let+ (moved, did_it_zip) = move(d, path, unzipped);
        (moved, Option.is_some(did_it_zip));
      | `Typ(unzipped) =>
        let+ (moved, did_it_zip) = Typ.move(d, path, unzipped);
        (moved, Option.is_some(did_it_zip));
      };

    let insert_tiles =
        (~insert_tiles, tiles: HPat.Inner.t, (two_step, target), p) =>
      switch (unzip(two_step, (p, None))) {
      | `Typ(ty, unzipped) =>
        switch (tiles) {
        | Pat(_) => None
        | Other(tiles) =>
          let+ (selection, inserted) = Typ.insert_tiles(tiles, target, ty);
          switch (Typ.zip_ztile(inserted, Option.get(unzipped))) {
          | (_, `Typ(_)) => failwith("unzipping and rezipping changed sort")
          | (two_step, `Pat(rezipped, _)) => (
              cons_ordered_selection(two_step, selection),
              rezipped,
            )
          };
        }
      | `Pat(p, unzipped) =>
        let+ (selection, inserted) = insert_tiles(tiles, target, p);
        switch (zip_ztile(inserted, Option.get(unzipped))) {
        | (_, `Exp(_)) => failwith("unzipping and rezipping changed sort")
        | (_, `Pat(rezipped, _)) => (
            cons_ordered_selection(two_step, selection),
            rezipped,
          )
        };
      };

    let remove_tiles =
        (~remove_tiles, (two_step, selection), p)
        : option((HPat.Inner.t, t, HPat.t)) =>
      switch (unzip(two_step, (p, None))) {
      | `Pat(p, zrest) =>
        let+ (removed, removed_path, p) = remove_tiles(selection, p);
        switch (zip_ztile(p, Option.get(zrest))) {
        | (_, `Exp(_)) => failwith("unzipping and rezipping changed sort")
        | (two_step, `Pat(p, _)) => (
            removed,
            cons(two_step, removed_path),
            p,
          )
        };
      | `Typ(ty, zrest) =>
        let+ (removed, removed_path, ty) = Typ.remove_tiles(selection, ty);
        switch (Typ.zip_ztile(ty, Option.get(zrest))) {
        | (_, `Typ(_)) => failwith("unzipping and rezipping changed sort")
        | (_, `Pat(p, _none)) => (
            HPat.Inner.Other(removed),
            cons(two_step, removed_path),
            p,
          )
        };
      };

    let restructure =
        (
          ~place_cursor: [ | `Selection | `Other] as 'pc,
          ~restructure: (~place_cursor: 'pc=?, _, _, _) => _,
          (two_step, (l, r), target),
          p: HPat.t,
        )
        : option((t, HPat.t)) =>
      switch (unzip(two_step, (p, None))) {
      | `Pat(p, zrest) =>
        let+ (path, p) = restructure(~place_cursor, (l, r), target, p);
        switch (zip_ztile(p, Option.get(zrest))) {
        | (_, `Exp(_)) => failwith("unzipping and rezipping changed sort")
        | (_, `Pat(p, _none)) => (cons(two_step, path), p)
        };
      | `Typ(ty, zrest) =>
        let+ (path, ty) =
          Typ.restructure(~place_cursor, (l, r), target, ty);
        switch (Typ.zip_ztile(ty, Option.get(zrest))) {
        | (_, `Typ(_)) => failwith("unzipping and rezipping changed sort")
        | (_, `Pat(p, _none)) => (cons(two_step, path), p)
        };
      };

    let delete_selection =
        (~delete_selection, (two_step, selection), p): option((t, HPat.t)) =>
      switch (unzip(two_step, (p, None))) {
      | `Typ(ty, unzipped) =>
        let+ (path, ty) = Typ.delete_selection(selection, ty);
        switch (Typ.zip_ztile(ty, Option.get(unzipped))) {
        | (_, `Typ(_)) => failwith("unzipping and rezipping changed sort")
        | (_, `Pat(p, _)) => (cons(two_step, path), p)
        };
      | `Pat(p, unzipped) =>
        let+ (path, p) = delete_selection(selection, p);
        switch (zip_ztile(p, Option.get(unzipped))) {
        | (_, `Exp(_)) => failwith("unzipping and rezipping changed sort")
        | (_, `Pat(p, _)) => (cons(two_step, path), p)
        };
      };
  };
  include Common(HPat.Tile, HPat.Inner, P);
}
and Exp: {
  type zipped = [ | `Exp(ZExp.zipper)];
  type did_it_zip = option((two_step, zipped));

  let zip: (HExp.t, ZExp.t) => (tile_step, ZExp.zipper);
  let zip_ztile: (HExp.t, ZExp.ztile) => (two_step, zipped);

  type unzipped = [ | `Exp(ZExp.zipper) | `Pat(ZPat.zipper)];

  let unzip_tile: (child_step, HExp.Tile.t, ZExp.t) => unzipped;
  let unzip: (two_step, ZExp.zipper) => unzipped;

  let sort_at: (t, HExp.t) => Sort.t;

  let move: (Direction.t, t, ZExp.zipper) => option((t, did_it_zip));

  let select:
    (Direction.t, t, ZExp.zipper) => option((anchored_selection, did_it_zip));

  let delete_selection: (ordered_selection, HExp.t) => option((t, HExp.t));

  let round_selection: (ordered_selection, HExp.t) => ordered_selection;

  let remove_tiles:
    (ordered_selection, HExp.t) => option((HExp.Inner.t, t, HExp.t));
  let insert_tiles:
    (HExp.Inner.t, t, HExp.t) => option((ordered_selection, HExp.t));
  let restructure:
    (
      ~place_cursor: [ | `Selection | `Other]=?,
      ordered_selection,
      t,
      HExp.t
    ) =>
    option((t, HExp.t));
} = {
  type zipped = [ | `Exp(ZExp.zipper)];

  let zip = (e: HExp.t, ze: ZExp.t): (tile_step, ZExp.zipper) => (
    List.length(ze.prefix),
    (ze.prefix @ e @ ze.suffix, ze.z),
  );
  let zip_ztile = (e: HExp.t, ztile: ZExp.ztile): (two_step, zipped) =>
    switch (ztile) {
    | Operand(ParenZ_body(ze)) =>
      let (tile_step, (e, zrest)) =
        zip([Tile.Operand(HExp.Tile.Paren(e))], ze);
      ((tile_step, 0), `Exp((e, zrest)));
    | PreOp(_) => raise(ZExp.Void_ZPreOp)
    | PostOp(ApZ_arg(status, ze)) =>
      let (tile_step, (e, zrest)) =
        zip([Tile.PostOp(HExp.Tile.Ap(status, e))], ze);
      ((tile_step, 0), `Exp((e, zrest)));
    | BinOp(_) => raise(ZExp.Void_ZBinOp)
    };

  type unzipped = [ | `Exp(ZExp.zipper) | `Pat(ZPat.zipper)];

  let unzip_tile = (r: child_step, tile: HExp.Tile.t, ze: ZExp.t): unzipped =>
    switch (tile) {
    | Operand(Paren(body)) when r == 0 =>
      `Exp((body, Some(Tile.Operand(ParenZ_body(ze)))))
    | PreOp(Lam(status, p)) when r == 0 =>
      `Pat((p, Some(Tile.PreOp(ZPat.LamZ_pat(status, ze)))))
    | PostOp(Ap(status, arg)) when r == 0 =>
      `Exp((arg, Some(Tile.PostOp(ZExp.ApZ_arg(status, ze)))))
    | _ => raise(Invalid_argument("ZPath.Exp.unzip_tile"))
    };
  let unzip =
      ((l, r): two_step, (e: HExp.t, zrest: option(ZExp.ztile))): unzipped => {
    let (tile, ze) = {
      let (prefix, tile, suffix) = ListUtil.split_nth(l, e);
      (tile, ZExp.mk(~prefix, ~z=?zrest, ~suffix, ()));
    };
    unzip_tile(r, tile, ze);
  };

  let rec sort_at = ((steps, j): t, e: HExp.t): Sort.t =>
    switch (steps) {
    | [] => Exp
    | [two_step, ...steps] =>
      let path = (steps, j);
      switch (unzip(two_step, (e, None))) {
      | `Pat(p, _) => Pat.sort_at(path, p)
      | `Exp(e, _) => sort_at(path, e)
      };
    };

  module P = {
    type ztile = ZExp.ztile;
    type zipper = ZExp.zipper;

    type nonrec zipped = zipped;
    type nonrec unzipped = unzipped;

    let sort = Sort.Exp;
    let sort_at = sort_at;

    let zip_ztile = zip_ztile;
    let unzip = unzip;
    let unzip_cis = (two_step, e) =>
      switch (unzip(two_step, (e, None))) {
      | `Pat(_) => None
      | `Exp(e, unzipped) =>
        switch (Option.get(unzipped)) {
        | Operand(ParenZ_body({prefix, suffix, _}))
        | PostOp(ApZ_arg(_, {prefix, suffix, _})) =>
          Some(ZList.{prefix, z: e, suffix})
        | PreOp(_) => raise(ZExp.Void_ZPreOp)
        | BinOp(_) => raise(ZExp.Void_ZBinOp)
        }
      };

    let enter_from =
        (d: Direction.t, tile: HExp.Tile.t)
        : option((child_step, caret_step)) =>
      switch (tile) {
      | Operand(OperandHole | Num(_) | Var(_))
      | BinOp(Plus(_) | OperatorHole) => None
      | Operand(Paren(e))
      | PostOp(Ap(_, e)) => Some((0, d == Left ? List.length(e) : 0))
      | PreOp(Lam(_, p)) => Some((0, d == Left ? List.length(p) : 0))
      };
    let move = (~move, d, path, unzipped) =>
      switch (unzipped) {
      | `Exp(unzipped) =>
        let+ (moved, did_it_zip) = move(d, path, unzipped);
        (moved, Option.is_some(did_it_zip));
      | `Pat(unzipped) =>
        let+ (moved, did_it_zip) = Pat.move(d, path, unzipped);
        (moved, Option.is_some(did_it_zip));
      };

    let insert_tiles =
        (~insert_tiles, tiles: HExp.Inner.t, (two_step, target), e) =>
      switch (unzip(two_step, (e, None))) {
      | `Pat(p, unzipped) =>
        switch (tiles) {
        | Exp(_) => None
        | Other(tiles) =>
          let+ (selection, inserted) = Pat.insert_tiles(tiles, target, p);
          switch (Pat.zip_ztile(inserted, Option.get(unzipped))) {
          | (_, `Pat(_)) => failwith("unzipping and rezipping changed sorts")
          | (two_step, `Exp(rezipped, _)) => (
              cons_ordered_selection(two_step, selection),
              rezipped,
            )
          };
        }
      | `Exp(e, unzipped) =>
        let+ (selection, inserted) = insert_tiles(tiles, target, e);
        let (two_step, `Exp(rezipped, _)) =
          zip_ztile(inserted, Option.get(unzipped));
        (cons_ordered_selection(two_step, selection), rezipped);
      };

    let remove_tiles =
        (~remove_tiles, (two_step, selection), e)
        : option((HExp.Inner.t, t, HExp.t)) =>
      switch (unzip(two_step, (e, None))) {
      | `Exp(e, zrest) =>
        let+ (removed, removed_path, e) = remove_tiles(selection, e);
        let (two_step, `Exp(e, _)) = zip_ztile(e, Option.get(zrest));
        (removed, cons(two_step, removed_path), e);
      | `Pat(p, zrest) =>
        let+ (removed, removed_path, p) = Pat.remove_tiles(selection, p);
        let (two_step, rezipped) = Pat.zip_ztile(p, Option.get(zrest));
        switch (rezipped) {
        | `Pat(_) => failwith("unzipping and rezipping changed sort")
        | `Exp(e, _none) => (
            HExp.Inner.Other(removed),
            cons(two_step, removed_path),
            e,
          )
        };
      };

    let restructure =
        (
          ~place_cursor: [ | `Selection | `Other] as 'pc,
          ~restructure: (~place_cursor: 'pc=?, _, _, _) => _,
          (two_step: two_step, (l, r): ordered_selection, target: t),
          e: HExp.t,
        )
        : option((t, HExp.t)) =>
      switch (unzip(two_step, (e, None))) {
      | `Exp(e, zrest) =>
        let+ (path, e) = restructure(~place_cursor, (l, r), target, e);
        let (_, `Exp(e, _)) = zip_ztile(e, Option.get(zrest));
        (cons(two_step, path), e);
      | `Pat(p, zrest) =>
        let+ (path, p) = Pat.restructure(~place_cursor, (l, r), target, p);
        switch (Pat.zip_ztile(p, Option.get(zrest))) {
        | (_, `Pat(_)) => failwith("unzipping and rezipping changed sort")
        | (_, `Exp(e, _)) => (cons(two_step, path), e)
        };
      };

    let delete_selection =
        (~delete_selection, (two_step, selection), e): option((t, HExp.t)) =>
      switch (unzip(two_step, (e, None))) {
      | `Pat(p, unzipped) =>
        let+ (path, p) = Pat.delete_selection(selection, p);
        switch (Pat.zip_ztile(p, Option.get(unzipped))) {
        | (_, `Pat(_)) => failwith("unzipping and rezipping changed sort")
        | (_, `Exp(e, _)) => (cons(two_step, path), e)
        };
      | `Exp(e, unzipped) =>
        let+ (path, e) = delete_selection(selection, e);
        let (_, `Exp(e, _)) = zip_ztile(e, Option.get(unzipped));
        (cons(two_step, path), e);
      };
  };
  include Common(HExp.Tile, HExp.Inner, P);
};
