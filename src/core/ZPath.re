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
type steps = list(two_step);
[@deriving sexp]
type t = (steps, caret_step);

exception Out_of_sync;
exception Unzip_rezip_changes_sort;

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
  origin: t,
  anchor: t,
  focus: t,
};
[@deriving sexp]
type ordered_selection = (t, t);

let mk_ordered_selection =
    ({anchor, focus, _}: anchored_selection)
    : (ordered_selection, Direction.t) =>
  compare(anchor, focus) <= 0
    ? ((anchor, focus), Right) : ((focus, anchor), Left);

let cons_anchored_selection = (two_step, {origin, anchor, focus}) => {
  origin: cons(two_step, origin),
  anchor: cons(two_step, anchor),
  focus: cons(two_step, focus),
};

let cons_ordered_selection = (two_step, (l, r)) => (
  cons(two_step, l),
  cons(two_step, r),
);

module type COMMON = {
  module T: Tile.S;
  module Z: ZTile.S with module T := T;
  type inner_tiles;

  type zip_result;
  let zip: (T.s, Z.t) => (tile_step, Z.zipper);
  let zip_ztile: (T.s, Z.ztile) => (two_step, zip_result);

  type unzip_result;
  let unzip_tile: (child_step, T.t, Z.t) => unzip_result;
  let unzip: (two_step, Z.zipper) => unzip_result;

  let length_at: (steps, T.s) => int;
  let sort_at: (t, T.s) => Sort.t;

  type did_it_zip = option((two_step, zip_result));

  let children: (~filter: [ | `Open | `Closed]=?, T.t) => list(child_step);

  let move: (Direction.t, t, T.s) => option(t);
  /**
   * `move_zipper(d, zipper, path)` first attempts to returns the next
   * path from `path` in direction `d` within the focused term of `zipper`.
   * If no such path exists (i.e. the cursor is at one of the ends of the
   * focused term), then it attempts to zip `zipper` and try once more.
   */
  let move_zipper: (Direction.t, t, Z.zipper) => option((t, did_it_zip));

  // let select:
  //   (Direction.t, t, Z.zipper) => option((anchored_selection, did_it_zip));
  let delete_selection: (ordered_selection, T.s) => option((t, T.s));
  let round_selection: (ordered_selection, T.s) => ordered_selection;

  let remove_tiles:
    (ordered_selection, T.s) => option((inner_tiles, t, T.s));
  let insert_tiles:
    (inner_tiles, t, T.s) => option((ordered_selection, T.s));
  let restructure:
    (~place_cursor: [ | `Selection | `Other]=?, (t, t), t, T.s) =>
    option((t, T.s));
};

module Common =
       (
         T: Tile.S,
         Z: ZTile.S with module T := T,
         I: {
           type t;
           let wrap: T.s => t;
           let unwrap: t => option(T.s);
         },
         P: {
           type zip_result;

           let sort: Sort.t;
           let sort_at: (t, T.s) => Sort.t;

           let zip_ztile: (T.s, Z.ztile) => (two_step, zip_result);

           let unzip_cis: (two_step, T.s) => option(ZList.t(T.s, T.t));

           let enter_from:
             (Direction.t, T.t) => option((child_step, caret_step));
           let move:
             (Direction.t, (child_step, t), T.t) => option((child_step, t));

           let insert_tiles:
             (I.t, (two_step, t), T.s) => option((ordered_selection, T.s));
           let remove_tiles:
             ((two_step, ordered_selection), T.s) => option((I.t, t, T.s));
           let restructure:
             (
               ~place_cursor: [ | `Selection | `Other],
               (two_step, ordered_selection, t),
               T.s
             ) =>
             option((t, T.s));
           let delete_selection:
             ((two_step, ordered_selection), T.s) => option((t, T.s));
         },
       ) => {
  module Ts = Tiles.Make(T);

  type did_it_zip = option((two_step, P.zip_result));

  let move = (d: Direction.t, (steps, j): t, ts: T.s): option(t) =>
    switch (steps) {
    | [] =>
      let next_tile_step = d == Left ? j - 1 : j;
      let+ next_tile = ListUtil.nth_opt(next_tile_step, ts);
      switch (P.enter_from(Direction.toggle(d), next_tile)) {
      | None => ([], d == Left ? j - 1 : j + 1)
      | Some((child_step, j)) => ([(next_tile_step, child_step)], j)
      };
    | [(tile_step, child_step), ...steps] =>
      let tile = List.nth(ts, tile_step);
      let path =
        switch (P.move(d, (child_step, (steps, j)), tile)) {
        | Some((child_step, path)) => cons((tile_step, child_step), path)
        | None => ([], d == Left ? tile_step : tile_step + 1)
        };
      Some(path);
    };
  let move_zipper =
      (d: Direction.t, path: t, (ts, unzipped): Z.zipper)
      : option((t, did_it_zip)) =>
    switch (move(d, path, ts)) {
    | Some(path) => Some((path, None))
    | None =>
      let+ ztile = unzipped;
      let ((tile_step, _) as two_step, zip_result) = P.zip_ztile(ts, ztile);
      let path = ([], d == Left ? tile_step : tile_step + 1);
      (path, Some((two_step, zip_result)));
    };

  /*
   let select =
       (d: Direction.t, anchor: t, zipper: Z.zipper)
       : option((anchored_selection, did_it_zip)) => {
     let+ (next, did_it_zip) = move_zipper(d, anchor, zipper);
     switch (did_it_zip) {
     | None => ({anchor, focus: next}, did_it_zip)
     | Some((two_step, _)) =>
       let anchor = cons(two_step, anchor);
       ({anchor, focus: next}, did_it_zip);
     };
   };
   */

  /* assumes l, r are maximally unzipped */
  let round_selection =
      ((l, r): ordered_selection, ts: T.s): ordered_selection => {
    let rec next_cis_path = (d: Direction.t, current: t): t =>
      if (P.sort_at(current, ts) == P.sort) {
        current;
      } else {
        let next = Option.get(move(d, current, ts));
        next_cis_path(d, next);
      };
    let l = next_cis_path(Left, l);
    let r = next_cis_path(Right, r);
    (l, r);
  };

  let insert_tiles =
      (tiles: I.t, (steps, j): t, ts: T.s)
      : option((ordered_selection, T.s)) =>
    switch (steps) {
    | [] =>
      let+ tiles = I.unwrap(tiles);
      let (prefix, suffix) = ListUtil.split_n(j, ts);
      let (prefix, tiles, suffix) =
        ListUtil.take_3(Ts.fix_empty_holes([prefix, tiles, suffix]));
      let inserted_selection = (
        ([], List.length(prefix)),
        ([], List.length(prefix) + List.length(tiles)),
      );
      (inserted_selection, prefix @ tiles @ suffix);
    | [two_step, ...steps] =>
      P.insert_tiles(tiles, (two_step, (steps, j)), ts)
    };

  let remove_tiles =
      ((l, r): ordered_selection, ts: T.s): option((I.t, t, T.s)) =>
    switch (l, r) {
    | (([], j_l), ([], j_r)) =>
      let (prefix, removed, suffix) = ListUtil.split_sublist(j_l, j_r, ts);
      let (prefix, suffix) =
        ListUtil.take_2(Ts.fix_empty_holes([prefix, suffix]));
      Some((I.wrap(removed), ([], List.length(prefix)), prefix @ suffix));
    | (([], _), ([_, ..._], _))
    | (([_, ..._], _), ([], _)) => None
    | (([two_step_l, ...steps_l], j_l), ([two_step_r, ...steps_r], j_r)) =>
      if (two_step_l != two_step_r) {
        None;
      } else {
        let selection = ((steps_l, j_l), (steps_r, j_r));
        P.remove_tiles((two_step_l, selection), ts);
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
        let moving_left = j_target <= j_l;
        let (prefix, swapped_l, swapped_r, suffix) =
          if (moving_left) {
            let (prefix_l, prefix_r) = ListUtil.split_n(j_target, prefix);
            (prefix_l, removed, prefix_r, suffix);
          } else {
            let (suffix_l, suffix_r) =
              ListUtil.split_n(
                j_target - (List.length(prefix) + List.length(removed)),
                suffix,
              );
            (prefix, suffix_l, removed, suffix_r);
          };
        let (prefix, swapped_l, swapped_r, suffix) =
          [prefix, swapped_l, swapped_r, suffix]
          |> Ts.fix_empty_holes
          |> ListUtil.take_4;
        let j =
          List.length(
            switch (moving_left, place_cursor) {
            | (true, `Selection)
            | (false, `Other) => prefix
            | (true, `Other)
            | (false, `Selection) => prefix @ swapped_l
            },
          );
        Some((
          ([], j),
          List.concat([prefix, swapped_l, swapped_r, suffix]),
        ));
      | ([], [], [(tile_step, child_step), ...steps]) =>
        let (prefix, removed, suffix) = ListUtil.split_sublist(j_l, j_r, ts);
        let (fixed_prefix, fixed_suffix) =
          ListUtil.take_2(Ts.fix_empty_holes([prefix, suffix]));
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
          P.restructure(~place_cursor, (two_step_l, (l, r), target), ts);
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
      let (prefix, suffix) =
        ListUtil.take_2(Ts.fix_empty_holes([prefix, suffix]));
      let path = ([], List.length(prefix));
      Some((path, prefix @ suffix));
    | (([], j_l), ([two_step_r, ...steps_r], j_r)) =>
      let* {prefix, z: ts, suffix} = P.unzip_cis(two_step_r, ts);
      let (prefix, _deleted) = ListUtil.split_n(j_l, prefix);
      let+ ((_empty, j), ts) =
        delete_selection((([], 0), (steps_r, j_r)), ts);
      let (l, r) = ListUtil.split_n(j, ts);
      let (prefix, l, r, suffix) =
        ListUtil.take_4(Ts.fix_empty_holes([prefix, l, r, suffix]));
      let path = ([], List.length(prefix));
      (path, List.concat([prefix, l, r, suffix]));
    | (([(tile_step, _) as two_step_l, ...steps_l], j_l), ([], j_r)) =>
      let* {prefix, z: ts, suffix} = P.unzip_cis(two_step_l, ts);
      let (_deleted, suffix) =
        ListUtil.split_n(j_r - (tile_step + 1), suffix);
      let+ ((_empty, j), ts) =
        delete_selection(((steps_l, j_l), ([], List.length(ts))), ts);
      let (l, r) = ListUtil.split_n(j, ts);
      let (prefix, l, r, suffix) =
        ListUtil.take_4(Ts.fix_empty_holes([prefix, l, r, suffix]));
      let path = ([], List.length(prefix @ l));
      (path, List.concat([prefix, l, r, suffix]));
    | (([two_step_l, ...steps_l], j_l), ([two_step_r, ...steps_r], j_r)) =>
      if (two_step_l == two_step_r) {
        let selection = ((steps_l, j_l), (steps_r, j_r));
        P.delete_selection((two_step_l, selection), ts);
      } else {
        let* {z: ts_l, _} = P.unzip_cis(two_step_l, ts);
        let* {z: ts_r, _} = P.unzip_cis(two_step_r, ts);
        let+ (_, ts_l) =
          delete_selection(
            ((steps_l, j_l), ([], List.length(ts_l))),
            ts_l,
          )
        and+ (_, ts_r) =
          delete_selection((([], 0), (steps_r, j_r)), ts_r);
        let (prefix, _, suffix) =
          ListUtil.split_sublist(fst(two_step_l), fst(two_step_r) + 1, ts);
        let (prefix, ts_l, ts_r, suffix) =
          ListUtil.take_4(Ts.fix_empty_holes([prefix, ts_l, ts_r, suffix]));
        let path = ([], List.length(prefix));
        (path, List.concat([prefix, ts_l, ts_r, suffix]));
      }
    };
};

module type TYP = {
  include
    COMMON with
      module T := HTyp.T and
      module Z := ZTyp and
      type inner_tiles := HTyp.Inner.t and
      type zip_result = [ | `Typ(ZTyp.zipper) | `Pat(ZPat.zipper)] and
      type unzip_result = [ | `Typ(ZTyp.zipper)];
};
module type PAT = {
  include
    COMMON with
      module T := HPat.T and
      module Z := ZPat and
      type inner_tiles := HPat.Inner.t and
      type zip_result = [ | `Pat(ZPat.zipper) | `Exp(ZExp.zipper)] and
      type unzip_result = [ | `Pat(ZPat.zipper) | `Typ(ZTyp.zipper)];
};
module type EXP = {
  include
    COMMON with
      module T := HExp.T and
      module Z := ZExp and
      type inner_tiles := HExp.Inner.t and
      type zip_result = [ | `Exp(ZExp.zipper)] and
      type unzip_result = [ | `Exp(ZExp.zipper) | `Pat(ZPat.zipper)];
};

module rec Typ: TYP = {
  type zip_result = [ | `Typ(ZTyp.zipper) | `Pat(ZPat.zipper)];

  let zip = (ty: HTyp.t, zty: ZTyp.t): (tile_step, ZTyp.zipper) => (
    List.length(zty.prefix),
    (zty.prefix @ ty @ zty.suffix, zty.z),
  );
  let zip_ztile =
      (subject: HTyp.t, ztile: ZTyp.ztile): (two_step, zip_result) =>
    switch (ztile) {
    | Op(ParenZ_body(zty)) =>
      let (tile_step, (ty, zrest)) =
        zip([Tile.Op(HTyp.T.Paren(subject))], zty);
      ((tile_step, 0), `Typ((ty, zrest)));
    | Pre () => raise(ZTyp.Void_zpre)
    | Post(AnnZ_ann(status, zp)) =>
      let (tile_step, (p, zrest)) =
        Pat.zip([Tile.Post(HPat.T.Ann(status, subject))], zp);
      ((tile_step, 0), `Pat((p, zrest)));
    | Bin () => raise(ZTyp.Void_zbin)
    };

  type unzip_result = [ | `Typ(ZTyp.zipper)];

  let unzip_tile = (r: child_step, tile: HTyp.T.t, zty: ZTyp.t): unzip_result => {
    let invalid = () => raise(Invalid_argument("ZPath.Typ.unzip_tile"));
    HTyp.T.(
      tile
      |> Tile.get(
           fun
           | OpHole
           | Num
           | Bool => invalid()
           | Paren(body) =>
             r == 0
               ? `Typ((body, Some(Tile.Op(ZTyp.ParenZ_body(zty)))))
               : invalid(),
           () => raise(Void_pre),
           () => raise(Void_post),
           fun
           | BinHole
           | Arrow => invalid(),
         )
    );
  };
  let unzip =
      ((l, r): two_step, (ty: HTyp.t, zrest: option(ZTyp.ztile)))
      : unzip_result => {
    let (tile, ze) = {
      let (prefix, tile, suffix) = ListUtil.split_nth(l, ty);
      (tile, ZTyp.mk(~prefix, ~z=?zrest, ~suffix, ()));
    };
    unzip_tile(r, tile, ze);
  };

  let rec length_at = (steps, ty) =>
    switch (steps) {
    | [] => List.length(ty)
    | [two_step, ...steps] =>
      let `Typ(ty, _) = unzip(two_step, (ty, None));
      length_at(steps, ty);
    };

  let sort_at = (_, _) => Sort.Typ;

  let children = (~filter=?) =>
    HTyp.T.(
      Tile.get(
        fun
        | OpHole
        | Num
        | Bool => []
        | Paren(_) => filter == Some(`Closed) ? [] : [0],
        () => raise(Void_pre),
        () => raise(Void_post),
        fun
        | BinHole
        | Arrow => [],
      )
    );

  module P = {
    type nonrec zip_result = zip_result;

    let sort = Sort.Typ;
    let sort_at = sort_at;

    let zip_ztile = zip_ztile;
    let unzip = unzip;
    let unzip_cis = (two_step, ty) => {
      let `Typ(ty, unzipped) = unzip(two_step, (ty, None));
      switch (Option.get(unzipped)) {
      | Op(ParenZ_body({prefix, suffix, _})) =>
        Some(ZList.{prefix, z: ty, suffix})
      | Pre () => raise(ZTyp.Void_zpre)
      | Post(AnnZ_ann(_)) => raise(Unzip_rezip_changes_sort)
      | Bin () => raise(ZTyp.Void_zbin)
      };
    };

    let enter_from =
        (d: Direction.t, tile: HTyp.T.t): option((child_step, caret_step)) =>
      switch (tile) {
      | Op(OpHole | Num | Bool)
      | Bin(BinHole | Arrow) => None
      | Op(Paren(ty)) => Some((0, d == Left ? 0 : List.length(ty)))
      | Pre () => raise(HTyp.T.Void_pre)
      | Post () => raise(HTyp.T.Void_post)
      };

    let move = (d, (child_step, path), tile) => {
      let `Typ(ty, unzipped) = unzip_tile(child_step, tile, ZTyp.mk());
      switch (Typ.move(d, path, ty)) {
      | Some(path) => Some((child_step, path))
      | None =>
        ZTyp.(
          Option.get(unzipped)
          |> Tile.get(
               fun
               | ParenZ_body(_) => None,
               () => raise(Void_zpre),
               fun
               | AnnZ_ann(_) => raise(Unzip_rezip_changes_sort),
               () => raise(Void_zbin),
             )
        )
      };
    };

    let insert_tiles = (tiles: HTyp.Inner.t, (two_step, target), ty) => {
      let `Typ(ty, unzipped) = unzip(two_step, (ty, None));
      let+ (selection, inserted) = Typ.insert_tiles(tiles, target, ty);
      switch (Typ.zip_ztile(inserted, Option.get(unzipped))) {
      | (_, `Pat(_)) => raise(Unzip_rezip_changes_sort)
      | (two_step, `Typ(rezipped, _)) => (
          cons_ordered_selection(two_step, selection),
          rezipped,
        )
      };
    };

    let remove_tiles =
        ((two_step, selection), ty): option((HTyp.Inner.t, t, HTyp.t)) => {
      let `Typ(ty, zrest) = unzip(two_step, (ty, None));
      let+ (removed, removed_path, ty) = Typ.remove_tiles(selection, ty);
      switch (zip_ztile(ty, Option.get(zrest))) {
      | (_, `Pat(_)) => raise(Unzip_rezip_changes_sort)
      | (two_step, `Typ(ty, _none)) => (
          removed,
          cons(two_step, removed_path),
          ty,
        )
      };
    };

    let restructure =
        (
          ~place_cursor: [ | `Selection | `Other],
          (two_step, (l, r), target),
          ty: HTyp.t,
        )
        : option((t, HTyp.t)) => {
      let `Typ(ty, zrest) = unzip(two_step, (ty, None));
      let+ (path, ty) = Typ.restructure(~place_cursor, (l, r), target, ty);
      switch (zip_ztile(ty, Option.get(zrest))) {
      | (_, `Pat(_)) => raise(Unzip_rezip_changes_sort)
      | (_, `Typ(ty, _)) => (cons(two_step, path), ty)
      };
    };

    let delete_selection = ((two_step, selection), ty): option((t, HTyp.t)) => {
      let `Typ(ty, unzipped) = unzip(two_step, (ty, None));
      let+ (path, ty) = Typ.delete_selection(selection, ty);
      switch (zip_ztile(ty, Option.get(unzipped))) {
      | (_, `Pat(_)) => raise(Unzip_rezip_changes_sort)
      | (_, `Typ(ty, _)) => (cons(two_step, path), ty)
      };
    };
  };
  include Common(HTyp.T, ZTyp, HTyp.Inner, P);
}
and Pat: PAT = {
  type zip_result = [ | `Pat(ZPat.zipper) | `Exp(ZExp.zipper)];

  let zip = (p: HPat.t, zp: ZPat.t): (tile_step, ZPat.zipper) => (
    List.length(zp.prefix),
    (zp.prefix @ p @ zp.suffix, zp.z),
  );
  let zip_ztile = (p: HPat.t, ztile: ZPat.ztile): (two_step, zip_result) =>
    switch (ztile) {
    | Op(ParenZ_body(zp)) =>
      let (tile_step, (p, zrest)) = zip([Tile.Op(HPat.T.Paren(p))], zp);
      ((tile_step, 0), `Pat((p, zrest)));
    | Pre(LamZ_pat(status, ze)) =>
      let (tile_step, (e, zrest)) =
        Exp.zip([Tile.Pre(HExp.T.Lam(status, p))], ze);
      ((tile_step, 0), `Exp((e, zrest)));
    | Pre(LetZ_pat(ze, def)) =>
      let (tile_step, (e, zrest)) =
        Exp.zip([Tile.Pre(HExp.T.Let(p, def))], ze);
      ((tile_step, 0), `Exp((e, zrest)));
    | Post () => raise(ZPat.Void_zpost)
    | Bin () => raise(ZPat.Void_zbin)
    };

  type unzip_result = [ | `Pat(ZPat.zipper) | `Typ(ZTyp.zipper)];

  let unzip_tile = (r: child_step, tile: HPat.T.t, zp: ZPat.t): unzip_result => {
    open HPat.T;
    let invalid = () => raise(Invalid_argument("ZPath.Pat.unzip_tile"));
    tile
    |> Tile.get(
         fun
         | OpHole
         | Var(_) => invalid()
         | Paren(body) =>
           r == 0
             ? `Pat((body, Some(Tile.Op(ZPat.ParenZ_body(zp)))))
             : invalid(),
         () => raise(Void_pre),
         fun
         | Ann(status, ann) =>
           r == 0
             ? `Typ((ann, Some(Tile.Post(ZTyp.AnnZ_ann(status, zp)))))
             : invalid(),
         fun
         | BinHole => invalid(),
       );
  };
  let unzip =
      ((l, r): two_step, (p: HPat.t, zrest: option(ZPat.ztile)))
      : unzip_result => {
    let (tile, zp) = {
      let (prefix, tile, suffix) = ListUtil.split_nth(l, p);
      (tile, ZPat.mk(~prefix, ~z=?zrest, ~suffix, ()));
    };
    unzip_tile(r, tile, zp);
  };

  let rec length_at = (steps, p) =>
    switch (steps) {
    | [] => List.length(p)
    | [two_step, ...steps] =>
      switch (unzip(two_step, (p, None))) {
      | `Typ(ty, _) => Typ.length_at(steps, ty)
      | `Pat(p, _) => length_at(steps, p)
      }
    };

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

  let children = (~filter=?) =>
    HPat.T.(
      Tile.get(
        fun
        | OpHole
        | Var(_) => []
        | Paren(_) =>
          switch (filter) {
          | Some(`Closed) => []
          | Some(`Open)
          | None => [0]
          },
        () => raise(Void_pre),
        fun
        | Ann(_) =>
          switch (filter) {
          | Some(`Open) => []
          | Some(`Closed)
          | None => [0]
          },
        fun
        | BinHole => [],
      )
    );

  module P = {
    type nonrec zip_result = zip_result;

    let sort = Sort.Pat;
    let sort_at = sort_at;

    let zip_ztile = zip_ztile;
    let unzip = unzip;
    let unzip_cis = (two_step, p) =>
      switch (unzip(two_step, (p, None))) {
      | `Typ(_) => None
      | `Pat(p, unzipped) =>
        switch (Option.get(unzipped)) {
        | Op(ParenZ_body({prefix, suffix, _})) =>
          Some(ZList.{prefix, z: p, suffix})
        | Pre(LamZ_pat(_) | LetZ_pat(_)) => None
        | Post () => raise(ZPat.Void_zpost)
        | Bin () => raise(ZPat.Void_zbin)
        }
      };

    let enter_from =
        (d: Direction.t, tile: HPat.T.t): option((child_step, caret_step)) =>
      switch (tile) {
      | Op(OpHole | Var(_))
      | Bin(BinHole) => None
      | Op(Paren(p)) => Some((0, d == Left ? 0 : List.length(p)))
      | Post(Ann(_, ty)) => Some((0, d == Left ? 0 : List.length(ty)))
      | Pre () => raise(HPat.T.Void_pre)
      };

    let move = (d, (child_step, path), tile) =>
      switch (unzip_tile(child_step, tile, ZPat.mk())) {
      | `Typ(ty, unzipped) =>
        switch (Typ.move(d, path, ty)) {
        | Some(path) => Some((child_step, path))
        | None =>
          ZTyp.(
            Option.get(unzipped)
            |> Tile.get(
                 fun
                 | ParenZ_body(_) => raise(Unzip_rezip_changes_sort),
                 () => raise(Void_zpre),
                 fun
                 | AnnZ_ann(_) => None,
                 () => raise(Void_zbin),
               )
          )
        }
      | `Pat(p, unzipped) =>
        switch (Pat.move(d, path, p)) {
        | Some(path) => Some((child_step, path))
        | None =>
          ZPat.(
            Option.get(unzipped)
            |> Tile.get(
                 fun
                 | ParenZ_body(_) => None,
                 fun
                 | LamZ_pat(_)
                 | LetZ_pat(_) => raise(Unzip_rezip_changes_sort),
                 () => raise(Void_zpost),
                 () => raise(Void_zbin),
               )
          )
        }
      };

    let insert_tiles = (tiles: HPat.Inner.t, (two_step, target), p) =>
      switch (unzip(two_step, (p, None))) {
      | `Typ(ty, unzipped) =>
        switch (tiles) {
        | Pat(_) => None
        | Other(tiles) =>
          let+ (selection, inserted) = Typ.insert_tiles(tiles, target, ty);
          switch (Typ.zip_ztile(inserted, Option.get(unzipped))) {
          | (_, `Typ(_)) => raise(Unzip_rezip_changes_sort)
          | (two_step, `Pat(rezipped, _)) => (
              cons_ordered_selection(two_step, selection),
              rezipped,
            )
          };
        }
      | `Pat(p, unzipped) =>
        let+ (selection, inserted) = Pat.insert_tiles(tiles, target, p);
        switch (zip_ztile(inserted, Option.get(unzipped))) {
        | (_, `Exp(_)) => raise(Unzip_rezip_changes_sort)
        | (_, `Pat(rezipped, _)) => (
            cons_ordered_selection(two_step, selection),
            rezipped,
          )
        };
      };

    let remove_tiles =
        ((two_step, selection), p): option((HPat.Inner.t, t, HPat.t)) =>
      switch (unzip(two_step, (p, None))) {
      | `Pat(p, zrest) =>
        let+ (removed, removed_path, p) = Pat.remove_tiles(selection, p);
        switch (zip_ztile(p, Option.get(zrest))) {
        | (_, `Exp(_)) => raise(Unzip_rezip_changes_sort)
        | (two_step, `Pat(p, _)) => (
            removed,
            cons(two_step, removed_path),
            p,
          )
        };
      | `Typ(ty, zrest) =>
        let+ (removed, removed_path, ty) = Typ.remove_tiles(selection, ty);
        switch (Typ.zip_ztile(ty, Option.get(zrest))) {
        | (_, `Typ(_)) => raise(Unzip_rezip_changes_sort)
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
          (two_step, (l, r), target),
          p: HPat.t,
        )
        : option((t, HPat.t)) =>
      switch (unzip(two_step, (p, None))) {
      | `Pat(p, zrest) =>
        let+ (path, p) = Pat.restructure(~place_cursor, (l, r), target, p);
        switch (zip_ztile(p, Option.get(zrest))) {
        | (_, `Exp(_)) => raise(Unzip_rezip_changes_sort)
        | (_, `Pat(p, _none)) => (cons(two_step, path), p)
        };
      | `Typ(ty, zrest) =>
        let+ (path, ty) =
          Typ.restructure(~place_cursor, (l, r), target, ty);
        switch (Typ.zip_ztile(ty, Option.get(zrest))) {
        | (_, `Typ(_)) => raise(Unzip_rezip_changes_sort)
        | (_, `Pat(p, _none)) => (cons(two_step, path), p)
        };
      };

    let delete_selection = ((two_step, selection), p): option((t, HPat.t)) =>
      switch (unzip(two_step, (p, None))) {
      | `Typ(ty, unzipped) =>
        let+ (path, ty) = Typ.delete_selection(selection, ty);
        switch (Typ.zip_ztile(ty, Option.get(unzipped))) {
        | (_, `Typ(_)) => raise(Unzip_rezip_changes_sort)
        | (_, `Pat(p, _)) => (cons(two_step, path), p)
        };
      | `Pat(p, unzipped) =>
        let+ (path, p) = Pat.delete_selection(selection, p);
        switch (zip_ztile(p, Option.get(unzipped))) {
        | (_, `Exp(_)) => raise(Unzip_rezip_changes_sort)
        | (_, `Pat(p, _)) => (cons(two_step, path), p)
        };
      };
  };
  include Common(HPat.T, ZPat, HPat.Inner, P);
}
and Exp: EXP = {
  type zip_result = [ | `Exp(ZExp.zipper)];

  let zip = (e: HExp.t, ze: ZExp.t): (tile_step, ZExp.zipper) => (
    List.length(ze.prefix),
    (ze.prefix @ e @ ze.suffix, ze.z),
  );
  let zip_ztile = (e: HExp.t, ztile: ZExp.ztile): (two_step, zip_result) =>
    switch (ztile) {
    | Op(ParenZ_body(ze)) =>
      let (tile_step, (e, zrest)) = zip([Tile.Op(HExp.T.Paren(e))], ze);
      ((tile_step, 0), `Exp((e, zrest)));
    | Pre(LetZ_def(p, ze)) =>
      let (tile_step, (e, zrest)) =
        zip([Tile.Pre(HExp.T.Let(p, e))], ze);
      ((tile_step, 1), `Exp((e, zrest)));
    | Post(ApZ_arg(status, ze)) =>
      let (tile_step, (e, zrest)) =
        zip([Tile.Post(HExp.T.Ap(status, e))], ze);
      ((tile_step, 0), `Exp((e, zrest)));
    | Bin () => raise(ZExp.Void_zbin)
    };

  type unzip_result = [ | `Exp(ZExp.zipper) | `Pat(ZPat.zipper)];

  let unzip_tile = (r: child_step, tile: HExp.T.t, ze: ZExp.t): unzip_result => {
    open HExp.T;
    let invalid = () => raise(Invalid_argument("ZPath.Exp.unzip_tile"));
    tile
    |> Tile.get(
         fun
         | OpHole
         | Num(_)
         | Var(_) => invalid()
         | Paren(body) =>
           r == 0
             ? `Exp((body, Some(Tile.Op(ZExp.ParenZ_body(ze)))))
             : invalid(),
         fun
         | Lam(status, p) =>
           r == 0
             ? `Pat((p, Some(Tile.Pre(ZPat.LamZ_pat(status, ze)))))
             : invalid()
         | Let(p, def) =>
           switch (r) {
           | 0 => `Pat((p, Some(Tile.Pre(ZPat.LetZ_pat(ze, def)))))
           | 1 => `Exp((def, Some(Tile.Pre(ZExp.LetZ_def(p, ze)))))
           | _ => invalid()
           },
         fun
         | Ap(status, arg) =>
           r == 0
             ? `Exp((arg, Some(Tile.Post(ZExp.ApZ_arg(status, ze)))))
             : invalid(),
         fun
         | BinHole
         | Plus(_) => invalid(),
       );
  };
  let unzip =
      ((l, r): two_step, (e: HExp.t, zrest: option(ZExp.ztile)))
      : unzip_result => {
    let (tile, ze) = {
      let (prefix, tile, suffix) = ListUtil.split_nth(l, e);
      (tile, ZExp.mk(~prefix, ~z=?zrest, ~suffix, ()));
    };
    unzip_tile(r, tile, ze);
  };

  let rec length_at = (steps, e) =>
    switch (steps) {
    | [] => List.length(e)
    | [two_step, ...steps] =>
      switch (unzip(two_step, (e, None))) {
      | `Pat(p, _) => Pat.length_at(steps, p)
      | `Exp(e, _) => length_at(steps, e)
      }
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

  let children = (~filter=?) =>
    HExp.T.(
      Tile.get(
        fun
        | OpHole
        | Num(_)
        | Var(_) => []
        | Paren(_) =>
          switch (filter) {
          | Some(`Closed) => []
          | _ => [0]
          },
        fun
        | Lam(_) =>
          switch (filter) {
          | Some(`Open) => []
          | _ => [0]
          }
        | Let(_) =>
          switch (filter) {
          | None => [0, 1]
          | Some(`Closed) => [0]
          | Some(`Open) => [1]
          },
        fun
        | Ap(_) =>
          switch (filter) {
          | Some(`Closed) => []
          | _ => [0]
          },
        fun
        | Plus(_)
        | BinHole => [],
      )
    );

  module P = {
    type nonrec zip_result = zip_result;

    let sort = Sort.Exp;
    let sort_at = sort_at;

    let zip_ztile = zip_ztile;
    let unzip = unzip;
    let unzip_cis = (two_step, e) =>
      switch (unzip(two_step, (e, None))) {
      | `Pat(_) => None
      | `Exp(e, unzipped) =>
        switch (Option.get(unzipped)) {
        | Op(ParenZ_body({prefix, suffix, _}))
        | Pre(LetZ_def(_, {prefix, suffix, _}))
        | Post(ApZ_arg(_, {prefix, suffix, _})) =>
          Some(ZList.{prefix, z: e, suffix})
        | Bin () => raise(ZExp.Void_zbin)
        }
      };

    let enter_from =
        (d: Direction.t): (HExp.T.t => option((child_step, caret_step))) =>
      HExp.T.(
        Tile.get(
          fun
          | OpHole
          | Num(_)
          | Var(_) => None
          | Paren(body) => Some((0, d == Left ? 0 : List.length(body))),
          fun
          | Lam(_, p) => Some((0, d == Left ? 0 : List.length(p)))
          | Let(_, def) =>
            d == Left ? Some((0, 0)) : Some((1, List.length(def))),
          fun
          | Ap(_, arg) => Some((0, d == Left ? 0 : List.length(arg))),
          fun
          | Plus(_)
          | BinHole => None,
        )
      );
    let move = (d, (child_step, path), tile) =>
      switch (unzip_tile(child_step, tile, ZExp.mk())) {
      | `Pat(p, unzipped) =>
        switch (Pat.move(d, path, p)) {
        | Some(path) => Some((child_step, path))
        | None =>
          ZPat.(
            Option.get(unzipped)
            |> Tile.get(
                 fun
                 | ParenZ_body(_) => raise(Unzip_rezip_changes_sort),
                 fun
                 | LamZ_pat(_) => None
                 | LetZ_pat(_) => d == Left ? None : Some((1, ([], 0))),
                 () => raise(Void_zpost),
                 () => raise(Void_zbin),
               )
          )
        }
      | `Exp(e, unzipped) =>
        switch (Exp.move(d, path, e)) {
        | Some(path) => Some((child_step, path))
        | None =>
          ZExp.(
            Option.get(unzipped)
            |> Tile.get(
                 (
                   fun
                   | ParenZ_body(_) => None
                 ),
                 (
                   fun
                   | LetZ_def(p, _) =>
                     d == Left ? Some((0, ([], List.length(p)))) : None
                 ),
                 (
                   fun
                   | ApZ_arg(_) => None
                 ),
                 () =>
                 raise(Void_zbin)
               )
          )
        }
      };

    let insert_tiles = (tiles: HExp.Inner.t, (two_step, target), e) =>
      switch (unzip(two_step, (e, None))) {
      | `Pat(p, unzipped) =>
        switch (tiles) {
        | Exp(_) => None
        | Other(tiles) =>
          let+ (selection, inserted) = Pat.insert_tiles(tiles, target, p);
          switch (Pat.zip_ztile(inserted, Option.get(unzipped))) {
          | (_, `Pat(_)) => raise(Unzip_rezip_changes_sort)
          | (two_step, `Exp(rezipped, _)) => (
              cons_ordered_selection(two_step, selection),
              rezipped,
            )
          };
        }
      | `Exp(e, unzipped) =>
        let+ (selection, inserted) = Exp.insert_tiles(tiles, target, e);
        let (two_step, `Exp(rezipped, _)) =
          zip_ztile(inserted, Option.get(unzipped));
        (cons_ordered_selection(two_step, selection), rezipped);
      };

    let remove_tiles =
        ((two_step, selection), e): option((HExp.Inner.t, t, HExp.t)) =>
      switch (unzip(two_step, (e, None))) {
      | `Exp(e, zrest) =>
        let+ (removed, removed_path, e) = Exp.remove_tiles(selection, e);
        let (two_step, `Exp(e, _)) = zip_ztile(e, Option.get(zrest));
        (removed, cons(two_step, removed_path), e);
      | `Pat(p, zrest) =>
        let+ (removed, removed_path, p) = Pat.remove_tiles(selection, p);
        let (two_step, rezipped) = Pat.zip_ztile(p, Option.get(zrest));
        switch (rezipped) {
        | `Pat(_) => raise(Unzip_rezip_changes_sort)
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
          (two_step: two_step, (l, r): ordered_selection, target: t),
          e: HExp.t,
        )
        : option((t, HExp.t)) =>
      switch (unzip(two_step, (e, None))) {
      | `Exp(e, zrest) =>
        let+ (path, e) = Exp.restructure(~place_cursor, (l, r), target, e);
        let (_, `Exp(e, _)) = zip_ztile(e, Option.get(zrest));
        (cons(two_step, path), e);
      | `Pat(p, zrest) =>
        let+ (path, p) = Pat.restructure(~place_cursor, (l, r), target, p);
        switch (Pat.zip_ztile(p, Option.get(zrest))) {
        | (_, `Pat(_)) => raise(Unzip_rezip_changes_sort)
        | (_, `Exp(e, _)) => (cons(two_step, path), e)
        };
      };

    let delete_selection = ((two_step, selection), e): option((t, HExp.t)) =>
      switch (unzip(two_step, (e, None))) {
      | `Pat(p, unzipped) =>
        let+ (path, p) = Pat.delete_selection(selection, p);
        switch (Pat.zip_ztile(p, Option.get(unzipped))) {
        | (_, `Pat(_)) => raise(Unzip_rezip_changes_sort)
        | (_, `Exp(e, _)) => (cons(two_step, path), e)
        };
      | `Exp(e, unzipped) =>
        let+ (path, e) = Exp.delete_selection(selection, e);
        let (_, `Exp(e, _)) = zip_ztile(e, Option.get(unzipped));
        (cons(two_step, path), e);
      };
  };
  include Common(HExp.T, ZExp, HExp.Inner, P);
};
