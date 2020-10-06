open OptUtil.Syntax;

type tile_step = int;
type child_step = int;
type caret_step = int;
type two_step = (tile_step, child_step);
type t = (list(two_step), caret_step);

type selection = (t, t);

let select_while_moving = (start: t, end_: t): (Direction.t => selection) =>
  fun
  | Left => (end_, start)
  | Right => (start, end_);

let cons = (two_step, (steps, j)) => ([two_step, ...steps], j);

let cons_selection = (two_step, (l, r)) => (
  cons(two_step, l),
  cons(two_step, r),
);

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

module rec Typ: {
  type zipped = [ | `Typ(ZTyp.zipper) | `Pat(ZPat.zipper)];
  type did_it_zip = option((two_step, zipped));

  let zip: (HTyp.t, ZTyp.t) => (tile_step, ZTyp.zipper);
  let zip_ztile: (HTyp.t, ZTyp.ztile) => (two_step, zipped);

  type unzipped = [ | `Typ(ZTyp.zipper)];

  let unzip_tile: (child_step, HTyp.Tile.t, ZTyp.t) => unzipped;
  let unzip: (two_step, ZTyp.zipper) => unzipped;

  let sort: (t, HTyp.t) => [ | `Typ];

  /**
   * `move(d, zipper, path)` first attempts to returns the next path
   * from `path` in direction `d` within the focused term of `zipper`.
   * If no such path exists (i.e. the cursor is at one of the ends of
   * the focused term), then it attempts to zip `zipper` and try once
   * more.
   */
  let move: (Direction.t, t, ZTyp.zipper) => option((t, did_it_zip));

  let select:
    (Direction.t, t, ZTyp.zipper) => option((selection, did_it_zip));

  let remove_selection: (selection, HTyp.t) => option((t, HTyp.t));

  let round_selection: (selection, HTyp.t) => selection;

  let remove_tiles: (selection, HTyp.t) => (HTyp.inner_tiles, HTyp.t);
  let insert_tiles: (t, HTyp.inner_tiles, HPat.t) => HTyp.t;
  let restructure:
    (~place_cursor: [ | `Left | `Right]=?, (t, t), t, HTyp.t) =>
    option((t, HTyp.t));
} = {
  type zipped = [ | `Typ(ZTyp.zipper) | `Pat(ZPat.zipper)];
  type did_it_zip = option((two_step, zipped));

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

  let rec move =
          (
            d: Direction.t,
            (steps, k): t,
            (ty, zrest) as zipper: ZTyp.zipper,
          )
          : option((t, did_it_zip)) => {
    let if_left = (then_, else_) => d == Left ? then_ : else_;
    switch (steps) {
    | [] =>
      let next = if_left(k - 1, k);
      switch (List.nth_opt(ty, next), zrest) {
      | (None, None) => None
      | (Some(tile), _) =>
        let path =
          switch (tile) {
          | Operand(OperandHole | Num)
          | BinOp(OperatorHole | Arrow) => (steps, if_left(k - 1, k + 1))
          | Operand(Paren(ty)) => (
              [(next, 0)],
              if_left(List.length(ty), 0),
            )
          | PreOp(_) => raise(HTyp.Tile.Void_PreOp)
          | PostOp(_) => raise(HTyp.Tile.Void_PostOp)
          };
        Some((path, None));
      | (_, Some(ztile)) =>
        let ((tile_step, _) as two_step, zipped) = zip_ztile(ty, ztile);
        let path = ([], if_left(tile_step, tile_step + 1));
        Some((path, Some((two_step, zipped))));
      };
    | [two_step, ...steps] =>
      let `Typ(unzipped) = unzip(two_step, zipper);
      let+ (path, _) = move(d, (steps, k), unzipped);
      (cons(two_step, path), None);
    };
  };

  let sort = (_, _) => `Typ;

  let select =
      (d: Direction.t, start: t, zipper: ZTyp.zipper)
      : option((selection, did_it_zip)) => {
    let* (next, did_it_zip) = move(d, start, zipper);
    switch (did_it_zip) {
    | None => Some((select_while_moving(start, next, d), did_it_zip))
    | Some((two_step, `Typ(_))) =>
      let start = cons(two_step, start);
      Some((select_while_moving(start, next, d), did_it_zip));
    | Some((_, `Pat(zipper))) =>
      let+ (selection, _) = Pat.select(Direction.toggle(d), next, zipper);
      (selection, did_it_zip);
    };
  };

  /* assumes l, r are maximally unzipped */
  let round_selection = (selection, _) => selection;

  let remove_selection = (_, _) => failwith("unimplemented");

  let remove_tiles = (_, _) => failwith("unimplemented");
  let insert_tiles = (_, _, _) => failwith("unimplemented");
  [@warning "-27"]
  let restructure = (~place_cursor=`Left, _, _, _) =>
    failwith("unimplemented");
}
and Pat: {
  type zipped = [ | `Pat(ZPat.zipper) | `Exp(ZExp.zipper)];
  type did_it_zip = option((two_step, zipped));

  let zip: (HPat.t, ZPat.t) => (tile_step, ZPat.zipper);
  let zip_ztile: (HPat.t, ZPat.ztile) => (two_step, zipped);

  type unzipped = [ | `Pat(ZPat.zipper) | `Typ(ZTyp.zipper)];

  let unzip_tile: (child_step, HPat.Tile.t, ZPat.t) => unzipped;
  let unzip: (two_step, ZPat.zipper) => unzipped;

  let sort: (t, HPat.t) => [ | `Pat | `Typ];

  let move: (Direction.t, t, ZPat.zipper) => option((t, did_it_zip));

  let select:
    (Direction.t, t, ZPat.zipper) => option((selection, did_it_zip));

  let remove_selection: (selection, HPat.t) => option((t, HPat.t));

  let round_selection: (selection, HPat.t) => selection;

  let remove_tiles:
    (selection, HPat.t) => option((HPat.inner_tiles, HPat.t));
  let insert_tiles: (t, HPat.inner_tiles, HPat.t) => option(HPat.t);
  let restructure:
    (~place_cursor: [ | `Left | `Right]=?, selection, t, HPat.t) =>
    option((t, HPat.t));
} = {
  type zipped = [ | `Pat(ZPat.zipper) | `Exp(ZExp.zipper)];
  type did_it_zip = option((two_step, zipped));

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

  let rec move =
          (d: Direction.t, (steps, k): t, (p, zrest) as zipper: ZPat.zipper)
          : option((t, did_it_zip)) => {
    let if_left = (then_, else_) => d == Left ? then_ : else_;
    switch (steps) {
    | [] =>
      let next = if_left(k - 1, k);
      switch (List.nth_opt(p, next), zrest) {
      | (None, None) => None
      | (Some(tile), _) =>
        let path =
          switch (tile) {
          | Operand(OperandHole | Var(_))
          | BinOp(OperatorHole) => (steps, if_left(k - 1, k + 1))
          | Operand(Paren(p)) => (
              [(next, 0)],
              if_left(List.length(p), 0),
            )
          | PostOp(Ann(_, ty)) => (
              [(next, 0)],
              if_left(List.length(ty), 0),
            )
          | PreOp(_) => raise(HPat.Tile.Void_PreOp)
          };
        Some((path, None));
      | (_, Some(ztile)) =>
        let ((tile_step, _) as two_step, zipped) = zip_ztile(p, ztile);
        let path = ([], if_left(tile_step, tile_step + 1));
        Some((path, Some((two_step, zipped))));
      };
    | [two_step, ...steps] =>
      let+ path =
        switch (unzip(two_step, zipper)) {
        | `Pat(unzipped) => Option.map(fst, move(d, (steps, k), unzipped))
        | `Typ(unzipped) =>
          Option.map(fst, Typ.move(d, (steps, k), unzipped))
        };
      (cons(two_step, path), None);
    };
  };

  // TODO fix output type
  let rec sort = ((steps, j): t, p: HPat.t): [ | `Typ | `Pat] =>
    switch (steps) {
    | [] => `Pat
    | [two_step, ...steps] =>
      let path = (steps, j);
      switch (unzip(two_step, (p, None))) {
      | `Typ(_) => `Typ
      | `Pat(p, _) => sort(path, p)
      };
    };

  let select =
      (d: Direction.t, start: t, (p, _) as zipper: ZPat.zipper)
      : option((selection, did_it_zip)) => {
    let rec go = (current: t): option((selection, did_it_zip)) => {
      let* (next, did_it_zip) = move(d, current, zipper);
      switch (did_it_zip) {
      | None =>
        switch (sort(next, p)) {
        | `Typ => go(next)
        | `Pat => Some((select_while_moving(start, next, d), did_it_zip))
        }
      | Some((two_step, `Pat(_))) =>
        let start = cons(two_step, start);
        Some((select_while_moving(start, next, d), did_it_zip));
      | Some((_, `Exp(zipper))) =>
        let+ (selection, _) = Exp.select(Direction.toggle(d), next, zipper);
        (selection, did_it_zip);
      };
    };
    go(start);
  };

  /* assumes l, r are maximally unzipped */
  let round_selection = ((l, r): selection, p: HPat.t): selection => {
    let rec next_pat_path = (d: Direction.t, current: t): t => {
      let (next, _) = Option.get(move(d, current, (p, None)));
      switch (sort(next, p)) {
      | `Typ => next_pat_path(d, next)
      | `Pat => current
      };
    };
    let l =
      switch (sort(l, p)) {
      | `Pat => l
      | _ => next_pat_path(Left, l)
      };
    let r =
      switch (sort(r, p)) {
      | `Pat => r
      | _ => next_pat_path(Right, r)
      };
    (l, r);
  };

  let remove_selection = (_, _) => failwith("unimplemented");

  let remove_tiles = (_, _) => failwith("unimplemented");
  let insert_tiles = (_, _, _) => failwith("unimplemented");
  [@warning "-27"]
  let restructure = (~place_cursor=`Left, (_, _), _, _) =>
    failwith("unimplemented");
}
and Exp: {
  type zipped = [ | `Exp(ZExp.zipper)];
  type did_it_zip = option((two_step, zipped));

  let zip: (HExp.t, ZExp.t) => (tile_step, ZExp.zipper);
  let zip_ztile: (HExp.t, ZExp.ztile) => (two_step, zipped);

  type unzipped = [ | `Exp(ZExp.zipper) | `Pat(ZPat.zipper)];

  let unzip_tile: (child_step, HExp.Tile.t, ZExp.t) => unzipped;
  let unzip: (two_step, ZExp.zipper) => unzipped;

  let sort: (t, HExp.t) => [ | `Exp | `Pat | `Typ];

  let move: (Direction.t, t, ZExp.zipper) => option((t, did_it_zip));

  let select:
    (Direction.t, t, ZExp.zipper) => option((selection, did_it_zip));

  let remove_selection: (selection, HExp.t) => option((t, HExp.t));

  let round_selection: (selection, HExp.t) => selection;

  let remove_tiles:
    (selection, HExp.t) => option((HExp.inner_tiles, HExp.t));
  let insert_tiles: (t, HExp.inner_tiles, HExp.t) => option(HExp.t);
  let restructure:
    (~place_cursor: [ | `Left | `Right]=?, selection, t, HExp.t) =>
    option((t, HExp.t));
} = {
  type zipped = [ | `Exp(ZExp.zipper)];
  type did_it_zip = option((two_step, zipped));

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

  let rec move =
          (d: Direction.t, (steps, k): t, (e, zrest) as zipper: ZExp.zipper)
          : option((t, did_it_zip)) => {
    let if_left = (then_, else_) => d == Left ? then_ : else_;
    switch (steps) {
    | [] =>
      let next = if_left(k - 1, k);
      switch (List.nth_opt(e, next), zrest) {
      | (None, None) => None
      | (Some(tile), _) =>
        let path =
          switch (tile) {
          | Operand(OperandHole | Num(_) | Var(_))
          | BinOp(Plus(_) | OperatorHole) => (steps, if_left(k - 1, k + 1))
          | Operand(Paren(e))
          | PostOp(Ap(_, e)) => (
              [(next, 0), ...steps],
              if_left(List.length(e), 0),
            )
          | PreOp(Lam(_, p)) => (
              [(next, 0), ...steps],
              if_left(List.length(p), 0),
            )
          };
        Some((path, None));
      | (_, Some(ztile)) =>
        let ((tile_step, _) as two_step, zipped) = zip_ztile(e, ztile);
        let path = ([], if_left(tile_step, tile_step + 1));
        Some((path, Some((two_step, zipped))));
      };
    | [two_step, ...steps] =>
      let+ path =
        switch (unzip(two_step, zipper)) {
        | `Exp(unzipped) => Option.map(fst, move(d, (steps, k), unzipped))
        | `Pat(unzipped) =>
          Option.map(fst, Pat.move(d, (steps, k), unzipped))
        };
      (cons(two_step, path), None);
    };
  };

  let rec sort = ((steps, j): t, e: HExp.t): [ | `Typ | `Pat | `Exp] =>
    switch (steps) {
    | [] => `Exp
    | [two_step, ...steps] =>
      let path = (steps, j);
      switch (unzip(two_step, (e, None))) {
      | `Pat(p, _) =>
        switch (Pat.sort(path, p)) {
        | `Typ => `Typ
        | `Pat => `Pat
        }
      | `Exp(e, _) => sort(path, e)
      };
    };

  let select =
      (d: Direction.t, start: t, (e, _) as zipper: ZExp.zipper)
      : option((selection, did_it_zip)) => {
    let rec go = (current: t): option((selection, did_it_zip)) => {
      let* (next, did_it_zip) = move(d, current, zipper);
      switch (did_it_zip) {
      | None =>
        switch (sort(next, e)) {
        | `Typ
        | `Pat => go(next)
        | `Exp => Some((select_while_moving(start, next, d), did_it_zip))
        }
      | Some((two_step, `Exp(_))) =>
        let start = cons(two_step, start);
        Some((select_while_moving(start, next, d), did_it_zip));
      };
    };
    go(start);
  };

  let rec remove_tiles =
          ((l, r): selection, e: HExp.t)
          : option((HExp.inner_tiles, HExp.t)) =>
    switch (l, r) {
    | (([], j_l), ([], j_r)) =>
      let (prefix, removed, suffix) = ListUtil.split_sublist(j_l, j_r, e);
      Some((Exp(removed), prefix @ suffix));
    | (([], _), ([_, ..._], _))
    | (([_, ..._], _), ([], _)) => None
    | (([two_step_l, ...steps_l], j_l), ([two_step_r, ...steps_r], j_r)) =>
      if (two_step_l != two_step_r) {
        None;
      } else {
        switch (unzip(two_step_l, (e, None))) {
        | `Exp(e, zrest) =>
          let+ (removed, e) =
            remove_tiles(((steps_l, j_l), (steps_r, j_r)), e);
          let ztile = OptUtil.get(() => assert(false), zrest);
          let (_, `Exp(e, _)) = zip_ztile(e, ztile);
          (removed, e);
        | `Pat(p, zrest) =>
          let+ (removed, p) =
            Pat.remove_tiles(((steps_l, j_l), (steps_r, j_r)), p);
          let (_, rezipped) = Pat.zip_ztile(p, Option.get(zrest));
          switch (rezipped) {
          | `Pat(_) => failwith("unzipping and rezipping changed sort")
          | `Exp(e, _none) => (HExp.Other(removed), e)
          };
        };
      }
    };

  let rec remove_selection =
          ((l, r): selection, e: HExp.t): option((t, HExp.t)) =>
    switch (l, r) {
    | (([], j_l), ([], j_r)) =>
      let (prefix, _, suffix) = ListUtil.split_sublist(j_l, j_r, e);
      Some((l, prefix @ suffix));
    | (([], j_l), ([two_step_r, ...steps_r], j_r)) =>
      switch (unzip(two_step_r, (e, None))) {
      | `Pat(_) => None
      | `Exp(e, unzipped) =>
        let+ (path, e) = remove_selection((([], 0), (steps_r, j_r)), e);
        switch (Option.get(unzipped)) {
        | Operand(ParenZ_body({prefix, suffix, _}))
        | PostOp(ApZ_arg(_, {prefix, suffix, _})) =>
          let (prefix, _removed) = ListUtil.split_n(j_l, prefix);
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
          (path, prefix @ e @ suffix);
        | PreOp(_) => raise(ZExp.Void_ZPreOp)
        | BinOp(_) => raise(ZExp.Void_ZBinOp)
        };
      }
    | (([(tile_step, _) as two_step_l, ...steps_l], j_l), ([], j_r)) =>
      switch (unzip(two_step_l, (e, None))) {
      | `Pat(_) => None
      | `Exp(e, unzipped) =>
        let+ (path, e) =
          remove_selection(((steps_l, j_l), ([], List.length(e))), e);
        switch (Option.get(unzipped)) {
        | Operand(ParenZ_body({prefix, suffix, _}))
        | PostOp(ApZ_arg(_, {prefix, suffix, _})) =>
          let (_removed, suffix) =
            ListUtil.split_n(j_r - (tile_step + 1), suffix);
          let path = {
            let n = List.length(prefix) + List.length(e);
            switch (path) {
            | ([], j) => ([], j + n)
            | ([(tile_step, child_step), ...steps], j) => (
                [(tile_step + n, child_step), ...steps],
                j,
              )
            };
          };
          (path, prefix @ e @ suffix);
        | PreOp(_) => raise(ZExp.Void_ZPreOp)
        | BinOp(_) => raise(ZExp.Void_ZBinOp)
        };
      }
    | (([two_step_l, ...steps_l], j_l), ([two_step_r, ...steps_r], j_r)) =>
      if (two_step_l == two_step_r) {
        switch (unzip(two_step_l, (e, None))) {
        | `Pat(p, unzipped) =>
          let+ (path, p) =
            Pat.remove_selection(((steps_l, j_l), (steps_r, j_r)), p);
          switch (Pat.zip_ztile(p, Option.get(unzipped))) {
          | (_, `Pat(_)) => failwith("unzipping and rezipping changed sort")
          | (_, `Exp(e, _)) => (cons(two_step_l, path), e)
          };
        | `Exp(e, unzipped) =>
          let+ (path, e) =
            remove_selection(((steps_l, j_l), (steps_r, j_r)), e);
          let (_, `Exp(e, _)) = zip_ztile(e, Option.get(unzipped));
          (cons(two_step_l, path), e);
        };
      } else {
        switch (
          unzip(two_step_l, (e, None)),
          unzip(two_step_r, (e, None)),
        ) {
        | (`Pat(_), _)
        | (_, `Pat(_)) => None
        | (`Exp(e_l, _), `Exp(e_r, _)) =>
          let+ (path, e_l) =
            remove_selection(
              ((steps_l, j_l), ([], List.length(e_l))),
              e_l,
            )
          and+ (_, e_r) =
            remove_selection((([], 0), (steps_r, j_r)), e_r);
          let (prefix, _, suffix) =
            ListUtil.split_sublist(fst(two_step_l), fst(two_step_r) + 1, e);
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
          (path, prefix @ e_l @ e_r @ suffix);
        };
      }
    };

  /* assumes l, r are maximally unzipped */
  let round_selection = ((l, r): selection, e: HExp.t): selection => {
    let rec next_exp_path = (d: Direction.t, current: t): t => {
      let (next, _) = Option.get(move(d, current, (e, None)));
      switch (sort(next, e)) {
      | `Typ
      | `Pat => next_exp_path(d, next)
      | `Exp => current
      };
    };
    let l =
      switch (sort(l, e)) {
      | `Exp => l
      | _ => next_exp_path(Left, l)
      };
    let r =
      switch (sort(r, e)) {
      | `Exp => r
      | _ => next_exp_path(Right, r)
      };
    (l, r);
  };

  [@warning "-27"]
  let insert_tiles =
      (target: t, tiles: HExp.inner_tiles, e: HExp.t): option(HExp.t) =>
    failwith("unimplemented");

  let rec restructure =
          (
            ~place_cursor: [ | `Left | `Right]=`Left,
            (l, r): selection,
            target: t,
            e: HExp.t,
          )
          : option((t, HExp.t)) =>
    OptUtil.Syntax.(
      if (compare(l, r) > 0) {
        restructure(~place_cursor, (r, l), target, e);
      } else if (compare(l, target) < 0 && compare(target, r) < 0) {
        None;
      } else {
        // l before r
        // target before l or r before target
        switch (l, r, target) {
        | (([_, ..._], _), ([], _), ([], _))
        | (([], _), ([_, ..._], _), ([_, ..._], _)) =>
          restructure(~place_cursor=`Right, (r, target), l, e)
        | (([], _), ([_, ..._], _), ([], _))
        | (([_, ..._], _), ([], _), ([_, ..._], _)) =>
          restructure(~place_cursor=`Right, (target, l), r, e)
        | (([], j_l), ([], j_r), ([], j_target)) =>
          let* (removed, e) = remove_tiles((l, r), e);
          let j_insert = j_target <= j_l ? j_target : j_target - (j_r - j_l);
          let+ e = insert_tiles(([], j_insert), removed, e);
          let j =
            switch (place_cursor) {
            | `Left => j_insert
            | `Right => j_insert + j_r - j_l
            };
          (([], j), e);
        | (
            ([], j_l),
            ([], j_r),
            ([(tile_step, child_step), ...steps], j_target),
          ) =>
          let* (removed, e) = remove_tiles((l, r), e);
          let insert_steps = {
            let tile_step =
              tile_step < j_l ? tile_step : tile_step - (j_r - j_l);
            [(tile_step, child_step), ...steps];
          };
          let+ e = insert_tiles((insert_steps, j_target), removed, e);
          let j =
            switch (place_cursor) {
            | `Left => j_target
            | `Right => j_target + (j_r - j_l)
            };
          ((insert_steps, j), e);
        | (
            ([two_step_l, ..._], j_l),
            ([two_step_r, ..._], j_r),
            ([], j_target),
          ) =>
          if (two_step_l != two_step_r) {
            None;
          } else {
            let* (removed, e) = remove_tiles((l, r), e);
            let+ e = insert_tiles(target, removed, e);
            let j =
              switch (place_cursor) {
              | `Left => j_target
              | `Right => j_target + (j_r - j_l)
              };
            (([], j), e);
          }
        | (
            ([two_step_l, ...steps_l], j_l),
            ([two_step_r, ...steps_r], j_r),
            ([two_step_t, ...steps_t] as steps_target, j_target),
          ) =>
          if (two_step_l == two_step_r && two_step_r == two_step_t) {
            let l = (steps_l, j_l);
            let r = (steps_r, j_r);
            let target = (steps_t, j_target);
            switch (unzip(two_step_l, (e, None))) {
            | `Exp(e, zrest) =>
              let+ (path, e) =
                restructure(~place_cursor, (l, r), target, e);
              let (_, `Exp(e, _)) = zip_ztile(e, Option.get(zrest));
              (cons(two_step_l, path), e);
            | `Pat(p, zrest) =>
              let+ (path, p) =
                Pat.restructure(~place_cursor, (l, r), target, p);
              switch (Pat.zip_ztile(p, Option.get(zrest))) {
              | (_, `Pat(_)) =>
                failwith("unzipping and rezipping changed sort")
              | (_, `Exp(e, _)) => (cons(two_step_l, path), e)
              };
            };
          } else if (two_step_l == two_step_r) {
            let* (removed, e) = remove_tiles((l, r), e);
            let+ e = insert_tiles(target, removed, e);
            let j =
              switch (place_cursor) {
              | `Left => j_target
              | `Right => j_target + (j_r - j_l)
              };
            ((steps_target, j), e);
          } else if (two_step_t == two_step_l) {
            restructure(~place_cursor=`Right, (r, target), l, e);
          } else if (two_step_r == two_step_t) {
            restructure(~place_cursor=`Right, (target, l), r, e);
          } else {
            None;
          }
        };
      }
    );
};
