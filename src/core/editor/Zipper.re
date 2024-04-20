open Util;

// todo: document potential same-id token on either side of caret
// l|et x = 1 in x + 1
[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  foc: Focus.t,
  ctx: Ctx.t,
};

let mk = (~foc=Focus.Point, ctx) => {foc, ctx};

let init = failwith("todo: zipper init");

let unselect = (~toward=?, z: t) =>
  switch (z.foc) {
  | Point => z
  | Select(d, sel) =>
    let onto = Dir.toggle(Option.value(toward, ~default=d));
    mk(Melder.Ctx.push_zigg(~onto, sel, z.ctx));
  };

let unroll_cell = (~ctx=Ctx.empty, side: Dir.t, cell: Cell.t) => {
  let f_open =
    side == L
      ? ([], Melder.Slope.Up.unroll(cell))
      : (Melder.Slope.Dn.unroll(cell), []);
  Ctx.map_fst(Frame.Open.cat(f_open), ctx);
};
let unzip_cell =
    (~ctx=Ctx.empty, n: Path.Cell.Idx.t, cell: Cell.t): (Cell.t, Ctx.t) => {
  let M(l, w, r) = Cell.get(cell) |> OptUtil.get_or_raise(Path.Invalid);
  if (n == 0) {
    let terr = Terr.{wald: w, cell: r};
    let ctx = Ctx.map_fst(Frame.Open.cat(([], [terr])), ctx);
    (l, ctx);
  } else if (n == Wald.length(w)) {
    let terr = Terr.{cell: l, wald: Wald.rev(w)};
    let ctx = Ctx.map_fst(Frame.Open.cat(([terr], [])), ctx);
    (r, ctx);
  } else {
    let (pre, cell, suf) = Wald.unzip_cell(n - 1, w);
    let terrs = Terr.({cell: l, wald: pre}, {wald: suf, cell: r});
    (cell, Ctx.link(terrs, ctx));
  };
};
let rec unzip_point = (~ctx=Ctx.empty, p: Path.Point.Idx.t, cell: Cell.t) =>
  switch (p) {
  | End(side) => unroll_cell(~ctx, side, cell)
  | Tok(i, j) =>
    let M(l, w, r) = Cell.get(cell) |> OptUtil.get_or_raise(Path.Invalid);
    let (pre, tok, suf) = Wald.unzip_tok(i, w);
    switch (Token.split(j, tok)) {
    | Error(side) =>
      // normalize to overlapping cursor position in neighboring cell
      let n = Dir.pick(side, (i - 1, i));
      let (cell, ctx) = unzip_cell(~ctx, n, cell);
      unzip_point(~ctx, End(Dir.toggle(side)), cell);
    | Ok((tok_l, tok_r)) =>
      let l = Terr.{cell: l, wald: Wald.zip_tok(~suf=pre, tok_l)};
      let r = Terr.{wald: Wald.zip_tok(tok_r, ~suf), cell: r};
      Ctx.link((l, r), ctx);
    };
  };

// let normalize_cursor = (c: Cell) =>
//   switch (c.marks.cursor) {
//   | None
//   | Some(Point(_)) => c
//   | Some(Select(l, r)) =>
//     switch (Path.Point.(uncons(l), uncons(r))) {
//     | (Some((hd_l, tl_l)), Some((hd_r, tl_r))) when hd_l == hd_r =>

//     }
//   };

// unzips assuming range path normalized and common prefix trimmed
let unzip_range = (~ctx=Ctx.empty, (d, (l, r)): Path.Range.t, c: Cell.t) => {
  let m = Cell.get(c) |> OptUtil.get_or_raise(Path.Invalid);
  let (uc_l, uc_r) = Path.Point.(uncons(l), uncons(r));
  let n_l =
    switch (uc_l) {
    | Ok((hd_l, _)) => hd_l + 1
    | Error(End(L)) => 0
    | Error(End(R)) => failwith("expected normalized cursor")
    | Error(Tok(n, _)) => n
    };
  let n_r =
    switch (uc_r) {
    | Ok((hd_r, _)) => hd_r
    | Error(End(L)) => failwith("expected normalized cursor")
    | Error(End(R)) => Meld.length(m)
    | Error(Tok(n, _)) => n
    };
  let (pre, top, suf) = Meld.split_subwald(n_l, n_r, m);
  let (hd_pre: Cell.t, tl_pre) = Chain.split_hd(pre);
  let ((pre_dn, pre_up), top) =
    switch (uc_l) {
    | Ok((_, tl_l)) =>
      let ctx_pre = Ctx.unit((Option.to_list(Terr.mk(tl_pre)), []));
      // todo: fix so that unzip_point takes path.point.t
      (Ctx.flatten(unzip_point(tl_l, hd_pre, ~ctx=ctx_pre)), top);
    | Error(End(L)) =>
      let ctx_pre = Ctx.unit((Option.to_list(Terr.mk(tl_pre)), []));
      (unroll_cell(L, hd_pre, ~ctx=ctx_pre), top);
    | Error(End(R)) => failwith("expected normalized cursor")
    | Error(Tok(_, j)) =>
      let hd_top = Wald.hd(top);
      switch (Token.split(j, hd_top)) {
      | Error(_) => failwith("expected normalized cursor")
      | Ok((hd_top_l, hd_top_r)) =>
        let (cs_pre, ts_pre) = pre;
        let pre = ([Terr.mk([hd_top_l, ...ts_pre], cs_pre)], []);
        (pre, Wald.put_hd(hd_top_r, top));
      };
    };
  let (hd_suf: Cell.t, tl_suf) = Chain.split_hd(suf);
  let (top, (suf_dn, suf_up)) =
    switch (uc_r) {
    | Ok((_, tl_r)) =>
      let ctx_suf = Ctx.unit(([], Option.to_list(Terr.mk(tl_suf))));
      // todo: fix so that unzip_point takes path.point.t
      (Ctx.flatten(unzip_point(tl_r, hd_suf, ~ctx=ctx_suf)), top);
    | Error(End(L)) => failwith("expected normalized cursor")
    | Error(End(R)) =>
      let ctx_suf = Ctx.unit(([], Option.to_list(Terr.mk(tl_suf))));
      (unroll_cell(R, hd_suf, ~ctx=ctx_suf), top);
    | Error(Tok(_, j)) =>
      let ft_top = Wald.ft(top);
      switch (Token.split(j, hd_top)) {
      | Error(_) => failwith("expected normalized cursor")
      | Ok((ft_top_l, ft_top_r)) =>
        let (cs_suf, ts_suf) = suf;
        let suf = ([Terr.mk([ft_top_l, ...ts_suf], cs_suf)], []);
        (suf, Wald.put_ft(ft_top_r, top));
      };
    };
  let zigg = Zigg.mk(~up=pre_up, top, ~dn=suf_dn);
  let ctx = Ctx.map_fst(Frame.Open.cat((pre_dn, suf_up)), ctx);
  Zipper.mk(~foc=Select(d, zigg), ctx);
};

let unzip_cursor = (c: Cell.t) => {
  let rec go = (~ctx=Ctx.empty, c: Cell.t) =>
    switch (c.marks.cursor) {
    | None => unroll_cell(L, c, ~ctx)
    | Some(cur) =>
      switch (Path.Cursor.uncons(cur)) {
      | Some((n, cur)) =>
        let (c, ctx) = unzip_cell(n, c, ~ctx);
        go(c, ~ctx);
      | None =>
        switch (cur) {
        | Point((_, p)) => unzip_point(p, c, ~ctx)
        | Select(rng) => unzip_range(rng, c, ~ctx)
        }
      }
    };
  go(normalize_cursor(c));
};

let zip_closed = ((l, r): Frame.Closed.t, zipped: Cell.t) => {
  let w = Wald.zip_cell(l.wald, zipped, r.wald);
  Cell.put(Meld.mk(~l=l.cell, w, ~r=r.cell));
};
let rec zip_open = ((dn, up): Frame.Open.t, zipped: Cell.t) => {
  let get =
    fun
    | [zipped] => zipped
    | _ => failwith("bug: broken multiplicity invariant");
  switch (dn, up) {
  | ([], []) => zipped
  | ([], [_, ..._]) => get(Melder.Slope.Up.roll(~fill=[zipped], up))
  | ([_, ..._], []) => get(Melder.Slope.Dn.roll(dn, ~fill=[zipped]))
  | ([l, ..._] as dn, [r, ..._]) when Melder.Wald.lt(l.wald, r.wald) =>
    Cell.put(Meld.mk(~l=zipped, r.wald, ~r=r.cell)) |> zip_open((dn, up))
  | ([l, ...dn], [r, ..._] as up) when Melder.Wald.gt(l.wald, r.wald) =>
    Cell.put(Meld.mk(~l=l.cell, l.wald, ~r=zipped)) |> zip_open((dn, up))
  | ([l, ...dn], [r, ...up]) =>
    assert(Melder.Wald.eq(l.wald, r.wald));
    zipped |> zip_closed((l, r)) |> zip_open((dn, up));
  };
};
let zip = (z: t) =>
  z.ctx
  |> Ctx.fold(
       open_ => zip_open(open_, Cell.Space.cursor),
       (zipped, closed, open_) =>
         zipped |> zip_closed(closed) |> zip_open(open_),
     );

let move_to_cursor = (z: t) => unzip(zip(z));
