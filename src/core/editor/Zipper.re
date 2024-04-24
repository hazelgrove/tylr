open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;
open Util;

module Cursor = {
  include Cursor;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Cursor.t(unit, (Dir.t, Zigg.t));
};

// todo: document potential same-id token on either side of caret
// l|et x = 1 in x + 1
[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  cur: Cursor.t,
  ctx: Ctx.t,
};

let mk = (~cur=Cursor.point(), ctx) => {cur, ctx};

let init = failwith("todo: zipper init");

let unroll_cell = (~ctx=Ctx.empty, side: Dir.t, cell: Cell.t) => {
  let f_open =
    side == L
      ? ([], Melder.Slope.Up.unroll(cell))
      : (Melder.Slope.Dn.unroll(cell), []);
  Ctx.map_fst(Frame.Open.cat(f_open), ctx);
};

let rec unzip = (~ctx=Ctx.empty, cell: Cell.t) => {
  let (cursor, meld) = Cell.get_cur(cell);
  switch (meld) {
  | None => mk(ctx)
  | Some(M(l, w, r) as m) =>
    switch (cursor) {
    | None
    | Some(Here(Point(_))) => mk(unroll_cell(L, cell, ~ctx))
    | Some(Here(Select(sel))) => unzip_select(sel, m, ~ctx)
    | Some(There(step)) =>
      if (step == 0) {
        let terr = Terr.{wald: w, cell: r};
        let ctx = Ctx.map_fst(Frame.Open.cat(([], [terr])), ctx);
        unzip(l, ~ctx);
      } else if (step == Wald.length(w)) {
        let terr = Terr.{cell: l, wald: Wald.rev(w)};
        let ctx = Ctx.map_fst(Frame.Open.cat(([terr], [])), ctx);
        unzip(r, ~ctx);
      } else {
        let (pre, cell, suf) = Wald.unzip_cell(step - 1, w);
        let terrs = Terr.({cell: l, wald: pre}, {wald: suf, cell: r});
        unzip(cell, ~ctx=Ctx.link(terrs, ctx));
      }
    }
  };
}
and unzip_select = (~ctx=Ctx.empty, sel: Path.Select.t, meld: Meld.t) => {
  let (d, (l, r)) = Path.Select.order(sel);
  let n_l = ListUtil.hd_opt(l) |> Option.value(~default=0);
  let n_r =
    ListUtil.hd_opt(r) |> Option.value(~default=Meld.total_length(meld) - 1);
  let (pre, top, suf) = Meld.split_subwald(n_l, n_r, meld);
  let ((pre_dn, pre_up), top) = {
    let (hd_pre, tl_pre) = Chain.split_fst(pre);
    let ctx_pre = Ctx.unit((Option.to_list(Terr.mk'(tl_pre)), []));
    switch (l) {
    | [] =>
      // selection covers left end of meld
      assert(Chain.Affix.is_empty(tl_pre));
      (Ctx.flatten(unroll_cell(L, hd_pre, ~ctx=ctx_pre)), top);
    | [hd_l, ..._tl_l] when hd_l mod 2 == 0 =>
      // hd_l points to cell
      // (assuming tl_l already propagated into hd_pre)
      let z = unzip(hd_pre, ~ctx=ctx_pre);
      (Ctx.flatten(z.ctx), top);
    | [_hd_l, ...tl_l] =>
      // hd_l points to token
      switch (tl_l) {
      | [] =>
        // no char index => left end of token
        (Ctx.flatten(unroll_cell(R, hd_pre, ~ctx=ctx_pre)), top)
      | [j, ..._] =>
        // char index (ignore rest)
        let (hd_top_l, hd_top_r) =
          Token.split(j, Wald.hd(top))
          |> Result.get_or_fail("expected normalized cursor");
        let (cs_pre, ts_pre) = pre;
        let pre = ([Terr.mk([hd_top_l, ...ts_pre], cs_pre)], []);
        (pre, Wald.put_hd(hd_top_r, top));
      }
    };
  };
  let (top, (suf_dn, suf_up)) = {
    let (hd_suf, tl_suf) = Chain.split_fst(suf);
    let ctx_suf = Ctx.unit(([], Option.to_list(Terr.mk'(tl_suf))));
    switch (r) {
    | [] =>
      // selection covers right end of meld
      assert(Chain.Affix.is_empty(tl_suf));
      (top, Ctx.flatten(unroll_cell(R, hd_suf, ~ctx=ctx_suf)));
    | [hd_r, ..._tl_r] when hd_r mod 2 == 0 =>
      // hd_r points to cell
      // (assuming tl_r already propagated into hd_suf)
      let z = unzip(hd_suf, ~ctx=ctx_suf);
      (top, Ctx.flatten(z.ctx));
    | [_hd_r, ...tl_r] =>
      // hd_r points to token
      switch (tl_r) {
      | [] =>
        // no char index => right end of token
        (top, Ctx.flatten(unroll_cell(L, hd_suf, ~ctx=ctx_suf)))
      | [j, ..._] =>
        // char index (ignore rest)
        let (ft_top_l, ft_top_r) =
          Token.split(j, Wald.ft(top))
          |> Result.get_or_fail("expected normalized cursor");
        let (cs_suf, ts_suf) = suf;
        let suf = ([], [Terr.mk([ft_top_r, ...ts_suf], cs_suf)]);
        (Wald.put_ft(ft_top_l, top), suf);
      }
    };
  };
  let zigg = Zigg.mk(~up=pre_up, top, ~dn=suf_dn);
  let ctx = Ctx.map_fst(Frame.Open.cat((pre_dn, suf_up)), ctx);
  mk(~cur=Select((d, zigg)), ctx);
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
       open_ => zip_open(open_, Cell.point()),
       (zipped, closed, open_) =>
         zipped |> zip_closed(closed) |> zip_open(open_),
     );

let move_to_cursor = (z: t) => unzip(zip(z));
