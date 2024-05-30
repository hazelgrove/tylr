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

let unroll = (~ctx=Ctx.empty, side: Dir.t, cell: Cell.t) => {
  let f_open =
    side == L
      ? ([], Melder.Slope.Up.unroll(cell))
      : (Melder.Slope.Dn.unroll(cell), []);
  Ctx.map_hd(Frame.Open.cat(f_open), ctx);
};
let mk_unroll = (~ctx=Ctx.empty, side: Dir.t, cell: Cell.t) =>
  mk(unroll(side, cell, ~ctx));

let unzip_point = (~ctx=Ctx.empty, p: Path.Point.t, m: Meld.t) =>
  switch (p.path) {
  | [] => mk(unroll(L, Cell.put(m), ~ctx))
  | [hd, ...tl] =>
    let (pre, tok, suf) = Meld.unzip_tok(hd, m);
    let j = Base.List.hd(tl) |> Option.value(~default=0);
    switch (Token.unzip(j, tok)) {
    | Ok((l, r)) =>
      let pre = Chain.Affix.cons(l, pre);
      let suf = Chain.Affix.cons(r, suf);
      mk(Ctx.cons((pre, suf), ctx));
    | Error(L) =>
      let (cell, pre) = Chain.split_hd(pre);
      let suf = Chain.Affix.cons(tok, suf);
      let ctx = Ctx.cons((pre, suf), ctx);
      mk(unroll(R, cell, ~ctx));
    | Error(R) =>
      let pre = Chain.Affix.cons(tok, pre);
      let (cell, suf) = Chain.split_hd(suf);
      let ctx = Ctx.cons((pre, suf), ctx);
      mk(unroll(L, cell, ~ctx));
    };
  };

let rec unzip = (~ctx=Ctx.empty, cell: Cell.t) => {
  let (cursor, meld) = Cell.get_cur(cell);
  switch (meld) {
  | None => Some(mk(ctx))
  | Some(m) =>
    Option.bind(
      cursor,
      fun
      | Here(Point(p)) => Some(unzip_point(p, m, ~ctx))
      | Here(Select(sel)) => Some(unzip_select(sel, m, ~ctx))
      | There(step) => {
          let (pre, cell, suf) = Meld.unzip_cell(step, m);
          unzip(~ctx=Ctx.cons((pre, suf), ctx), cell);
        },
    )
  };
}
and unzip_select = (~ctx=Ctx.empty, sel: Path.Select.t, meld: Meld.t) => {
  let (d, (l, r)) = Path.Select.order(sel);
  let n_l = Base.List.hd(l) |> Option.value(~default=0);
  let n_r = Base.List.hd(r) |> Option.value(~default=Meld.length(meld) - 1);
  let (pre, top, suf) = Meld.split_subwald(n_l, n_r, meld);
  let ((pre_dn, pre_up), top) = {
    let (hd_pre, tl_pre) = Chain.split_hd(pre);
    let ctx_pre = Ctx.unit((Option.to_list(Terr.mk'(tl_pre)), []));
    switch (l) {
    | [] =>
      // selection covers left end of meld
      assert(Chain.Affix.is_empty(tl_pre));
      (Ctx.flatten(unroll(L, hd_pre, ~ctx=ctx_pre)), top);
    | [hd_l, ..._tl_l] when hd_l mod 2 == 0 =>
      // hd_l points to cell
      // (assuming tl_l already propagated into hd_pre)
      let z = Options.get_exn(Not_found, unzip(hd_pre, ~ctx=ctx_pre));
      (Ctx.flatten(z.ctx), top);
    | [_hd_l, ...tl_l] =>
      // hd_l points to token
      switch (tl_l) {
      | [] =>
        // no char index => left end of token
        (Ctx.flatten(unroll(R, hd_pre, ~ctx=ctx_pre)), top)
      | [j, ..._] =>
        // char index (ignore rest)
        let (hd_top_l, hd_top_r) =
          Token.unzip(j, Wald.hd(top))
          |> Result.get_fail("expected normalized cursor");
        let (cs_pre, ts_pre) = pre;
        let pre = ([Terr.mk([hd_top_l, ...ts_pre], cs_pre)], []);
        (pre, Wald.put_hd(hd_top_r, top));
      }
    };
  };
  let (top, (suf_dn, suf_up)) = {
    let (hd_suf, tl_suf) = Chain.split_hd(suf);
    let ctx_suf = Ctx.unit(([], Option.to_list(Terr.mk'(tl_suf))));
    switch (r) {
    | [] =>
      // selection covers right end of meld
      assert(Chain.Affix.is_empty(tl_suf));
      (top, Ctx.flatten(unroll(R, hd_suf, ~ctx=ctx_suf)));
    | [hd_r, ..._tl_r] when hd_r mod 2 == 0 =>
      // hd_r points to cell
      // (assuming tl_r already propagated into hd_suf)
      let z = Options.get_exn(Not_found, unzip(hd_suf, ~ctx=ctx_suf));
      (top, Ctx.flatten(z.ctx));
    | [_hd_r, ...tl_r] =>
      // hd_r points to token
      switch (tl_r) {
      | [] =>
        // no char index => right end of token
        (top, Ctx.flatten(unroll(L, hd_suf, ~ctx=ctx_suf)))
      | [j, ..._] =>
        // char index (ignore rest)
        let (ft_top_l, ft_top_r) =
          Token.unzip(j, Wald.ft(top))
          |> Result.get_fail("expected normalized cursor");
        let (cs_suf, ts_suf) = suf;
        let suf = ([], [Terr.mk([ft_top_r, ...ts_suf], cs_suf)]);
        (Wald.put_ft(ft_top_l, top), suf);
      }
    };
  };
  let zigg = Zigg.mk(~up=pre_up, top, ~dn=suf_dn);
  let ctx = Ctx.map_hd(Frame.Open.cat((pre_dn, suf_up)), ctx);
  mk(~cur=Select((d, zigg)), ctx);
};

let zip_closed = ((l, r): Frame.Closed.t, zipped: Cell.t) => {
  let w = Wald.zip_cell(l.wald, zipped, r.wald);
  Cell.put(Meld.mk(~l=l.cell, w, ~r=r.cell));
};
let rec zip_open = ((dn, up): Frame.Open.t, zipped: Cell.t) =>
  switch (dn, up) {
  | ([], []) => zipped
  | ([], [_, ..._]) =>
    Fill.hd(Melder.Slope.Up.roll(~fill=Fill.unit(zipped), up))
  | ([_, ..._], []) =>
    Fill.hd(Melder.Slope.Dn.roll(dn, ~fill=Fill.unit(zipped)))
  | ([hd_dn, ..._], [hd_up, ...tl_up])
      when Melder.Wald.lt(hd_dn.wald, hd_up.wald) =>
    Cell.put(Meld.mk(~l=zipped, hd_up.wald, ~r=hd_up.cell))
    |> zip_open((dn, tl_up))
  | ([hd_dn, ...tl_dn], [hd_up, ..._])
      when Melder.Wald.gt(hd_dn.wald, hd_up.wald) =>
    Cell.put(Meld.mk(~l=hd_dn.cell, Wald.rev(hd_dn.wald), ~r=zipped))
    |> zip_open((tl_dn, up))
  | ([l, ...dn], [r, ...up]) =>
    let cursor = Cell.is_point(zipped);
    switch (Wald.zip_hds(~from=L, l.wald, ~cursor?, r.wald)) {
    | Some(w) => Cell.put(M(l.cell, w, r.cell)) |> zip_open((dn, up))
    | None =>
      assert(Melder.Wald.eq(l.wald, r.wald));
      zipped |> zip_closed((l, r)) |> zip_open((dn, up));
    };
  };
let zip = (~save_cursor=false, z: t) =>
  z.ctx
  |> (
    switch (z.cur) {
    | Point () => Fun.id
    | Select((foc, zigg)) =>
      let fill =
        save_cursor ? Fill.unit(Cell.point(~foc=false, ())) : Fill.empty;
      Melder.Ctx.push_zigg(~onto=Dir.toggle(foc), zigg, ~fill);
    }
  )
  |> Ctx.fold(
       open_ => zip_open(open_, save_cursor ? Cell.point() : Cell.empty),
       (zipped, closed, open_) =>
         zipped |> zip_closed(closed) |> zip_open(open_),
     );

// tries zipping and unzipping to cursor
let load_cursor = (z: t) => unzip(zip(z));
