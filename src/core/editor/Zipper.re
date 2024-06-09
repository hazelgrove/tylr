open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;
open Stds;

module Caret = {
  include Caret;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Caret.t(unit);
  let mk = hand => mk(hand, ());
  let focus = focus();
};
// module Selection = {
//   include Selection;
//   [@deriving (show({with_path: false}), sexp, yojson)]
//   type t = Selection.t(Zigg.t);
// };
module Cursor = {
  include Cursor;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Cursor.t(Caret.t, Selection.t(Zigg.t));
};

// todo: document potential same-id token on either side of caret
// l|et x = 1 in x + 1
[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  cur: Cursor.t,
  ctx: Ctx.t,
};

let mk = (~cur=Cursor.point(Caret.focus), ctx) => {cur, ctx};

let unroll = (~ctx=Ctx.empty, side: Dir.t, cell: Cell.t) => {
  let f_open =
    side == L ? ([], Slope.Up.unroll(cell)) : (Slope.Dn.unroll(cell), []);
  Ctx.map_hd(Frame.Open.cat(f_open), ctx);
};
let mk_unroll = (~ctx=Ctx.empty, side: Dir.t, cell: Cell.t) =>
  mk(unroll(side, cell, ~ctx));

// assumes normalized cursor
let rec unzip = (~ctx=Ctx.empty, cell: Cell.t) => {
  open Options.Syntax;
  let len = Option.map(Meld.length, cell.meld) |> Option.value(~default=0);
  let* hd = Path.Cursor.hd(~len, cell.marks.cursor);
  switch (hd) {
  | Error(Point(_) as cur) =>
    assert(Option.is_none(cell.meld));
    Some(mk(~cur, ctx));
  | Error(Select(range)) =>
    let m = Options.get_exn(Marks.Invalid, cell.meld);
    Some(unzip_select(range, m, ~ctx));
  | Ok(step) =>
    let m = Options.get_exn(Marks.Invalid, cell.meld);
    switch (Meld.unzip(step, m)) {
    | Loop((pre, cell, suf)) => unzip(~ctx=Ctx.add((pre, suf), ctx), cell)
    | Link((pre, tok, suf)) =>
      let (l, cur, r) = Options.get_exn(Marks.Invalid, Token.unzip(tok));
      let mk = mk(~cur=Cursor.map(Fun.id, Selection.map(Zigg.of_tok), cur));
      switch (l, r) {
      | (None, None) => failwith("todo")
      | (None, Some(r)) =>
        let (cell, pre) = Chain.uncons(pre);
        let suf = Chain.Affix.cons(r, suf);
        let ctx = Ctx.add((pre, suf), ctx);
        Some(mk(unroll(R, cell, ~ctx)));
      | (Some(l), None) =>
        let pre = Chain.Affix.cons(l, pre);
        let (cell, suf) = Chain.uncons(suf);
        let ctx = Ctx.add((pre, suf), ctx);
        Some(mk(unroll(L, cell, ~ctx)));
      | (Some(l), Some(r)) =>
        let pre = Chain.Affix.cons(l, pre);
        let suf = Chain.Affix.cons(r, suf);
        Some(mk(Ctx.add((pre, suf), ctx)));
      };
    };
  };
}
and unzip_select =
    (~ctx=Ctx.empty, sel: Selection.t(Step.Range.t), meld: Meld.t) => {
  let get = Options.get_exn(Marks.Invalid);
  let (l, r) = sel.range;
  let (pre, top, suf) = Meld.split_subwald(l, r, meld);
  let ((pre_dn, pre_up), top) =
    if (l mod 2 == 0) {
      // l points to cell hd_pre
      let (hd_pre, tl_pre) = Chain.uncons(pre);
      let ctx_pre = Ctx.unit((Option.to_list(Terr.mk'(tl_pre)), []));
      let z = get(unzip(hd_pre, ~ctx=ctx_pre));
      (Ctx.flatten(z.ctx), top);
    } else {
      // split expected to succeed given normalized cursor
      let (hd_top_l, _, hd_top_r) = Token.split_caret(Wald.hd(top));
      let (cs_pre, ts_pre) = pre;
      let pre = ([Terr.mk([hd_top_l, ...ts_pre], cs_pre)], []);
      (pre, Wald.put_hd(hd_top_r, top));
    };
  let (top, (suf_dn, suf_up)) =
    if (r mod 2 == 0) {
      // r points to cell hd_suf
      let (hd_suf, tl_suf) = Chain.uncons(suf);
      let ctx_suf = Ctx.unit(([], Option.to_list(Terr.mk'(tl_suf))));
      let z = get(unzip(hd_suf, ~ctx=ctx_suf));
      (top, Ctx.flatten(z.ctx));
    } else {
      // split expected to succeed given normalized cursor
      let (ft_top_l, _, ft_top_r) = Token.split_caret(Wald.ft(top));
      let (cs_suf, ts_suf) = suf;
      let suf = ([], [Terr.mk([ft_top_r, ...ts_suf], cs_suf)]);
      (Wald.put_ft(ft_top_l, top), suf);
    };
  let zigg = Zigg.mk(~up=pre_up, top, ~dn=suf_dn);
  let ctx = Ctx.map_hd(Frame.Open.cat((pre_dn, suf_up)), ctx);
  mk(~cur=Select({focus: sel.focus, range: zigg}), ctx);
};

let zip_lt = (zipped: Cell.t, r: Terr.L.t) =>
  Cell.put(M(zipped, r.wald, r.cell));
let zip_gt = (l: Terr.R.t, zipped: Cell.t) =>
  Cell.put(M(l.cell, Wald.rev(l.wald), zipped));
let zip_eq = (l: Terr.R.t, zipped: Cell.t, r: Terr.L.t) => {
  let w = Wald.zip_cell(l.wald, zipped, r.wald);
  Cell.put(Meld.mk(~l=l.cell, w, ~r=r.cell));
};

let step_zip_open = ((dn, up): Frame.Open.t, zipped: Cell.t) =>
  switch (dn, up) {
  | ([], []) => None
  | ([], [hd, ...tl]) => Some((zip_lt(zipped, hd), (dn, tl)))
  | ([hd, ...tl], []) => Some((zip_gt(hd, zipped), (tl, up)))
  | ([hd_dn, ..._], [hd_up, ...tl_up])
      when Melder.lt(hd_dn.wald, hd_up.wald) =>
    Some((zip_lt(zipped, hd_up), (dn, tl_up)))
  | ([hd_dn, ...tl_dn], [hd_up, ..._])
      when Melder.gt(hd_dn.wald, hd_up.wald) =>
    Some((zip_gt(hd_dn, zipped), (tl_dn, up)))
  | ([l, ...dn], [r, ...up]) =>
    let caret = Cell.is_caret(zipped);
    switch (Wald.zip_hds(~from=L, l.wald, ~caret?, r.wald)) {
    | Some(w) => Some((Cell.put(M(l.cell, w, r.cell)), (dn, up)))
    | None =>
      assert(Melder.eq(l.wald, r.wald));
      Some((zip_eq(l, zipped, r), (dn, up)));
    };
  };
let step_zip = (ctx: Ctx.t, zipped: Cell.t) =>
  switch (Ctx.unlink(ctx)) {
  | Error(open_) =>
    Option.map(Tuples.map_snd(Ctx.unit), step_zip_open(open_, zipped))
  | Ok((open_, (l, r), ctx)) =>
    switch (step_zip_open(open_, zipped)) {
    | Some((zipped, open_)) =>
      Some((zipped, Ctx.link(~open_, (l, r), ctx)))
    | None => Some((zip_eq(l, zipped, r), ctx))
    }
  };

let rec zip_open = ((dn, up): Frame.Open.t, zipped: Cell.t) =>
  switch (step_zip_open((dn, up), zipped)) {
  | None => zipped
  | Some((zipped, rest)) => zip_open(rest, zipped)
  };
let zip_closed = ((l, r): Frame.Closed.t, zipped: Cell.t) =>
  zip_eq(l, zipped, r);

let zip = (~save_cursor=false, z: t) =>
  z.ctx
  |> (
    switch (z.cur) {
    | Point(_) => Fun.id
    | Select({focus, range: zigg}) =>
      let fill = save_cursor ? Cell.point(Anchor) : Cell.empty;
      Ctx.push_zigg(~onto=Dir.toggle(focus), zigg, ~fill);
    }
  )
  |> Ctx.fold(
       open_ =>
         zip_open(open_, save_cursor ? Cell.point(Focus) : Cell.empty),
       (zipped, closed, open_) =>
         zipped |> zip_closed(closed) |> zip_open(open_),
     );

// tries zipping and unzipping to cursor
let load_cursor = (z: t) => unzip(zip(z)) /*   */;

// let focused_term = (z: t): option((Cell.t, Ctx.t)) =>
//   switch (z.cur) {
//   | Select(_)
//   | Point(Anchor) => None
//   | Point(Focus) =>
