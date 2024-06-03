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
    side == L
      ? ([], Melder.Slope.Up.unroll(cell))
      : (Melder.Slope.Dn.unroll(cell), []);
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
    | Loop((pre, cell, suf)) => unzip(~ctx=Ctx.cons((pre, suf), ctx), cell)
    | Link((pre, tok, suf)) =>
      let (l, cur, r) = Options.get_exn(Marks.Invalid, Token.unzip(tok));
      let mk = mk(~cur=Cursor.map(Fun.id, Selection.map(Zigg.of_tok), cur));
      switch (l, r) {
      | (None, None) => failwith("todo")
      | (None, Some(r)) =>
        let (cell, pre) = Chain.split_hd(pre);
        let suf = Chain.Affix.cons(r, suf);
        let ctx = Ctx.cons((pre, suf), ctx);
        Some(mk(unroll(R, cell, ~ctx)));
      | (Some(l), None) =>
        let pre = Chain.Affix.cons(l, pre);
        let (cell, suf) = Chain.split_hd(suf);
        let ctx = Ctx.cons((pre, suf), ctx);
        Some(mk(unroll(L, cell, ~ctx)));
      | (Some(l), Some(r)) =>
        let pre = Chain.Affix.cons(l, pre);
        let suf = Chain.Affix.cons(r, suf);
        Some(mk(Ctx.cons((pre, suf), ctx)));
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
      let (hd_pre, tl_pre) = Chain.split_hd(pre);
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
      let (hd_suf, tl_suf) = Chain.split_hd(suf);
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
    | Point(_) => Fun.id
    | Select({focus, range: zigg}) =>
      let fill = save_cursor ? Fill.unit(Cell.point(Anchor)) : Fill.empty;
      Melder.Ctx.push_zigg(~onto=Dir.toggle(focus), zigg, ~fill);
    }
  )
  |> Ctx.fold(
       open_ =>
         zip_open(open_, save_cursor ? Cell.point(Focus) : Cell.empty),
       (zipped, closed, open_) =>
         zipped |> zip_closed(closed) |> zip_open(open_),
     );

// tries zipping and unzipping to cursor
let load_cursor = (z: t) => unzip(zip(z));
