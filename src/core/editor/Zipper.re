open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;
open Stds;

exception Bug__lost_cursor;

module Caret = {
  include Caret;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Caret.t(unit);
  let mk = hand => mk(hand, ());
  // let focus = focus();
};
module Selection = {
  include Selection;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Selection.t(Zigg.t);
  let split_range = Fun.const(((), ()));
  let carets = carets(~split_range);
};
module Cur = Cursor;
module Cursor = {
  include Cur;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Cur.t(Caret.t, Selection.t);
  let flatten: t => _ =
    fun
    | Point(_) => []
    | Select({range, _}) => Zigg.flatten(range);
};

module Site = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Between
    | Within(Token.t);
  module Cursor = {
    type nonrec t = Cur.t(t, (t, t));
  };
};

// todo: document potential same-id token on either side of caret
// l|et x = 1 in x + 1
[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  cur: Cursor.t,
  ctx: Ctx.t,
};

let mk = (~cur=Cursor.point(Caret.focus()), ctx) => {cur, ctx};

let unroll = (~ctx=Ctx.empty, side: Dir.t, cell: Cell.t) => {
  let f_open =
    side == L ? ([], Slope.Up.unroll(cell)) : (Slope.Dn.unroll(cell), []);
  Ctx.map_hd(Frame.Open.cat(f_open), ctx);
};
let mk_unroll = (~ctx=Ctx.empty, side: Dir.t, cell: Cell.t) =>
  mk(unroll(side, cell, ~ctx));

let cursor_site = (z: t): (Site.Cursor.t, Ctx.t) =>
  switch (z.cur) {
  | Point(_) =>
    let (l, ctx) = Ctx.pull(~from=L, z.ctx);
    let (r, ctx) = Ctx.pull(~from=R, ctx);
    switch (l, r) {
    | (Node(l), Node(r)) when Token.merges(l, r) => (
        Cur.Point(Site.Within(l)),
        ctx,
      )
    | _ => (Cur.Point(Between), z.ctx)
    };
  | Select(sel) =>
    let (l, ctx) = {
      let face = Zigg.face(~side=L, sel.range);
      switch (Ctx.pull(~from=L, z.ctx)) {
      | (Node(l), rest) when Token.merges(l, face) => (
          Site.Within(face),
          rest,
        )
      | _ => (Between, z.ctx)
      };
    };
    let (r, ctx) = {
      let face = Zigg.face(sel.range, ~side=R);
      switch (Ctx.pull(~from=R, ctx)) {
      | (Node(r), rest) when Token.merges(face, r) => (
          Site.Within(face),
          rest,
        )
      | _ => (Between, ctx)
      };
    };
    (Cur.Select((l, r)), ctx);
  };

let rec unzip = (~ctx=Ctx.empty, cell: Cell.t) => {
  open Options.Syntax;
  let* hd = Option.map(Path.Cursor.hd, cell.marks.cursor);
  let m = Cell.get(cell);
  switch (hd) {
  | Error(Point(_) as cur) =>
    assert(Option.is_none(m));
    Some(mk(~cur, ctx));
  | Error(Select(range)) =>
    let m = Options.get_exn(Marks.Invalid, m);
    Some(unzip_select(range, m, ~ctx));
  | Ok(step) =>
    let m = Options.get_exn(Marks.Invalid, m);
    switch (Meld.unzip(step, m)) {
    | Loop((pre, cell, suf)) => unzip(~ctx=Ctx.add((pre, suf), ctx), cell)
    | Link((pre, tok, suf)) =>
      let map = Cursor.map(Fun.id, Selection.map(Zigg.of_tok));
      switch (Token.unzip(tok)) {
      // | (_, None, _) => go_l(Point(Caret.focus), tok)
      | (None, _, None) => failwith("todo: handle token paths")
      | (None, cur, Some(r)) =>
        let (cell, pre) = Chain.uncons(pre);
        let suf = Chain.Affix.cons(r, suf);
        let ctx = Ctx.add((pre, suf), ctx);
        Some(mk(~cur=map(cur), unroll(R, cell, ~ctx)));
      | (Some(l), cur, None) =>
        let pre = Chain.Affix.cons(l, pre);
        let (cell, suf) = Chain.uncons(suf);
        let ctx = Ctx.add((pre, suf), ctx);
        Some(mk(~cur=map(cur), unroll(L, cell, ~ctx)));
      | (Some(l), cur, Some(r)) =>
        let pre = Chain.Affix.cons(l, pre);
        let suf = Chain.Affix.cons(r, suf);
        Some(mk(~cur=map(cur), Ctx.add((pre, suf), ctx)));
      };
    };
  };
}
// assumes normalized cursor
and unzip_select = (~ctx=Ctx.empty, sel: Path.Selection.t, meld: Meld.t) => {
  let get = Options.get_exn(Marks.Invalid);
  let (l, r) = sel.range;
  let l = Path.hd(l) |> Path.Head.get(() => 0);
  let r = Path.hd(r) |> Path.Head.get(() => Meld.length(meld) - 1);
  let (pre, top, suf) =
    try(Meld.split_subwald(l, r, meld)) {
    | _ =>
      P.show("paths", Path.Selection.show(sel));
      P.show("meld", Meld.show(meld));
      failwith("failed split subwald");
    };
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

let unzip_exn = (~ctx=Ctx.empty, c: Cell.t) =>
  unzip(~ctx, c) |> Options.get_exn(Invalid_argument("Zipper.unzip_exn"));

let rec zip_neighbor = (~side: Dir.t, ~zipped: Cell.t, ctx: Ctx.t) => {
  open Options.Syntax;
  let* (rel, zipped, ctx) = Ctx.zip_step(~zipped, ctx);
  switch (rel) {
  | Eq () => return((zipped, ctx))
  | Neq(d) =>
    d == side ? return((zipped, ctx)) : zip_neighbor(~side, ~zipped, ctx)
  };
};

// zip up to the nearest containing cell.
// if zip_init(z) == (cell, ctx), then
//   Cell.is_point(cell)  iff  z points between tokens
let zip_init = (~save_cursor=false, z: t): (Cell.t, Ctx.t) =>
  switch (z.cur) {
  | Point(car) =>
    switch (Ctx.zip_toks(~save_cursor, z.ctx)) {
    // within token
    | Some((m, ctx)) => (Cell.put(m), ctx)
    | None =>
      // between tokens
      let zipped = save_cursor ? Cell.point(car.hand) : Cell.empty;
      (zipped, z.ctx);
    }
  | Select(sel) =>
    let (l, r) =
      save_cursor
        ? Cell.(empty, empty)
        : Selection.carets(sel)
          |> Tuples.map2(car =>
               Cell.caret(Caret.map(Fun.const(Path.empty), car))
             );
    let ((dn, up), tl) = Ctx.uncons(z.ctx);
    let (zigg, dn) = Zigg.take_ineq(~side=L, sel.range, ~fill=l, dn);
    let (zigg, up) = Zigg.take_ineq(~side=R, zigg, ~fill=r, up);
    (Cell.put(Zigg.roll(zigg)), Ctx.cons((dn, up), tl));
  };
let zip_indicated = (z: t): (Cell.t, Ctx.t) => {
  let (zipped, ctx) as init = zip_init(~save_cursor=true, z);
  let zipped_past_space = {
    open Options.Syntax;
    let* _ = Cell.is_caret(zipped);
    let* (rel, zipped, ctx) = Ctx.zip_step(~zipped, ctx);
    let/ () = Cell.Space.is_space(zipped) ? None : Some((zipped, ctx));
    let+ d = Rel.is_neq(rel);
    zip_neighbor(~side=Dir.toggle(d), ~zipped, ctx)
    |> Option.value(~default=(zipped, ctx));
  };
  Option.value(zipped_past_space, ~default=init);
};

let zip = (~save_cursor=false, z: t) => {
  let ctx =
    switch (z.cur) {
    | Point(_) => z.ctx
    | Select({focus, range: zigg}) =>
      let fill = save_cursor ? Cell.point(Anchor) : Cell.empty;
      Ctx.push_zigg(~onto=Dir.toggle(focus), zigg, ~fill, z.ctx);
    };
  let zipped = save_cursor ? Cell.point(Focus) : Cell.empty;
  Ctx.zip(ctx, ~zipped, ~save_cursor);
};

let path_of_ctx = (ctx: Ctx.t) => {
  let c = zip(~save_cursor=true, mk(ctx));
  let cur = Option.get(c.marks.cursor);
  let car = Option.get(Cursor.get_point(cur));
  car.path;
};

// tries zipping and unzipping to cursor
let load_cursor = (z: t) => unzip(zip(z));

let button = (z: t): t => {
  let (cell, ctx) = zip_init(~save_cursor=true, z);
  let (hd, tl) = Ctx.uncons(ctx);
  let cell = Frame.Open.zip(hd, ~zipped=cell);
  let ctx = Ctx.cons(Frame.Open.empty, tl);
  Option.get(unzip(cell, ~ctx));
};

let normalize = (~cell: Cell.t, path: Path.t): Path.t => {
  let cell = Cell.put_cursor(Point(Caret.focus(path)), cell);
  let zipped =
    unzip(cell)
    |> Options.get_exn(Invalid_argument("Cell.normalize"))
    |> zip(~save_cursor=true);
  let cur = Option.get(zipped.marks.cursor);
  let car = Option.get(Cursor.get_point(cur));
  car.path;
};
