open Stds;

include Chain;
[@deriving (show({with_path: false}), sexp, yojson)]
type t = Chain.t(Frame.Open.t, Frame.Closed.t);

let empty = Chain.unit(Frame.Open.empty);

let link = (~open_=Frame.Open.empty) => link(open_);

let fold = Chain.fold_left;
let fold_root = Chain.fold_right;

let face = (~side: Dir.t, ctx: t) => {
  open Options.Syntax;
  let/ () = Frame.Open.face(~side, hd(ctx));
  let+ (_, (l, r), _) = Result.to_option(unlink(ctx));
  Terr.face(Dir.pick(side, (l, r)));
};
let map_face = (~side: Dir.t, f: Token.t => Token.t, ctx: t) => {
  let (hd, tl) = Chain.uncons(ctx);
  switch (Frame.Open.map_face(~side, f, hd)) {
  | Some(hd) => Chain.cons(hd, tl)
  | None =>
    switch (unlink(ctx)) {
    | Error(_) => ctx
    | Ok((open_, closed, rest)) =>
      let closed = Frame.Closed.map_face(~side, f, closed);
      Chain.link(open_, closed, rest);
    }
  };
};

// let bound = (~side: Dir.t, ctx) =>
//   switch (unlink(ctx)) {
//   | Error(_) => Bound.Root
//   | Ok((_, closed, _)) => Node()
//   };

let flatten =
  Chain.fold_right(
    (open_, (l, r), acc) =>
      acc
      |> Frame.Open.cons(~onto=L, l)
      |> Frame.Open.cons(~onto=R, r)
      |> Frame.Open.cat(open_),
    Fun.id,
  );

// todo: rename
let add = ((pre, suf): (Meld.Affix.t, Meld.Affix.t)) =>
  switch (Terr.mk'(pre), Terr.mk'(suf)) {
  | (None, None) => Fun.id
  | (None, Some(r)) => map_hd(Frame.Open.cons(~onto=R, r))
  | (Some(l), None) => map_hd(Frame.Open.cons(~onto=L, l))
  | (Some(l), Some(r)) =>
    switch (Token.merge(Terr.hd(l), Terr.hd(r))) {
    | Some(_) => map_hd(((dn, up)) => ([l, ...dn], [r, ...up]))
    | None => link((l, r))
    }
  };

let pull_face = (~from: Dir.t, ctx: t): (Delim.t, t) => {
  let (hd, tl) = uncons(ctx);
  let (face, hd') = Frame.Open.pull(~from, hd);
  switch (face) {
  | Node(_) => (face, cons(hd', tl))
  | Root =>
    switch (tl) {
    | ([], _empty) => (face, cons(hd', tl))
    | ([c, ...cs], os) =>
      let (t, rest) = Frame.Closed.pull(~from, c);
      let ctx = map_hd(Frame.Open.cat(rest), mk(os, cs));
      (Node(t), ctx);
    }
  };
};

// let pull_faces = (ctx: t): (Frame.Faces.t, t) => {
//   let (hd, tl) = uncons(ctx);
//   let (faces, hd') = Frame.Open.pull_faces(hd);
//   switch (faces) {
//   | Within(_)
//   | Between(Node(_), Node(_)) => (faces, cons(hd', tl))
//   | Between(l, r) =>
//     switch (tl) {
//     | ([], _empty) => (faces, cons(hd', tl))
//     | ([closed, ...cs], os) =>
//       let rest = Chain.mk(os, cs);
//       let ((face_l, face_r), (rest_l, rest_r)) =
//         Frame.Closed.pull_faces(closed);
//       switch (l, r) {
//       | (Root, Root) =>
//         let face = Frame.Faces.Between(Node(face_l), Node(face_r));
//         let rest = Chain.map_hd(Frame.Open.cat((rest_l, rest_r)), rest);
//         (face, rest);
//       | (Node(_), _root) =>
//         let face = Frame.Faces.Between(l, Node(face_r));
//         let rest =
//           Chain.map_hd(Frame.Open.cat(([fst(closed)], rest_r)), rest);
//         (face, rest);
//       | (_root, Node(_)) =>
//         let face = Frame.Faces.Between(Node(face_l), r);
//         let rest =
//           Chain.map_hd(Frame.Open.cat((rest_l, [snd(closed)])), rest);
//         (face, rest);
//       };
//     }
//   };
// };

// let faces = (ctx: t) => fst(pull_faces(ctx));
// let face = (~side: Dir.t, ctx: t) => {
//   open Options.Syntax;
//   let/ () = Frame.Open.face(~side, hd(ctx));
//   let+ (_, (l, r), _) = Result.to_option(unlink(ctx));
//   Terr.face(Dir.pick(side, (l, r)));
// };
// let map_face = (~side: Dir.t, f: Token.t => Token.t, ctx: t) => {
//   let (hd, tl) = Chain.uncons(ctx);
//   switch (Frame.Open.map_face(~side, f, hd)) {
//   | Some(hd) => Chain.cons(hd, tl)
//   | None =>
//     switch (unlink(ctx)) {
//     | Error(_) => ctx
//     | Ok((open_, closed, rest)) =>
//       let closed = Frame.Closed.map_face(~side, f, closed);
//       Chain.link(open_, closed, rest);
//     }
//   };
// };

let rec pull = (~from as d: Dir.t, ctx: t): (Delim.t, t) =>
  switch (unlink(ctx)) {
  | Error(slopes) =>
    let (s_d, s_b) = Dir.order(d, slopes);
    let (delim, s_d) = Slope.pull(~from=d, s_d);
    (delim, unit(Dir.order(d, (s_d, s_b))));
  | Ok((slopes, terrs, ctx)) =>
    let (s_d, s_b) = Dir.order(d, slopes);
    let (delim, s_d) = Slope.pull(~from=d, s_d);
    switch (delim) {
    | Node(_) =>
      let open_ = Dir.order(d, (s_d, s_b));
      (delim, link(~open_, terrs, ctx));
    | Root =>
      let (t_d, t_b) = Dir.order(d, terrs);
      let slopes = Dir.order(d, ([t_d], s_b @ [t_b]));
      pull(~from=d, map_hd(Frame.Open.cat(slopes), ctx));
    };
  };
// let pull_opt = (~from, ctx) =>
//   switch (pull(~from, ctx)) {
//   | None => (None, ctx)
//   | Some((tok, ctx)) => (Some(tok), ctx)
//   };

module Tl = {
  include Chain.Affix;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Chain.Affix.t(Frame.Closed.t, Frame.Open.t);
  let bounds =
    fun
    | ([], _)
    | (_, []) => Bound.(Root, Root)
    | ([(l, r), ..._], _) => (Node(l), Node(r));
  let rest =
    fun
    | ([], _)
    | (_, []) => empty
    | ([_, ...cs], os) => Chain.mk(os, cs);
  // let tl = b => b |> Bound.map(snd) |> Bound.get(~root=empty);
  // let hd = b => b |> Bound.map(fst) |> Bound.split;
};

// let split_bound = (ctx: t): (Frame.Open.t, Bound.t) =>
//   switch (unlink(ctx)) {
//   | Error(open_) => (open_, Bound.Root)
//   | Ok((open_, closed, tl)) => (open_, Node((closed, tl)))
//   };
// let merge_bound = (open_: Frame.Open.t, bound: Bound.t) =>
//   switch (bound) {
//   | Root => unit(open_)
//   | Node((closed, ctx)) => link(open_, closed, ctx)
//   };

let extend = (~side: Dir.t, tl: Chain.Affix.t(Cell.t, Token.t)) =>
  map_hd(Frame.Open.extend(~side, tl));

let push_opt =
    (~onto as d: Dir.t, t: Token.t, ~fill=Cell.empty, ctx: t): option(t) => {
  open Options.Syntax;
  let (hd, tl) = uncons(ctx);
  let (s_d, s_b) = Dir.order(d, hd);
  let (b_d, b_b) = Dir.order(d, Tl.bounds(tl));
  let+ melded = Melder.push(~onto=d, t, ~fill, s_d, ~bound=b_d);
  switch (melded) {
  | Neq(s_d) =>
    let (dn, up) = Dir.order(d, (s_d, s_b));
    cons((dn, up), tl);
  | Eq(b_d) =>
    let s_b = Slope.cat(s_b, Bound.to_list(b_b));
    let open_ = Dir.order(d, ([b_d], s_b));
    map_hd(Frame.Open.cat(open_), Tl.rest(tl));
  };
};

let push = (~onto: Dir.t, tok: Token.t, ~fill=Cell.empty, ctx) => {
  let get = Options.get_exn(Invalid_argument("Ctx.push"));
  let pushed = get(push_opt(~onto, tok, ~fill, ctx));
  let (l, _, r) = Token.split(tok);
  switch (Dir.pick(onto, (r, l))) {
  | None => pushed
  | Some(_) => get(push_opt(~onto=Dir.toggle(onto), tok, pushed))
  };
};

// let push = (~onto: Dir.t, tok, ~fill=Cell.empty, ctx) =>
//   push_opt(~onto, tok, ~fill, ctx)
//   |> Options.get_exn(Invalid_argument("Ctx.push"));

// let push_face = (~onto: Dir.t, face: Frame.Face.t, ctx: t) =>
//   switch (face) {
//   | Out(Root) => ctx
//   | Out(Node(tok)) => ctx |> push(~onto, tok)
//   | In(tok) => ctx |> push(~onto=L, tok) |> push(~onto=R, tok)
//   };

let push_wald_opt = (~onto: Dir.t, w: Wald.t, ~fill=Cell.empty, ctx) => {
  let (hd, tl) = Wald.uncons(w);
  push_opt(~onto, hd, ~fill, ctx) |> Option.map(extend(~side=onto, tl));
};
let push_wald = (~onto: Dir.t, w: Wald.t, ~fill=Cell.empty, ctx) =>
  push_wald_opt(~onto, w, ~fill, ctx)
  |> Options.get_exn(Invalid_argument("Ctx.push_wald"));

let rec push_slope = (~onto: Dir.t, s: Slope.t, ~fill=Cell.empty, ctx: t) =>
  switch (s) {
  | [] => ctx
  | [hd, ...tl] =>
    let ctx = push_wald(~onto, hd.wald, ~fill, ctx);
    push_slope(~onto, tl, ~fill=hd.cell, ctx);
  };
let push_zigg = (~onto as d: Dir.t, zigg: Zigg.t, ~fill=Cell.empty, ctx: t) => {
  let (s_d, top, s_b) = Zigg.orient(d, zigg);
  let ctx = push_slope(~onto=d, s_d, ~fill, ctx);
  // need to propagate given fill if push_slope did not incorporate
  let fill = Stds.Lists.is_empty(s_d) ? fill : Cell.empty;
  let ctx = push_wald(~onto=d, top, ~fill, ctx);
  let rest = Dir.order(d, ([], s_b));
  map_hd(Frame.Open.cat(rest), ctx);
};

let zip_toks = (~save_cursor=false, ctx: t): option((Meld.t, t)) => {
  let (hd, tl) = uncons(ctx);
  Frame.Open.zip_toks(~save_cursor, hd)
  |> Option.map(Tuples.map_snd(hd => cons(hd, tl)));
};

let zip = (~save_cursor, ~zipped: Cell.t) =>
  fold(Frame.Open.zip(~zipped, ~save_cursor), (zipped, closed, open_) =>
    Frame.Open.zip(open_, ~zipped=Frame.Closed.zip(closed, ~zipped))
  );
let zip_step =
    (~zipped: Cell.t, ctx: t): option((Rel.t(unit, Dir.t), Cell.t, t)) =>
  switch (unlink(ctx)) {
  | Error(open_) =>
    // todo: review these save_cursor flags
    Frame.Open.zip_step(~save_cursor=true, ~zipped, open_)
    |> Option.map(((rel, zipped, open_)) => (rel, zipped, unit(open_)))
  | Ok((open_, (l, r), ctx)) =>
    switch (Frame.Open.zip_step(~save_cursor=true, ~zipped, open_)) {
    | None => Some((Eq(), Frame.zip_eq(l, zipped, r), ctx))
    | Some((rel, zipped, open_)) =>
      Some((rel, zipped, link(~open_, (l, r), ctx)))
    }
  };
