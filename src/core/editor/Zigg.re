// L2R: up top dn
[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  up: Slope.Up.t,
  top: Wald.t,
  dn: Slope.Dn.t,
};

let mk = (~up=Slope.empty, ~dn=Slope.empty, top) => {up, top, dn};
let of_tok = tok => mk(Wald.of_tok(tok));

let orient = (d: Dir.t, {up, top, dn}: t) => {
  let (s_d, s_b) = Dir.order(d, (up, dn));
  let top = Dir.pick(d, (Fun.id, Wald.rev), top);
  (s_d, top, s_b);
};
let unorient = (d: Dir.t, (s_d, top, s_b)) => {
  let (up, dn) = Dir.order(d, (s_d, s_b));
  let top = Dir.pick(d, (Fun.id, Wald.rev), top);
  mk(~up, top, ~dn);
};

// let x = (1 + [a + b ? c / d : e * f] + 3) + 4 * 5 in x + 1

// stepwell
// .                                                         .
// let x = ------------------------------------------in x + 1
//         .                                [+ 4, * 5]
//         (-------------------------------)
//         [1 +]                       [+ 3]

// selection
//                     ? c / d :
//                 + b           e *
//               a                   f

// selection:
//   focus: L
//   ziggurat: Some({ up: [a, + b], top: "? c / d :", dn: [e *, f] })
// stepwell:
//   slopes: ([1, +], [+, 3])
//   bridge: ( "(" , ")" )

let map_top = (f, zigg) => {...zigg, top: f(zigg.top)};
// let put_top = (top, zigg) => {...zigg, top: Some(top)};

let map_up = (f, zigg) => {...zigg, up: f(zigg.up)};
let put_up = up => map_up(_ => up);

let map_dn = (f, zigg) => {...zigg, dn: f(zigg.dn)};
let put_dn = dn => map_dn(_ => dn);

// let unroll = (c: Cell.t) => {
//   open Stds.Options.Syntax;
//   let+ M(l, top, r) = Cell.get(c);
//   let (up, dn) = Slope.(Up.unroll(l), Dn.unroll(r));
//   {up, top, dn};
// };
let of_dn = dn =>
  Stds.Lists.Framed.ft(dn)
  |> Option.map(((dn, t: Terr.t)) =>
       {
         up: Slope.Up.unroll(t.cell),
         top: Wald.rev(t.wald),
         dn: List.rev(dn),
       }
     );
let of_up = up =>
  Stds.Lists.Framed.ft(up)
  |> Option.map(((up, t: Terr.t)) =>
       {
         up: List.rev(up),
         top: Wald.rev(t.wald),
         dn: Slope.Up.unroll(t.cell),
       }
     );

let extend = (~side as d: Dir.t, tl: Chain.Affix.t(_), zigg) => {
  let (s_d, top, s_b) = orient(d, zigg);
  let (s_d, top) =
    switch (s_d) {
    | [] => (s_d, Wald.extend(tl, top))
    | [hd, ...rest] => ([Terr.extend(tl, hd), ...rest], top)
    };
  unorient(d, (s_d, top, s_b));
};

let push =
    (~side as d: Dir.t, t: Token.t, ~fill=Cell.empty, zigg: t)
    : Result.t(t, Slope.t) => {
  let b = Dir.toggle(d);
  let (s_d, top, s_b) = orient(d, zigg);
  // let stack = Melder.Stack.mk(~slope=s_d, Node(top));
  switch (
    Melder.push(~onto=b, t, ~fill, s_d, ~bound=Node(Terr.of_wald(top)))
  ) {
  | Some(Eq(top)) => Ok(unorient(d, ([], top.wald, s_b)))
  | Some(Neq(s_d)) => Ok(unorient(d, (s_d, top, s_b)))
  | None => Error(s_b @ [Melder.complete_wald(~side=d, ~fill, top)])
  };
};
// let push = (~side: Dir.t, tok: Token.t) =>
//   push_wald(~side, Wald.of_tok(tok));
let push_fail = (~side: Dir.t, tok, zigg) =>
  push(~side, tok, zigg) |> Stds.Result.get_fail("bug: failed push");

let push_wald = (~side: Dir.t, w: Wald.t, ~fill=Cell.empty, zigg: t) => {
  let (hd, tl) = Wald.uncons(w);
  push(~side, hd, ~fill, zigg) |> Stds.Result.map(~f=extend(~side, tl));
};

let pull = (~side as d: Dir.t, zigg: t): (Token.t, option(t)) => {
  let b = Dir.toggle(d);
  let (s_d, top, s_b) = orient(d, zigg);
  switch (Slope.pull(~from=b, s_d)) {
  | Some((tok, s_d)) => (tok, Some(unorient(d, (s_d, top, s_b))))
  | None =>
    let (tok, rest) = Wald.uncons(top);
    switch (rest) {
    | ([], _) => (tok, Dir.pick(d, (of_up, of_dn), s_b))
    | ([c, ...cs], ts) =>
      let s_d = Slope.unroll(~from=b, c);
      (tok, Some(unorient(d, (s_d, Wald.mk(ts, cs), s_b))));
    };
  };
};

let grow = (~side: Dir.t, tok: Token.t, zigg: t) =>
  switch (push(~side, tok, zigg)) {
  | Ok(zigg) => zigg
  | Error(s_b) => unorient(side, ([], Wald.of_tok(tok), s_b))
  };

let rec take_leq = (zigg: t, ~fill=Cell.empty, suf: Slope.Up.t) =>
  switch (suf) {
  | [] => ([], suf)
  | [hd, ...tl] =>
    switch (push_wald(~side=R, hd.wald, ~fill, zigg)) {
    | Error(_) => ([], suf)
    | Ok(zigg) =>
      let (leq, gt) = take_leq(zigg, ~fill=hd.cell, tl);
      ([hd, ...leq], gt);
    }
  };
let rec take_geq = (pre: Slope.Dn.t, ~fill=Cell.empty, zigg: t) =>
  switch (pre) {
  | [] => (pre, [])
  | [hd, ...tl] =>
    switch (push_wald(~side=L, hd.wald, ~fill, zigg)) {
    | Error(_) => (pre, [])
    | Ok(zigg) =>
      let (lt, geq) = take_geq(tl, ~fill=hd.cell, zigg);
      (lt, [hd, ...geq]);
    }
  };
