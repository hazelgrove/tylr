open Util;

[@deriving (show({with_path: false}), sexp, yojson, ord)]
type t('focus, 'atom) = ('focus, RCtx.t('atom));

// Root in output means one of the alternatives of r is null
let rec enter =
        (~ctx=RCtx.empty, ~from: Dir.t, r: Regex.t('a))
        : list(Bound.t(t('a, 'a))) => {
  let go = enter(~from);
  switch (r) {
  | Atom(a) => [Node((a, ctx))]
  | Star(r) => [Root, ...go(~ctx=[Star_, ...ctx], r)]
  | Alt(s) =>
    Lists.Framed.elems(s)
    |> List.concat_map(((r, (ls, rs))) =>
         go(~ctx=[Alt_(ls, rs), ...ctx], r)
       )
  | Seq(s) =>
    switch (from == L ? s : List.rev(s)) {
    | [] => [Root]
    | [hd, ...tl] =>
      hd
      |> go(~ctx=RCtx.push_s(~onto=Dir.toggle(from), tl, ctx))
      |> List.concat_map(
           fun
           | Bound.Node(_) as n => [n]
           | Root => go(~ctx=RCtx.push(~onto=from, hd, ctx), Seq(tl)),
         )
    }
  };
};

let step = (d: Dir.t, (a, ctx): t('a, 'a)): list(Bound.t(t('a, 'a))) => {
  let enter = enter(~from=Dir.toggle(d));
  // step past r into ctx
  let rec go = (r: Regex.t('a), ctx: RCtx.t(_)) =>
    switch (ctx) {
    | [] => [Bound.Root]
    | [f, ...fs] =>
      switch (d, f) {
      | (_, Star_) => go(Star(r), fs) @ enter(r, ~ctx)
      | (_, Alt_(ls, rs)) => go(Alt(List.rev(ls) @ [r, ...rs]), fs)
      | (L, Seq_([], rs)) => go(Seq([r, ...rs]), fs)
      | (R, Seq_(ls, [])) => go(Seq(List.rev([r, ...ls])), fs)
      | (L, Seq_([hd, ...tl], rs)) =>
        let ctx = [RFrame.Seq_(tl, [r, ...rs]), ...fs];
        enter(hd, ~ctx)
        |> List.concat_map(
             fun
             | Bound.Root => go(hd, ctx)
             | Node(_) as z => [z],
           );
      | (R, Seq_(ls, [hd, ...tl])) =>
        let ctx = [RFrame.Seq_([r, ...ls], tl), ...fs];
        enter(hd, ~ctx)
        |> List.concat_map(
             fun
             | Bound.Root => go(hd, ctx)
             | Node(_) as z => [z],
           );
      }
    };
  go(Atom(a), ctx);
};
