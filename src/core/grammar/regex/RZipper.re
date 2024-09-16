open Stds;

[@deriving (show({with_path: false}), sexp, yojson, ord)]
type t('focus, 'atom) = ('focus, RCtx.t('atom));

// Root in output means r is nullable
/*
 let rec enter =
         (~ctx=RCtx.empty, ~from: Dir.t, ~filter: Filter.t=[], r: Regex.t('a))
    */

let rec enter =
        (~filter: Filter.t=[], ~ctx=RCtx.empty, ~from: Dir.t, r: Regex.t('a))
        : list(Bound.t(t('a, 'a))) => {
  let go = enter(~from);
  switch (r) {
  | Atom(a) => [Node((a, ctx))]
  | Star(r) => [Root, ...go(~ctx=[Star_, ...ctx], r)]
  | Alt(s) =>
    //Filtering should be done here before we construct the context; still we want to persist the names; zipper walking should persist the names

    //Framed module elems pairs subject in focus with the context of the list
    //Converts the list into a zipper structure
    Lists.Framed.elems(List.filter(((nm, _)) => List.mem(nm, filter), s))
    //r = element of list
    //ls + rs = frame
    //ls = list left of element
    //rs = list right of element
    |> List.concat_map((((nm, r), (ls, rs)))
         //if ()
         => go(~ctx=[Alt_(ls, nm, rs), ...ctx], r))
  | Seq(s) =>
    let orient = Dir.pick(from, (Fun.id, List.rev));
    switch (orient(s)) {
    | [] => [Root]
    | [hd, ...tl] =>
      hd
      |> go(~ctx=RCtx.push_s(~onto=Dir.toggle(from), tl, ctx))
      |> List.concat_map(
           fun
           | Bound.Node(_) as n => [n]
           | Root =>
             Seq(orient(tl)) |> go(~ctx=RCtx.push(~onto=from, hd, ctx)),
         )
    };
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
      | (_, Alt_(ls, name, rs)) =>
        go(Alt(List.rev(ls) @ [(name, r), ...rs]), fs)
      | (L, Seq_([], rs)) => go(Seq([r, ...rs]), fs)
      | (R, Seq_(ls, [])) => go(Seq(List.rev([r, ...ls])), fs)
      | (L, Seq_([hd, ...tl], rs)) =>
        let ctx = [RFrame.Seq_(tl, rs), ...fs] |> RCtx.push(~onto=R, r);
        enter(hd, ~ctx)
        |> List.concat_map(
             fun
             | Bound.Root => go(hd, ctx)
             | Node(_) as z => [z],
           );
      | (R, Seq_(ls, [hd, ...tl])) =>
        let ctx = [RFrame.Seq_(ls, tl), ...fs] |> RCtx.push(~onto=L, r);
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
