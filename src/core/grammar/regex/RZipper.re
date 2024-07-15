open Stds;

[@deriving (show({with_path: false}), sexp, yojson, ord)]
type t('focus, 'atom) = ('focus, RCtx.t('atom));

// Root in output means r is nullable
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
      | (_, Alt_(ls, rs)) => go(Alt(List.rev(ls) @ [r, ...rs]), fs)
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

let all = (r: Regex.t('a)): list(t('a, 'a) as 'z) => {
  let seen = Hashtbl.create(32);
  let rec go = (z: 'z) =>
    if (!Hashtbl.mem(seen, z)) {
      Hashtbl.add(seen, z, ());
      step(R, z) |> List.filter_map(Bound.to_opt) |> List.iter(go);
    };
  enter(~from=L, r) |> List.filter_map(Bound.to_opt) |> List.iter(go);
  Hashtbl.to_seq(seen) |> Seq.map(fst) |> List.of_seq;
};
