open Stds;

// module Stack = {
//   type t = {
//     slope: Slope.t,
//     bound: Bound.t(Wald.t),
//   };
//   let mk = (~slope=Slope.empty, bound: Bound.t(_)) => {slope, bound};
//   let map_hd = (f: Wald.t => Wald.t, {slope, bound}: t) =>
//     switch (slope) {
//     | [] => mk(Bound.map(f, bound))
//     | [hd, ...tl] => {slope: [{...hd, wald: f(hd.wald)}, ...tl], bound}
//     };
//   let push = (w: Wald.t, c: Rel.t(Cell.t, Cell.t), {slope, bound}: t): t =>
//     switch (c) {
//     | Neq(c) => {
//         slope: [Terr.{cell: c, wald: Wald.rev(w)}, ...slope],
//         bound,
//       }
//     | Eq(c) => map_hd(Wald.zip_cell(w, c), {slope, bound})
//     };
//   let of_baked = (src: Bound.t(Wald.t), baked: Baked.t, dst: Wald.t): t =>
//     baked
//     |> Baked.fold(
//          c => push(dst, c),
//          (t, c) => push(Wald.of_tok(t), c),
//          mk(src),
//        );
// };

let lt = (l: Wald.t, r: Wald.t) =>
  !Lists.is_empty(Walker.lt(Node(Wald.face(l)), Node(Wald.face(r))));
let gt = (l: Wald.t, r: Wald.t) =>
  !Lists.is_empty(Walker.gt(Node(Wald.face(l)), Node(Wald.face(r))));
let eq = (l: Wald.t, r: Wald.t) =>
  !Lists.is_empty(Walker.eq(Node(Wald.face(l)), Node(Wald.face(r))));

// assumes w is already oriented toward side.
// used to complete zigg top when it takes precedence over pushed wald.
let complete_wald = (~side: Dir.t, ~fill=Cell.empty, w: Wald.t): Terr.t => {
  let _ = failwith("todo: review side arg in callers");
  let from = Dir.toggle(side);
  let exited = Walker.exit(~from, Node(Wald.face(w)));
  let baked = Baker.pick_and_bake(~from, ~fill=Fill.unit(fill), exited);
  // exited |> Oblig.Delta.minimize(Baker.bake(~from, ~fill=Fill.unit(fill)));
  switch (baked) {
  | Some(baked) => Baked.complete_wald(baked, w)
  | None =>
    assert(!Cell.is_empty(fill));
    print_endline("warning: dropping fill " ++ Cell.show(fill));
    let baked =
      Baker.pick_and_bake(~from, exited)
      |> Options.get_fail("bug: expected bake to succeed sans fill");
    // walker bug if no exits
    // let exited = List.hd(exited);
    // let baked = Baker.bake_sans_fill(~from, exited);
    Baked.complete_wald(baked, w);
  };
};
// onto confusing here when considered alone, same onto piped from push(~onto)
let complete_terr = (~onto: Dir.t, ~fill=Cell.empty, terr: Terr.t): Cell.t => {
  let orient = Dir.pick(onto, (Meld.rev, Fun.id));
  let exited = Walker.exit(~from=onto, Node(Terr.face(terr)));
  let baked =
    Baker.pick_and_bake(
      ~repair=true,
      ~from=onto,
      ~fill=Fill.unit(fill),
      exited,
    );
  // exited
  // |> Oblig.Delta.minimize(Baker.bake(~from=onto, ~fill=Fill.unit(fill)));
  switch (baked) {
  | Some(baked) =>
    Cell.put(orient(Baked.complete_terr(baked, terr, ~onto)))
  | None =>
    assert(!Cell.is_empty(fill));
    print_endline("warning: dropping fill " ++ Cell.show(fill));
    // walker bug if no exits
    // let exited = List.hd(exited);
    let baked =
      Baker.pick_and_bake(~repair=true, ~from=onto, exited)
      |> Options.get_fail("bug: expected bake to succeed sans fill");
    Cell.put(orient(Baked.complete_terr(baked, terr, ~onto)));
  };
};
let complete_slope = (~onto: Dir.t, ~fill=Cell.empty) =>
  Slope.fold(fill => complete_terr(~onto, ~fill), fill);

let complete_bounded =
    (~bounds as (l, r), ~onto: Dir.t, ~fill=Cell.empty, slope) => {
  // todo: fix weird
  let fill = complete_slope(~onto, ~fill, slope);
  Walker.walk_eq(~from=L, Bound.map(Terr.face, l), Bound.map(Terr.face, r))
  |> Baker.pick_and_bake(~repair=true, ~fill=Fill.unit(fill), ~from=onto)
  // |> List.filter_map(Baker.bake(~fill=Fill.unit(fill), ~from=onto))
  // |> Stds.Lists.hd
  |> Option.map(baked => snd(Chain.hd(baked)))
  |> Options.get_fail("hmmm");
};

module Connection = {
  type t = Rel.t(Wald.t, (Dir.t, Slope.t));
  let eq = wald => Rel.Eq(wald);
  let neq = (d, slope) => Rel.Neq((d, slope));
};

module Melded = {
  type t = Rel.t(Terr.t, Slope.t);
  let eq = terr => Rel.Eq(terr);
  let neq = slope => Rel.Neq(slope);
  let face =
    fun
    | Rel.Eq(terr) => Terr.face(terr)
    | Neq(slope) => Option.get(Slope.face(slope));
  let extend = tl => Rel.map(~eq=Terr.extend(tl), ~neq=Slope.extend(tl));
};

let connect_eq =
    (
      ~repair=false,
      ~onto as d: Dir.t,
      onto: Terr.t,
      ~fill=Cell.empty,
      t: Token.t,
    )
    : option(Terr.t) => {
  open Options.Syntax;
  let rec go = (onto: Terr.t, fill) => {
    let/ () = repair ? rm_ghost_and_go(onto, fill) : None;
    Walker.walk_eq(~from=d, Node(Terr.face(onto)), Node(t.mtrl))
    |> Oblig.Delta.minimize(
         ~to_zero=!repair,
         Baker.bake_swings(~from=d, ~fill),
       )
    |> Option.to_list
    |> Oblig.Delta.minimize(~to_zero=!repair, Baker.bake_stances)
    |> Option.map(baked => Baked.connect_eq(t, baked, onto, ~onto=d));
  }
  and rm_ghost_and_go = (onto, fill) =>
    switch (Terr.unlink(onto)) {
    | (hd, cell, Some(tl)) when Option.is_some(Token.Tile.is_ghost(hd)) =>
      let fill = Chain.link(cell, (), fill);
      go(tl, fill) |> Effects.perform_if(Remove(hd));
    | _ => None
    };
  go(onto, Fill.unit(fill));
};
let connect_neq =
    (
      ~repair=false,
      ~onto as d: Dir.t,
      onto: Bound.t(Terr.t),
      ~fill=Cell.empty,
      t: Token.t,
    )
    : option(Slope.t) => {
  Walker.walk_neq(~from=d, Bound.map(Terr.face, onto), Node(t.mtrl))
  |> Baker.pick_and_bake(~repair, ~from=d, ~fill=Fill.unit(fill))
  |> Option.map(baked => Baked.connect_neq(t, baked, onto, ~onto=d));
};
let connect_lt = connect_neq(~onto=L);
let connect_gt = connect_neq(~onto=R);

let connect_ineq =
    (
      ~repair=false,
      ~onto as d: Dir.t,
      onto: Bound.t(Terr.t),
      ~fill=Cell.empty,
      t: Token.t,
    )
    : option(Melded.t) => {
  let eq = () =>
    Bound.to_opt(onto)
    |> Options.bind(~f=onto => connect_eq(~repair, ~onto=d, onto, ~fill, t))
    |> Option.map(Rel.eq);
  let neq = () =>
    connect_neq(~repair, ~onto=d, onto, ~fill, t) |> Option.map(Rel.neq);
  Oblig.Delta.minimize(~to_zero=!repair, f => f(), [eq, neq]);
};

let connect =
    (
      ~repair=false,
      ~onto as d: Dir.t,
      onto: Terr.t,
      ~fill=Cell.empty,
      t: Token.t,
    )
    : Result.t(Melded.t, Cell.t) => {
  let b = Dir.toggle(d);
  let eq = () =>
    connect_eq(~repair, ~onto=d, onto, ~fill, t)
    |> Option.map(Melded.eq)
    |> Option.map(Result.ok);
  let neq_d = () =>
    connect_neq(~repair, ~onto=d, Node(onto), ~fill, t)
    |> Option.map(Melded.neq)
    |> Option.map(Result.ok);
  let neq_b = () => {
    let (hd, tl) = Wald.uncons(onto.wald);
    connect_neq(~repair, ~onto=b, Node(Terr.of_tok(t)), ~fill, hd)
    |> Option.map(Slope.extend(tl))
    |> Option.map(complete_slope(~onto=b, ~fill=onto.cell))
    |> Option.map(Result.err);
  };
  // ensure consistent ordering
  let neqs = Dir.pick(d, ([neq_d, neq_b], [neq_b, neq_d]));
  [eq, ...neqs]
  |> Oblig.Delta.minimize(~to_zero=!repair, f => f())
  |> Option.value(~default=Error(complete_terr(~onto=d, ~fill, onto)));
};

let rec push_neq =
        (
          ~repair=false,
          ~onto: Dir.t,
          t: Token.t,
          ~fill=Cell.empty,
          slope: Slope.t,
        )
        : Result.t(Slope.t, Cell.t) =>
  switch (Slope.unlink(slope)) {
  | Some((tok, cell, slope)) when repair && Token.Grout.is(tok) =>
    Effects.remove(tok);
    let slope = Slope.cat(Slope.unroll(~from=onto, cell), slope);
    push_neq(~repair, ~onto, t, ~fill, slope);
  | _ =>
    switch (slope) {
    | [] => Error(fill)
    | [hd, ...tl] =>
      switch (connect(~repair, ~onto, hd, ~fill, t)) {
      | Error(fill) => push_neq(~repair, ~onto, t, ~fill, tl)
      | Ok(Neq(s)) => Ok(Slope.cat(s, slope))
      | Ok(Eq(hd)) => Ok([hd, ...tl])
      }
    }
  };

let push_bound = (~repair=false, t: Token.t, ~fill=Cell.empty, bound, ~onto) => {
  let ineq = () => connect_ineq(~repair, ~onto, bound, ~fill, t);
  switch (bound) {
  | Bound.Root => ineq()
  | Node(terr) =>
    switch (Terr.zip_hd(t, terr, ~onto)) {
    | None => ineq()
    | Some(terr) => Some(Eq(terr))
    }
  };
};
let push =
    (~repair=false, t: Token.t, ~fill=Cell.empty, slope, ~bound, ~onto)
    : option(Melded.t) =>
  switch (Slope.zip_hd(t, slope, ~onto)) {
  | Some(slope) => Some(Neq(slope))
  | None =>
    switch (t.mtrl) {
    | Space () => Some(Neq([Terr.of_tok(t), ...slope]))
    | Grout(_)
    | Tile(_) =>
      switch (push_neq(~repair, t, ~fill, slope, ~onto)) {
      | Ok(slope) => Some(Neq(slope))
      | Error(fill) => push_bound(~repair, t, ~fill, bound, ~onto)
      }
    }
  };

let push_space = (spc: Token.t, slope: Slope.t, ~onto: Dir.t) => {
  assert(Token.Space.is(spc));
  switch (slope) {
  | [{wald: W(([spc'], [])), _} as hd, ...tl] =>
    let (l, r) = Dir.order(onto, (spc', spc));
    [{...hd, wald: W(([Token.merge(l, r)], []))}, ...tl];
  | _ => [Terr.of_tok(spc), ...slope]
  };
};
