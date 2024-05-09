open Util;
open Walk;

let expect_lbl =
  fun
  | Sym.NT(_) => failwith("expected alternating form")
  | T(mlbl) => mlbl;
let expect_srt =
  fun
  | Sym.T(_) => failwith("expected alternating form")
  | NT(msrt) => msrt;

let enter =
    (~from: Dir.t, ~l=Bound.Root, ~r=Bound.Root, s: Mtrl.Sorted.t)
    : list(Molded.NT.t) => {
  MGrammar.v
  |> Mtrl.Sorted.Map.find(s)
  |> Prec.Table.mapi(((p, a), rgx) => {
       let bounded =
         fun
         | Dir.L =>
           l |> Bound.map(l => Prec.lt(~a, l, p)) |> Bound.get(~root=true)
         | R =>
           r |> Bound.map(r => Prec.gt(~a, p, r)) |> Bound.get(~root=true);
       // need to check for legal bounded entry from both sides
       let go = (from: Dir.t) =>
         // currently filtering without assuming single operator form
         // for each prec level. this may need to change.
         RZipper.enter(~from, rgx)
         |> List.filter_map(
              fun
              | Bound.Root => None
              | Node((msym, rctx)) => {
                  let msrt = expect_srt(msym);
                  let mold = Mold.{sort: s, prec: p, rctx};
                  bounded(from) || Mtrl.is_space(msrt)
                    ? Some((msrt, mold)) : None;
                },
            );
       switch (go(L), go(R)) {
       | ([], _)
       | (_, []) => []
       | ([_, ..._] as ent_l, [_, ..._] as ent_r) =>
         Dir.pick(from, (ent_l, ent_r))
       };
     })
  |> List.concat;
};

// step from last molded NT to identify valid destinations for given walk
let arrive = (~from: Dir.t, w: Walk.t): Index.t =>
  switch (Chain.hd(Chain.hd(w))) {
  | Root => Index.singleton(Root, Set.singleton(w))
  | Node((mtrl, mold)) =>
    (Sym.NT(mtrl), mold.rctx)
    |> RZipper.step(Dir.toggle(from))
    |> List.map(
         Bound.map(((msym, rctx)) => (expect_lbl(msym), {...mold, rctx})),
       )
    |> List.map(lbl => (lbl, Set.singleton(w)))
    |> Index.of_list
    |> (
      Mold.nullable(~side=Dir.toggle(from), mold)
        ? Index.add(Root, w) : Fun.id
    )
  };

let swing =
  Core.Memo.general(((from: Dir.t, sort: Bound.t(Molded.NT.t))) => {
    let bounds = (s: Bound.t(Molded.NT.t)) => {
      let (l_sort, r_sort) = Molded.NT.bounds(sort);
      let (l_s, r_s) = Molded.NT.bounds(s);
      Dir.pick(from, ((l_sort, r_s), (l_s, r_sort)));
    };
    let index = ref(Walk.Index.empty);
    let seen = Hashtbl.create(32);
    let q = Queue.create();
    Queue.push(Walk.singleton(Swing.mk_eq(sort)), q);
    while (!Queue.is_empty(q)) {
      let w = Queue.pop(q);
      index := Index.union(index^, arrive(~from, w));
      // consider going deeper
      let s = Chain.hd(Chain.hd(w));
      // need only keep track of mtrl (sans mold) bc any differently-molded
      // same-mtrl NTs will only have tighter prec bounds and cannot access
      // any NTs not already reachable from the initial NT
      let mtrl = Molded.NT.mtrl(s);
      switch (s, Hashtbl.find_opt(seen, mtrl)) {
      // no further to go if "space" of space sort
      | (Node((Space, Mold.{sort: Space, _})), _)
      // avoid cycling
      | (_, Some ()) => ()
      | (_, None) =>
        Hashtbl.add(seen, mtrl, ());
        let (l, r) = bounds(s);
        enter(~from, ~l, ~r, mtrl)
        |> List.iter((next: Molded.NT.t) =>
             Queue.push(Walk.cons(Node(next), w), q)
           );
      };
    };
    index^;
  });
let swing = (~from: Dir.t, sort): Index.t => swing((from, sort));

let step =
  Core.Memo.general(((from: Dir.t, src: End.t)) =>
    switch (src) {
    | Root => swing(~from, Root)
    | Node((mtrl, mold)) =>
      (Sym.T(mtrl), mold.rctx)
      |> RZipper.step(Dir.toggle(from))
      |> List.map(
           Bound.map(((msym, rctx)) =>
             (expect_srt(msym), {...mold, rctx})
           ),
         )
      |> List.map(swing(~from))
      |> Index.union_all
    }
  );
let step = (~from: Dir.t, src: End.t): Index.t => step((from, src));

let bfs = (~from: Dir.t, q: Queue.t((End.t, Walk.t))): Index.t => {
  let index = ref(Index.empty);
  while (!Queue.is_empty(q)) {
    let (mid, mid_src) = Queue.pop(q);
    let seen = Index.mem(mid, index^);
    index := Index.add(mid, mid_src, index^);
    // consider stepping further
    switch (mid) {
    | Node(_) when !seen =>
      step(~from, mid)
      |> Index.iter((dst, dst_mid) => Queue.push((dst, dst_mid), q))
    | _ => ()
    };
  };
  index^;
};

let walk =
  Core.Memo.general(((from: Dir.t, src: End.t)) => {
    let q = Queue.create();
    step(~from, src) |> Index.iter((dst, w) => Queue.push((dst, w), q));
    bfs(~from, q);
  });
let walk = (~from: Dir.t, src: End.t) => walk((from, src));

let walk_into =
  Core.Memo.general(((from: Dir.t, sort: Bound.t(Molded.NT.t))) => {
    let q = Queue.create();
    swing(~from, sort)
    |> Index.filter(is_neq)
    |> Index.iter((dst, w) => Queue.push((dst, w), q));
    bfs(~from, q);
  });
let walk_into = (~from: Dir.t, sort) => walk_into((from, sort));

let step = (~from: Dir.t, src: End.t, dst: End.t): list(t) =>
  Index.find(dst, step(~from, src));
let lt =
  Core.Memo.general(((l: End.t, r: End.t)) =>
    step(~from=L, l, r)
    |> List.filter_map(walk => {
         let swing = hd(walk);
         Swing.is_eq(swing) ? None : Some(Swing.bot(swing));
       })
  )
  |> Funs.curry;
let gt =
  Core.Memo.general(((l: End.t, r: End.t)) =>
    step(~from=R, r, l)
    |> List.filter_map(walk => {
         let swing = hd(walk);
         Swing.is_eq(swing) ? None : Some(Swing.bot(swing));
       })
  )
  |> Funs.curry;
let eq =
  Core.Memo.general(((l: End.t, r: End.t)) =>
    step(~from=L, l, r) |> List.filter(walk => Swing.is_eq(hd(walk)))
  )
  |> Funs.curry;

let walk = (~from: Dir.t, src: End.t, dst: End.t): list(t) =>
  Index.find(dst, walk(~from, src));

let enter = (~from: Dir.t, sort: Bound.t(Molded.NT.t), dst: End.t) =>
  Index.find(dst, walk_into(~from, sort));
let exit = (~from: Dir.t, src: End.t) =>
  List.filter(is_eq, walk(~from, src, Root));
