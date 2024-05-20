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

let derive =
    (~from: Dir.t, ~l=Bound.Root, ~r=Bound.Root, s: Sort.t)
    : list(Molded.Sym.t) => {
  Grammar.v
  |> Sort.Map.find(s)
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
              | Node((sym, rctx)) => {
                  let mold = Mold.{sort: s, prec: p, rctx};
                  bounded(from) || Sym.is_t(sym) ? Some((sym, mold)) : None;
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

// conclude walk at sym if it's a T, otherwise swing over to next syms
// (assumed to be Ts given grammar in operator form)
let arrive = (sym: Bound.t(Molded.Sym.t), w: Walk.t, ~from: Dir.t) =>
  switch (sym) {
  | Root => Index.single(Root, Walk.unit(Swing.unit(Root)))
  | Node((T(t), mold)) =>
    let w = Walk.cons(Node(Space), w);
    Index.single(Node(Tile((t, mold))), w);
  | Node((NT(nt), mold)) =>
    let w = Walk.cons(Node(Tile((nt, mold))), w);
    (Sym.NT(nt), mold.rctx)
    |> RZipper.step(Dir.toggle(from))
    |> List.map(
         Bound.map(((sym, rctx)) =>
           Mtrl.Tile((Sym.expect_t(sym), {...mold, rctx}))
         ),
       )
    |> List.fold_left((index, dst) => Index.add(dst, w, index), Index.empty);
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
    Queue.push((Bound.map(Molded.map(Sym.nt), sort), Walk.empty), q);
    while (!Queue.is_empty(q)) {
      let (sym, w) = Queue.pop(q);
      index := Index.union(index^, arrive(sym, w, ~from));
      // consider going deeper
      switch (Molded.Sym.get_nt(sym)) {
      | None => ()
      | Some(nt) =>
        // need only keep track of sort (sans mold) bc any differently-molded
        // same-sort NTs will only have tighter prec bounds and cannot access
        // any NTs not already reachable from the initial NT
        let s = Molded.NT.sort(nt);
        switch (Hashtbl.find_opt(seen, s)) {
        // avoid cycling
        | Some () => ()
        | None =>
          Hashtbl.add(seen, s, ());
          let (l, r) = bounds(nt);
          let w = Walk.cons(Bound.map(Mtrl.tile, nt), w);
          derive(~from, ~l, ~r, s)
          |> List.iter((sym: Molded.Sym.t) =>
               Queue.push((Bound.Node(sym), w), q)
             );
        };
      };
    };
    index^;
  });
let swing = (~from: Dir.t, sort): Index.t => swing((from, sort));

let step =
  Core.Memo.general(((from: Dir.t, src: End.t)) =>
    switch (src) {
    | Root => swing(~from, Root)
    // space takes prec over everything. could make it so that space eq space
    // but this behavior is encoded in token zipping/merging instead of walks.
    | Node(Space) => Index.empty
    // grout always cleared and re-inserted based on walks between tiles/space
    | Node(Grout(_)) => Index.empty
    | Node(Tile((lbl, mold))) =>
      (Sym.T(lbl), mold.rctx)
      |> RZipper.step(Dir.toggle(from))
      |> List.map(
           fun
           // reached end of regex
           | Bound.Root => Index.single(Root, Walk.empty)
           | Node((Sym.T(lbl), rctx)) =>
             Index.single(Node(Tile((lbl, {...mold, rctx}))), Walk.empty)
           | Node((NT(sort), rctx)) =>
             swing(~from, Node((sort, {...mold, rctx}))),
         )
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

let enter =
  Core.Memo.general(((from: Dir.t, sort: Bound.t(Mtrl.NT.t))) => {
    let q = Queue.create();
    swing(~from, sort)
    |> Index.filter(is_neq)
    |> Index.iter((dst, w) => Queue.push((dst, w), q));
    bfs(~from, q);
  });
let enter = (~from: Dir.t, sort) => enter((from, sort));

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

let walk = (~from: Dir.t, src: End.t, ~to_: End.t): list(t) =>
  Index.find(dst, walk(~from, src));

let enter = (~from: Dir.t, sort: Bound.t(Mtrl.NT.t), ~to_: End.t) =>
  switch (sort, to_) {
  | (Node(Space), Node(Space)) =>
  }
  Index.find(to_, enter(~from, sort));

let exit = (~from: Dir.t, src: End.t) =>
  List.filter(is_eq, walk(~from, src, Root));
