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
    (~from: Dir.t, ~l=Bound.Root, ~r=Bound.Root, s: Sort.t): list(Tile.Sym.t) => {
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
              | Node((sym, _) as z) =>
                bounded(from) || Sym.is_t(sym)
                  ? Some(Tile.Sym.mk(s, p, z)) : None,
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
let finish_swing = (sym: Tile.Sym.t, w: Walk.t, ~from: Dir.t) =>
  switch (sym) {
  | T(t) =>
    let w = Walk.cons(Space(true), w);
    Index.single(Node(Tile(t)), w);
  | NT((s, mold) as nt) =>
    let w = Walk.cons(Tile(nt), w);
    switch (mold) {
    | Root => Index.single(Root, w)
    | Node(mold) =>
      (Sym.NT(s), mold.rctx)
      |> RZipper.step(Dir.toggle(from))
      |> List.map(
           Bound.map(((sym, rctx)) =>
             Mtrl.Tile((Sym.expect_t(sym), {...mold, rctx}))
           ),
         )
      |> List.fold_left((idx, dst) => Index.add(dst, w, idx), Index.empty)
    };
  };

let swing =
  Core.Memo.general(((from: Dir.t, sort: Tile.NT.t)) => {
    let bounds = (s: Tile.NT.t) => {
      let (l_sort, r_sort) = Tile.NT.bounds(sort);
      let (l_s, r_s) = Tile.NT.bounds(s);
      Dir.pick(from, ((l_sort, r_s), (l_s, r_sort)));
    };
    let index = ref(Walk.Index.empty);
    let seen = Hashtbl.create(32);
    let q = Queue.create();
    Queue.push((Sym.nt(sort), Walk.empty), q);
    while (!Queue.is_empty(q)) {
      let (sym, w) = Queue.pop(q);
      index := Index.union(index^, finish_swing(sym, w, ~from));
      // consider going deeper
      switch (sym) {
      | T(_) => ()
      | NT((s, _) as nt) =>
        // need only keep track of sort (sans mold) bc any differently-molded
        // same-sort NTs will only have tighter prec bounds and cannot access
        // any NTs not already reachable from the initial NT
        switch (Hashtbl.find_opt(seen, s)) {
        // avoid cycling
        | Some () => ()
        | None =>
          Hashtbl.add(seen, s, ());
          let (l, r) = bounds(nt);
          let w = Walk.cons(Tile(nt), w);
          derive(~from, ~l, ~r, s)
          |> List.iter(sym => Queue.push((sym, w), q));
        }
      };
    };
    index^;
  });
let swing = (~from: Dir.t, sort): Index.t => swing((from, sort));

let step_all =
  Core.Memo.general(((from: Dir.t, src: End.t)) =>
    switch (src) {
    | Root => swing(~from, Tile.NT.root)
    // space takes prec over everything. could make it so that space eq space
    // but this behavior is encoded in token zipping/merging instead of walks.
    | Node(Space ()) => Index.single(Root, Walk.empty)
    | Node(Grout((s, tips))) =>
      // I think grouting behavior should be determined downstream of walks,
      // so maybe weird that this is here, but it's currently used for exits
      switch (Dir.pick(from, tips)) {
      | Conv => Index.single(Root, Walk.space)
      | Conc => Index.single(Root, Walk.unit(Swing.unit(Grout(s))))
      }
    | Node(Tile((lbl, mold))) =>
      (Sym.T(lbl), mold.rctx)
      |> RZipper.step(Dir.toggle(from))
      |> List.map(
           fun
           // reached end of regex
           | Bound.Root => Index.single(Root, Walk.space)
           | Node((Sym.T(lbl), rctx)) =>
             Index.single(Node(Tile((lbl, {...mold, rctx}))), Walk.space)
           | Node((NT(sort), rctx)) =>
             swing(~from, (sort, Node({...mold, rctx}))),
         )
      |> Index.union_all
    }
  );
let step_all = (~from: Dir.t, src: End.t): Index.t => step_all((from, src));

let bfs = (~from: Dir.t, q: Queue.t((End.t, Walk.t))): Index.t => {
  let index = ref(Index.empty);
  while (!Queue.is_empty(q)) {
    let (mid, mid_src) = Queue.pop(q);
    let seen = Index.mem(mid, index^);
    index := Index.add(mid, mid_src, index^);
    // consider stepping further
    switch (mid) {
    | Node(_) when !seen =>
      step_all(~from, mid)
      |> Index.iter((dst, dst_mid) => Queue.push((dst, dst_mid), q))
    | _ => ()
    };
  };
  index^;
};

let walk_all =
  Core.Memo.general(((from: Dir.t, src: End.t)) => {
    let q = Queue.create();
    step_all(~from, src) |> Index.iter((dst, w) => Queue.push((dst, w), q));
    bfs(~from, q);
  });
let walk_all = (~from: Dir.t, src: End.t) => walk_all((from, src));

let enter_all =
  Core.Memo.general(((from: Dir.t, nt: Mtrl.NT.t)) =>
    switch (nt) {
    | Space(false) => Index.empty
    | Space(true) => Index.single(Node(Space()), Walk.empty)
    | Grout(_) => Index.empty
    | Tile(nt) =>
      let q = Queue.create();
      swing(~from, nt)
      |> Index.filter(is_neq)
      |> Index.iter((dst, w) => Queue.push((dst, w), q));
      bfs(~from, q);
    }
  );
let enter_all = (~from: Dir.t, nt) => enter_all((from, nt));

let step = (~from: Dir.t, src: End.t, dst: End.t): list(t) =>
  Index.find(dst, step_all(~from, src));
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

// todo: combine from and src
let walk = (~from: Dir.t, src: End.t, dst: End.t): list(t) =>
  Index.find(dst, walk_all(~from, src));

let enter = (~from: Dir.t, sort: Mtrl.NT.t, dst: End.t) =>
  Index.find(dst, enter_all(~from, sort));

let exit = (~from: Dir.t, src: End.t) =>
  List.filter(is_eq, walk(~from, src, Root));
