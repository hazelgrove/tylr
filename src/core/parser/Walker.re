open Util;
open Walk;

let descendants = (_s: Sort.t) => failwith("todo Walker.descendants");

let mtrlize =
    (~l=Bound.Root, ~r=Bound.Root, s: Sort.t, ~from: Dir.t): list(Tile.Sym.t) =>
  Grammar.v
  |> Sort.Map.find(s)
  |> Prec.Table.mapi(((p, a), rgx) => {
       let is_bounded =
         Bound.(
           l |> map(l => Prec.lt(~a, l, p)) |> get(~root=true),
           r |> map(r => Prec.gt(~a, p, r)) |> get(~root=true),
         );
       // need to check for legal bounded entry from both sides
       let enter_from = (from: Dir.t) =>
         // currently filtering without assuming single operator form
         // for each prec level. this may need to change.
         RZipper.enter(~from, rgx)
         |> List.filter_map(
              fun
              | Bound.Root => None
              | Node((sym, _) as z) =>
                Dir.pick(from, is_bounded) || Sym.is_t(sym)
                  ? Some(Tile.Sym.mk(s, p, z)) : None,
            );
       switch (enter_from(L), enter_from(R)) {
       | ([], _)
       | (_, []) => []
       | ([_, ..._] as l, [_, ..._] as r) => Dir.pick(from, (l, r))
       };
     })
  |> List.concat;

let swing_over = (w: Walk.t, ~from: Dir.t) =>
  switch (Swing.bot(Walk.hd(w))) {
  | Space(_) => Index.empty // handled elsewhere
  | Grout(s) =>
    Index.single(Root, w)
    |> Index.add(Node(Grout((s, Tip.(Conc, Conv)))), w)
    |> Index.add(Node(Grout((s, Tip.(Conc, Conc)))), w)
  | Tile((_, Root)) => Index.single(Root, w)
  | Tile((s, Node(mold))) =>
    (Sym.NT(s), mold.rctx)
    |> RZipper.step(Dir.toggle(from))
    |> List.map(
         Bound.map(((sym, rctx)) =>
           Mtrl.Tile((Sym.expect_t(sym), {...mold, rctx}))
         ),
       )
    |> List.fold_left((idx, dst) => Index.add(dst, w, idx), Index.empty)
  };

let arrive = (sym: Tile.Sym.t, w: Walk.t, ~from: Dir.t) =>
  switch (sym) {
  | T(t) => Index.single(Node(Tile(t)), Walk.cons(Space(true), w))
  | NT(nt) => swing_over(Walk.cons(Tile(nt), w), ~from)
  };

let swing_into = (w: Walk.t, ~from: Dir.t) => {
  let swing = Chain.hd(w);
  switch (Swing.bot(swing)) {
  | Space(false) => Index.empty
  | Space(true) => Index.single(Node(Space()), Walk.cons(Space(false), w))
  | Grout(s) =>
    descendants(s)
    |> List.concat_map(mtrlize(~from))
    |> List.map(sym => arrive(sym, w, ~from))
    |> Index.union_all
  | Tile((s, _)) =>
    let (l, r) = Swing.bounds(swing);
    mtrlize(~l, s, ~r, ~from)
    |> List.map(sym => arrive(sym, w, ~from))
    |> Index.union_all;
  };
};

let swing_all =
  Core.Memo.general(((nt: Mtrl.NT.t, from: Dir.t)) => {
    let index = ref(Walk.Index.empty);
    let seen = Hashtbl.create(32);
    let q = Queue.create();

    let w_init = Walk.unit(Swing.unit(nt));
    Queue.push(w_init, q);
    index := swing_over(w_init, ~from);

    while (!Queue.is_empty(q)) {
      let w = Queue.pop(q);
      let nt = Swing.bot(Chain.hd(w));
      // need only keep track of sort (sans mold) bc any differently-molded
      // same-sort NTs will only have tighter prec bounds and cannot access
      // any NTs not already reachable from the initial NT
      switch (Hashtbl.find_opt(seen, Mtrl.NT.sort(nt))) {
      | Some () => () // avoid cycling
      | None =>
        Hashtbl.add(seen, Mtrl.NT.sort(nt), ());
        let swung = swing_into(w, ~from);
        index := Index.union(index^, swung);
        swung |> Index.iter((_, w) => Queue.push(w, q));
      };
    };

    index^;
  });
let swing_all = (sort, ~from: Dir.t): Index.t => swing_all((sort, from));

let step_all =
  Core.Memo.general(((src: End.t, from: Dir.t)) =>
    switch (src) {
    | Root => swing_all(Tile(Tile.NT.root), ~from)
    // space takes prec over everything. could make it so that space eq space
    // but this behavior is encoded in token zipping/merging instead of walks.
    | Node(Space ()) => Index.single(Root, Walk.empty)
    | Node(Grout((s, tips))) =>
      switch (Dir.pick(from, tips)) {
      | Conc => swing_all(Grout(s), ~from)
      | Conv => swing_into(Walk.space, ~from) |> Index.add(Root, Walk.space)
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
             swing_all(Tile((sort, Node({...mold, rctx}))), ~from),
         )
      |> Index.union_all
    }
  );
let step_all = (src: End.t, ~from: Dir.t): Index.t => step_all((src, from));

let bfs = (~from: Dir.t, q: Queue.t((End.t, Walk.t))): Index.t => {
  let index = ref(Index.empty);
  while (!Queue.is_empty(q)) {
    let (mid, mid_src) = Queue.pop(q);
    let seen = Index.mem(mid, index^);
    index := Index.add(mid, mid_src, index^);
    // consider stepping further
    switch (mid) {
    | Node(m) when !seen =>
      step_all(~from, mid)
      |> Index.iter((dst, dst_mid) =>
           Queue.push((dst, Walk.append(dst_mid, m, mid_src)), q)
         )
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
  Core.Memo.general(((from: Dir.t, nt: Mtrl.NT.t)) => {
    let q = Queue.create();
    swing_all(~from, nt)
    |> Index.filter(is_neq)
    |> Index.iter((dst, w) => Queue.push((dst, w), q));
    bfs(~from, q);
  });
let enter_all = (~from: Dir.t, nt) => enter_all((from, nt));

let step = (~from: Dir.t, src: End.t, dst: End.t) =>
  Index.find(dst, step_all(~from, src));
let lt =
  Core.Memo.general(((l: End.t, r: End.t)) =>
    Set.filter(Walk.is_neq, step(~from=L, l, r))
  )
  |> Funs.curry;
let gt =
  Core.Memo.general(((l: End.t, r: End.t)) =>
    Set.filter(Walk.is_neq, step(~from=R, r, l))
  )
  |> Funs.curry;
let eq =
  Core.Memo.general(((l: End.t, r: End.t)) =>
    Set.filter(Walk.is_eq, step(~from=L, l, r))
  )
  |> Funs.curry;

// todo: combine from and src
let walk = (~from: Dir.t, src: End.t, dst: End.t) =>
  Index.find(dst, walk_all(~from, src));

let enter = (~from: Dir.t, sort: Mtrl.NT.t, dst: End.t) =>
  Index.find(dst, enter_all(~from, sort));

let exit = (~from: Dir.t, src: End.t) =>
  Set.filter(Walk.is_eq, walk(~from, src, Root));
