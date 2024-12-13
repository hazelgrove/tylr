open Stds;
open Walk;

let dbg = ref(false);

let mtrlize_tile = ((l, r, (filter, s), from)) =>
  Grammar.v
  |> Sort.Map.find(s)
  |> Prec.Table.mapi(((p, a), rgx) => {
       let is_bounded =
         Bound.(
           l |> map(l => Prec.lt(~a, l, p)) |> get(~root=true),
           r |> map(r => Prec.gt(~a, p, r)) |> get(~root=true),
         );
       RZipper.enter(~from, ~filter, rgx)
       |> List.filter_map(
            fun
            | Bound.Root => None
            | Node((sym, _) as z) =>
              Dir.pick(from, is_bounded) || Sym.is_t(sym)
                ? Some(Tile.Sym.mk(s, p, z)) : None,
          );
     })
  |> List.concat;
let mtrlize_tile =
    (~l=Bound.Root, ~r=Bound.Root, s: (Filter.t, Sort.t), ~from: Dir.t) =>
  mtrlize_tile((l, r, s, from));

let mtrlize_grout =
    (~l=Bound.Root, ~r=Bound.Root, s: Sort.t): list(Grout.Sym.t) =>
  Grout.T.(
    // here we materialize Ts directly, skipping over any intervening grout NTs,
    // because the NTs don't have enough contextual info to swing over.
    // instead these NTs are generated in `arrive`.
    switch (l, r) {
    | (Root, Root) => [op(s), pre(s), pos(s), in_(s)]
    | (Root, Node(_)) => [op(s), pos(s)]
    | (Node(_), Root) => [op(s), pre(s)]
    | (Node(_), Node(_)) => [op(s)]
    }
  )
  |> List.map(Sym.t);

let mtrlize =
    (~l=Bound.Root, ~r=Bound.Root, s: (Filter.t, Sort.t), ~from: Dir.t)
    : list(Mtrl.Sym.t) =>
  List.map(Mtrl.Sym.of_grout, mtrlize_grout(~l, snd(s), ~r))
  @ List.map(Mtrl.Sym.of_tile, mtrlize_tile(~l, s, ~r, ~from));

let swing_over = (w: Walk.t, ~from: Dir.t) =>
  switch (Swing.bot(Walk.hd(w))) {
  | Space(_) => Index.empty // handled in arrive
  | Grout(s) =>
    Index.single(Root, w)
    |> Index.add(Node(Grout((s, Dir.order(from, Tip.(Conc, Conv))))), w)
    |> Index.add(Node(Grout((s, Tip.(Conc, Conc)))), w)
  | Tile((_, Root)) => Index.single(Root, w)
  | Tile((s, Node(mold))) =>
    (Sym.NT(s), mold.rctx)
    |> RZipper.step(Dir.toggle(from))
    |> List.map(
         Bound.map(((sym, rctx)) =>
           try(Mtrl.Tile((Sym.expect_t(sym), {...mold, rctx}))) {
           | _ =>
             P.show("sym", Grammar.Sym.show(sym));
             P.show("rctx", RCtx.show(Grammar.Sym.pp, rctx));
             failwith("");
           }
         ),
       )
    |> List.fold_left((idx, dst) => Index.add(dst, w, idx), Index.empty)
  };

let arrive = (sym: Mtrl.Sym.t, w: Walk.t, ~from: Dir.t) =>
  switch (sym) {
  | NT(nt) => swing_over(Walk.cons(nt, w), ~from)
  | T(t) =>
    // extra logic encapsulated here to deal with space and grout NTs
    // not having enough contextual info to swing over, as well as to
    // generate placeholder space NT cells to separate adjacent tile Ts
    let over: Mtrl.NT.t =
      switch (t) {
      | Space(_) => Space(Closed)
      | Tile(_) => Space(Open)
      | Grout((s, tips)) =>
        switch (Dir.pick(from, tips)) {
        | Conc => Grout(s)
        | Conv => Space(Open)
        }
      };
    Index.single(Node(t), Walk.cons(over, w));
  };

let swing_into = (w: Walk.t, ~from: Dir.t) => {
  let swing = Chain.hd(w);
  switch (Swing.bot(swing)) {
  | Space(Closed) => Index.empty
  | Space(Open) =>
    Space.T.all
    |> List.fold_left(
         (idx, t) =>
           Index.add(Node(Space(t)), Walk.cons(Space(Closed), w), idx),
         Index.empty,
       )
  | Grout(s) =>
    // grout NTs can only be entered from directly preceding grout T.
    // otherwise, potential soundness issues where a tile T can step to
    // any descendant sort T.
    Swing.height(swing) == 0
      ? Sorts.deps(s)
        |> List.map(s => ([], s))
        |> List.concat_map(mtrlize(~from))
        |> List.map(sym => arrive(sym, w, ~from))
        |> Index.union_all
      : Index.empty
  | Tile((s, _)) =>
    let (l, r) = Swing.bounds(swing, ~from);
    mtrlize(~l, s, ~r, ~from)
    |> List.map(sym => arrive(sym, w, ~from))
    |> Index.union_all;
  };
};

let swing_all =
  Memo.general(((nt: Mtrl.NT.t, from: Dir.t)) => {
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
  Memo.general(((src: End.t, from: Dir.t)) =>
    switch (src) {
    | Root => swing_all(Tile(Tile.NT.root), ~from)
    | Node(Space(_)) =>
      // space takes prec over everything and matches itself
      Space.T.all
      |> List.fold_left(
           (idx, t) => Index.add(Node(Space(t)), Walk.empty, idx),
           Index.single(Root, Walk.empty),
         )
    | Node(Grout((s, tips))) =>
      switch (Dir.pick(Dir.toggle(from), tips)) {
      | Conc =>
        let w = Walk.unit(Walk.Swing.unit(Grout(s)));
        let un = Mtrl.Grout((s, Dir.order(from, Tip.(Conc, Conv))));
        let bin = Mtrl.Grout((s, Tip.(Conc, Conc)));
        swing_all(Grout(s), ~from)
        |> Index.add(Root, w)
        |> Index.add(Node(un), w)
        |> Index.add(Node(bin), w)
        |> Index.union(swing_into(Walk.space, ~from));
      | Conv =>
        Index.single(Root, Walk.space)
        |> Index.union(swing_into(Walk.space, ~from))
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
      |> List.cons(swing_into(Walk.space, ~from))
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

// if bottom (head) of swing is Tile, need to check the top (foot) of swing for
// whether it's also a Tile of the same sort. if so, combine the prec bounds of top
// and bottom to produce bounded_sort. otherwise, just use the prec bound of the
// bottom and have the other side bound be Bound.Root.
type bounded_sort = (Bound.t(Prec.t), Sort.t, Bound.t(Prec.t));
type swing_profile = Mtrl.t(Space.NT.t, Grout.NT.t, bounded_sort);
type swings_profile = list(swing_profile);

let build_swing_profile = (from: Dir.t, s: Swing.t): swing_profile => {
  let btm = Swing.bot(s);
  let top = Swing.top(s);

  switch (btm) {
  | Tile(((_, btm_sort), btm_bound)) =>
    let btm_bound =
      switch (btm_bound) {
      | Root => Bound.root
      | Node(btm_mold) => Dir.pick(Dir.toggle(from), Mold.bounds(btm_mold))
      };
    switch (top) {
    | Tile(((_, top_sort), top_bound)) =>
      let top_bound =
        switch (top_bound) {
        | Node(top_mold) when btm_sort == top_sort =>
          Dir.pick(from, Mold.bounds(top_mold))
        | _ => Bound.root
        };

      let (left_bound, right_bound) =
        Dir.order(from, (top_bound, btm_bound));

      Mtrl.Tile((left_bound, btm_sort, right_bound));
    | _ =>
      let (left_bound, right_bound) =
        Dir.order(from, (Bound.root, btm_bound));
      Mtrl.Tile((left_bound, btm_sort, right_bound));
    };
  | Space(spc) => Space(spc)
  | Grout(grt) => Grout(grt)
  };
};

//TODO: instead of doing list.mem (equality) check if the profile has wider (more relaxed) bounds than the pre-existing profile (gte)
//make the accumulator just a list of walks (the already checked walks) and then recalcuate the profiles for each walk in the accumulator - this will solve the problem of needing to do two-way checks with profiles/walks that were already approved/checked
let walk_filter_by_swing = (from: Dir.t, walks: list(Walk.t)): list(Walk.t) => {
  walks
  |> List.fold_left(
       (
         (swings_profiles: list(swings_profile), walks: list(Walk.t)),
         w: Walk.t,
       ) => {
         let swings = Walk.swings(w);
         let profile = List.map(build_swing_profile(from), swings);

         if (List.mem(profile, swings_profiles)) {
           (swings_profiles, walks);
         } else {
           ([profile, ...swings_profiles], [w, ...walks]);
         };
       },
       ([], []),
     )
  |> snd
  |> List.rev;
};

// notes:
// - [DONE] strengthen minimality check to rule out multiple grout levels
// - [DOING} apply additional filter that rules outs walks that accommodate the same thing as another existing walk

// WIP: inspecting the diff `git show -m bdc35446 -- src/core/material` to see why
// there is a difference between precompiled entry from root vs regular entry from
// root
let is_minimal = (w: Walk.t) =>
  !(
    Walk.is_neq(w)
    && (
      // avoid walks with spurious mid levels like the paren in `# <. ( <. 2`
      List.exists(Mtrl.is_tile, Walk.stance_sorts(w).mid)
      // avoid walk withs multiple grout levels eg `# <. << <. << <. Int`
      || List.length(List.filter(Mtrl.is_grout, Walk.stance_sorts(w).mid))
      > 1
    )
  );

let walk_all =
  Memo.general(((from: Dir.t, src: End.t)) => {
    let q = Queue.create();
    step_all(~from, src) |> Index.iter((dst, w) => Queue.push((dst, w), q));
    bfs(~from, q)
    |> Index.filter(Walk.is_valid)
    |> Index.filter(is_minimal)
    |> Index.fil(_ =>
         fun
         | [] => false
         | _ => true
       )
    |> Index.mp(walk_filter_by_swing(from))
    |> Index.sort;
  });
let walk_all = (~from: Dir.t, src: End.t): End.Map.t(list(T.t)) =>
  walk_all((from, src));

let enter_all =
  Memo.general(((from: Dir.t, nt: Mtrl.NT.t)) => {
    let q = Queue.create();
    swing_all(~from, nt)
    |> Index.filter(is_neq)
    |> Index.iter((dst, w) => Queue.push((dst, w), q));
    bfs(~from, q)
    |> Index.filter(Walk.is_valid)
    |> Index.filter(is_minimal)
    |> Index.fil(_ =>
         fun
         | [] => false
         | _ => true
       )
    // todo: apply swings_profile filter here
    |> Index.sort;
  });
let enter_all = (~from: Dir.t, nt) => enter_all((from, nt));

let walk_l_map = ref(End.Map.empty);
let walk_r_map = ref(End.Map.empty);
let enter_l_map = ref(Mtrl.NT.Map.empty);
let enter_r_map = ref(Mtrl.NT.Map.empty);

let stances_flipped = ref(Thin.FlippedStanceMap.empty);
let nts_flipped = ref(Thin.FlippedNTMap.empty);

let read_warmed_stances_nts = () => {
  print_endline("got warmed root l");

  print_endline("Reading nts");
  let nts_map =
    Thin.NTMap.t_of_sexp(
      Sexplib.Conv.int_of_sexp,
      Sexplib.Sexp.of_string(PrecompiledFiles.warmed_nts()),
    );
  print_endline("reading stances");
  let stances_map =
    Thin.StanceMap.t_of_sexp(
      Sexplib.Conv.int_of_sexp,
      Sexplib.Sexp.of_string(PrecompiledFiles.warmed_stances()),
    );

  stances_flipped :=
    stances_map
    |> Thin.StanceMap.to_seq
    |> Seq.map(((k, v)) => (v, k))
    |> Thin.FlippedStanceMap.of_seq;

  nts_flipped :=
    nts_map
    |> Thin.NTMap.to_seq
    |> Seq.map(((k, v)) => (v, k))
    |> Thin.FlippedNTMap.of_seq;
};

let read_warmed_walked = () => {
  print_endline("reading warmed walked");
  print_endline("reading walk r");

  let _thin_walk_r =
    Thin.ThinEnd.Map.t_of_sexp(
      Thin.ThinIndex.t_of_sexp,
      Sexplib.Sexp.of_string(PrecompiledFiles._warmed_walked_r()),
    );
  print_endline("doing a gc full major");
  Gc.full_major();
  print_endline("reading walk l");
  let thin_walk_l =
    Thin.ThinEnd.Map.t_of_sexp(
      Thin.ThinIndex.t_of_sexp,
      Sexplib.Sexp.of_string(PrecompiledFiles._warmed_walked_l()),
    );

  walk_r_map :=
    Thin.walk_map_of_thin(_thin_walk_r, stances_flipped^, nts_flipped^);
  walk_l_map :=
    Thin.walk_map_of_thin(thin_walk_l, stances_flipped^, nts_flipped^);
};

let read_warmed_enter = () => {
  let thin_enter_r =
    Thin.ThinNT.Map.t_of_sexp(
      Thin.ThinIndex.t_of_sexp,
      Sexplib.Sexp.of_string(PrecompiledFiles._enter_r_map()),
    );
  let thin_enter_l =
    Thin.ThinNT.Map.t_of_sexp(
      Thin.ThinIndex.t_of_sexp,
      Sexplib.Sexp.of_string(PrecompiledFiles._enter_l_map()),
    );

  enter_r_map :=
    Thin.enter_map_of_thin(thin_enter_r, stances_flipped^, nts_flipped^);
  enter_l_map :=
    Thin.enter_map_of_thin(thin_enter_l, stances_flipped^, nts_flipped^);
};

let read_warmed = () => {
  read_warmed_stances_nts();
  Gc.full_major();
  print_endline("read warmed stances nts");
  read_warmed_walked();
  print_endline("read warmed walked");
  End.Map.bindings(walk_l_map^)
  |> List.iteri((i, (src, index)) =>
       if (i < 1) {
         P.show("src", End.show(src));
         Index.bindings(index)
         |> List.iteri((_i, (dst, ws)) => {
              // if (i < 5) {
              P.show("- dst", End.show(dst));
              ws
              |> List.iteri((_i, w)
                   //  if (i < 3) {
                   => P.show("--- w", Walk.show(w)));
              //  }
              // }
            });
       }
     );
  Gc.full_major();
  // read_warmed_enter();
  // print_endline("read warmed entered");
  // Gc.full_major();
};

let walk_all_precompiled =
    (~from: Dir.t, source: End.t): End.Map.t(list(T.t)) => {
  switch (
    End.Map.find_opt(
      source,
      switch (from) {
      | L => walk_l_map^
      | R => walk_r_map^
      },
    )
  ) {
  | Some(walks) => walks
  | None => End.Map.empty
  };
};

let enter_all_precompiled = (~from: Dir.t, sort: Mtrl.NT.t) => {
  switch (
    Mtrl.NT.Map.find_opt(
      sort,
      switch (from) {
      | L => enter_l_map^
      | R => enter_r_map^
      },
    )
  ) {
  | Some(walks) => walks
  | None => End.Map.empty
  };
};

let step = (~from: Dir.t, src: End.t, dst: End.t) =>
  Index.find(dst, step_all(~from, src));
let lt =
  Memo.general(((l: End.t, r: End.t)) =>
    List.filter(Walk.is_neq, step(~from=L, l, r))
  )
  |> Funs.curry;
let gt =
  Memo.general(((l: End.t, r: End.t)) =>
    List.filter(Walk.is_neq, step(~from=R, r, l))
  )
  |> Funs.curry;
let eq =
  Memo.general(((l: End.t, r: End.t)) =>
    List.filter(Walk.is_eq, step(~from=L, l, r))
  )
  |> Funs.curry;

// todo: combine from and src
let walk = (~from: Dir.t, src: End.t, dst: End.t) =>
  Index.find(dst, walk_all_precompiled(~from, src));
let walk_eq = (~from: Dir.t, src: End.t, dst: End.t) =>
  List.filter(Walk.is_eq, walk(~from, src, dst));
let walk_neq = (~from: Dir.t, src: End.t, dst: End.t) =>
  List.filter(Walk.is_neq, walk(~from, src, dst));

let enter = (~from: Dir.t, sort: Mtrl.NT.t, dst: End.t) =>
  Index.find(dst, enter_all_precompiled(~from, sort));

let exit = (~from: Dir.t, src: End.t) =>
  List.filter(Walk.is_eq, walk(~from, src, Root));
