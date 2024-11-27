open Stds;

exception Bug__failed_to_push_space;

let dbg = ref(true);

// assumes w is already oriented toward side.
// used to complete zigg top when it takes precedence over pushed wald.
let complete_wald = (~side: Dir.t, ~fill=Cell.empty, w: Wald.t): Terr.t => {
  let from = Dir.toggle(side);
  let exited = Walker.exit(~from, Node(Wald.face(w).mtrl));
  let grouted = Grouter.pick(~repair=true, ~from, [fill], exited);
  switch (grouted) {
  | Some(grouted) => Grouted.complete_wald(grouted, w)
  | None =>
    assert(!Cell.is_empty(fill));
    print_endline("warning: dropping fill " ++ Cell.show(fill));
    let baked =
      Grouter.pick(~repair=true, ~from, [], exited)
      |> Options.get_fail("bug: expected grouter to succeed sans fill");
    Grouted.complete_wald(baked, w);
  };
};
// onto confusing here when considered alone, same onto piped from push(~onto)
let complete_terr = (~onto: Dir.t, ~fill=Cell.empty, terr: Terr.t): Cell.t => {
  let orient = Dir.pick(onto, (Meld.rev, Fun.id));
  let exited = Walker.exit(~from=onto, Node(Terr.face(terr).mtrl));
  let grouted = Grouter.pick(~repair=true, ~from=onto, [fill], exited);
  // if (dbg^) {
  //   P.log("--- Melder.complete_terr");
  //   P.show("onto", Dir.show(onto));
  //   P.show("fill", Cell.show(fill));
  //   P.show("terr", Terr.show(terr));
  // };
  switch (grouted) {
  | Some(grouted) =>
    let m = Grouted.complete_terr(grouted, terr);
    // if (dbg^) {
    //   P.log("--- Melder.complete_terr/grouted");
    //   P.show("grouted", Grouted.show(grouted));
    //   P.show("completed meld", Meld.show(m));
    //   P.show("oriented meld", Meld.show(orient(m)));
    //   Cell.dbg := true;
    //   P.show("oriented cell", Cell.show(Cell.put(orient(m))));
    //   Cell.dbg := false;
    // };
    Cell.put(orient(m));
  | None =>
    assert(!Cell.is_empty(fill));
    print_endline("warning: dropping fill " ++ Cell.show(fill));
    // walker bug if no exits
    // let exited = List.hd(exited);
    let grouted =
      Grouter.pick(~repair=true, ~from=onto, [], exited)
      |> Options.get_fail("bug: expected grouter to succeed sans fill");
    Cell.put(orient(Grouted.complete_terr(grouted, terr)));
  };
};
let complete_slope = (~onto: Dir.t, ~fill=Cell.empty) =>
  Slope.fold(fill => complete_terr(~onto, ~fill), fill);

let complete_bounded =
    (~bounds as (l, r), ~onto: Dir.t, ~fill=Cell.empty, slope) => {
  // from/onto terminology here very confusing...
  let (bd_onto, bd_from) = Dir.order(onto, (l, r));
  let fill = complete_slope(~onto, ~fill, slope);
  let fc_onto = bd_onto |> Bound.map(t => Terr.face(t).mtrl);
  let fc_from = bd_from |> Bound.map(t => Terr.face(t).mtrl);
  // if (dbg^) {
  //   P.log("--- Melder.complete_bounded");
  //   P.show("completed slope", Cell.show(fill));
  // };
  Walker.walk_eq(~from=onto, fc_onto, fc_from)
  |> Grouter.pick(~repair=true, [fill], ~from=onto)
  |> Option.map(grouted => snd(Chain.hd(grouted)))
  |> Options.get_fail("hmmm");
};

let connect_eq =
    (
      ~repair=false,
      ~onto as d: Dir.t,
      onto: Terr.t,
      ~fill=Cell.empty,
      t: Token.t,
    )
    : option((Grouted.t, Terr.t)) => {
  open Options.Syntax;
  let rec go = (onto: Terr.t, fill) => {
    let/ () = repair ? rm_ghost_and_go(onto, fill) : None;
    let face = Terr.face(onto).mtrl;
    Walker.walk_eq(~from=d, Node(face), Node(t.mtrl))
    |> Grouter.pick(~repair, ~from=d, List.rev(fill))
    |> Option.map(grouted => (grouted, onto));
  }
  and rm_ghost_and_go = (onto, fill) =>
    switch (Terr.unlink(onto)) {
    | (hd, cell, Some(tl))
        when Option.is_some(Token.Tile.is_ghost(~require_empty=true, hd)) =>
      go(tl, [cell, ...fill]) |> Effects.perform_if(Remove(hd))
    | _ => None
    };
  go(onto, [fill]);
};
let connect_neq =
    (
      ~repair=false,
      ~onto as d: Dir.t,
      onto: Bound.t(Terr.t),
      ~fill=Cell.empty,
      t: Token.t,
    )
    : option(Grouted.t) => {
  let face = onto |> Bound.map(t => Terr.face(t).mtrl);
  Walker.walk_neq(~from=d, face, Node(t.mtrl))
  |> Grouter.pick(~repair, ~from=d, [fill]);
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
    : option((Grouted.t, Bound.t(Terr.t))) => {
  let eq = () =>
    Bound.to_opt(onto)
    |> Options.bind(~f=onto => connect_eq(~repair, ~onto=d, onto, ~fill, t))
    |> Option.map(((grouted, terr)) => (grouted, Bound.Node(terr)));
  let neq = () =>
    connect_neq(~repair, ~onto=d, onto, ~fill, t)
    |> Option.map(grouted => (grouted, onto));
  if (repair) {
    open Options.Syntax;
    // if repairing, then this means we're molding/remolding and our push of the
    // current candidate token has reached the top of the local stack ie the nearest
    // bidelimited container. prioritize maintaining the current bidelimited
    // container if possible.
    let/ () = neq();
    eq();
  } else {
    Oblig.Delta.minimize(~to_zero=true, f => f(), [eq, neq]);
  };
};

let connect =
    (
      ~repair=false,
      ~onto as d: Dir.t,
      onto: Terr.t,
      ~fill=Cell.empty,
      t: Token.t,
    )
    : Result.t((Grouted.t, Terr.t), Cell.t) => {
  let b = Dir.toggle(d);
  let eq = () =>
    connect_eq(~repair, ~onto=d, onto, ~fill, t) |> Option.map(Result.ok);
  let neq_d = () =>
    connect_neq(~repair, ~onto=d, Node(onto), ~fill, t)
    |> Option.map(grouted => {(grouted, onto)})
    |> Option.map(Result.ok);
  let neq_b = () => {
    let (hd, _tl) = Wald.uncons(onto.wald);
    // we call connect_neq in b direction for the purpose of emitting token effects
    // for subsequent oblig minimization, but don't need the result
    connect_neq(~repair, ~onto=b, Node(Terr.of_tok(t)), ~fill, hd)
    |> Option.map(_ => complete_terr(~onto=d, ~fill, onto))
    |> Option.map(Result.err);
  };
  // ensure consistent ordering
  let neqs = Dir.pick(d, ([neq_d, neq_b], [neq_b, neq_d]));
  [eq, ...neqs]
  |> Oblig.Delta.minimize(~to_zero=!repair, f => f())
  // use get here instead of value to avoid spurious effects.
  // default value covers incomparability.
  |> Options.get(() => Error(complete_terr(~onto=d, ~fill, onto)));
};

let rec unzip_tok = (~frame=Frame.Open.empty, path: Path.t, cell: Cell.t) => {
  let m = Cell.get(cell);
  switch (path) {
  | [] => raise(Marks.Invalid)
  | [hd, ...tl] =>
    let m = Options.get_exn(Marks.Invalid, m);
    switch (Meld.unzip(hd, m)) {
    | Loop((pre, cell, suf)) =>
      unzip_tok(~frame=Frame.Open.add((pre, suf), frame), tl, cell)
    | Link((pre, tok, suf)) =>
      let (cell_pre, pre) = Chain.uncons(pre);
      let (cell_suf, suf) = Chain.uncons(suf);
      let (cell_pre, dn) = Slope.Dn.unroll(cell_pre);
      let (cell_suf, up) = Slope.Up.unroll(cell_suf);
      let frame =
        frame |> Frame.Open.add((pre, suf)) |> Frame.Open.cat((dn, up));
      ((cell_pre, tok, cell_suf), frame);
    };
  };
};

let rec push =
        (
          ~repair=?,
          t: Token.t,
          ~fill=Cell.empty,
          stack: Stack.t,
          ~onto: Dir.t,
        )
        : option((Grouted.t, Stack.t)) => {
  let r = Option.is_some(repair);
  switch (stack.slope) {
  | [] =>
    connect_ineq(~repair=r, ~onto, stack.bound, ~fill, t)
    |> Option.map(((grouted, bound)) =>
         (grouted, Stack.{slope: [], bound})
       )
  | [hd, ...tl] =>
    let connect = () =>
      switch (connect(~repair=r, ~onto, hd, ~fill, t)) {
      | Error(fill) =>
        push(~repair?, t, ~fill, {...stack, slope: tl}, ~onto)
      | Ok((grouted, hd)) =>
        Some((grouted, {...stack, slope: [hd, ...tl]}))
      };
    switch (repair) {
    | None => connect()
    | Some(remold) =>
      let discharge = () => discharge(~remold, stack, ~fill, t);
      Oblig.Delta.minimize(f => f(), [discharge, connect]);
    };
  };
}
and discharge = (~remold, stack: Stack.t, ~fill=Cell.empty, t: Token.t) => {
  switch (stack.slope) {
  | [] => None
  | [hd, ...tl] =>
    open Options.Syntax;
    let* (path, _) =
      hd.cell.marks.obligs
      |> Path.Map.filter((_, mtrl: Mtrl.T.t) =>
           switch (mtrl) {
           | Tile(_) when mtrl == t.mtrl => true
           | _ => false
           }
         )
      |> Path.Map.max_binding_opt;
    let ((c_l, tok, c_r), (dn, up)) = unzip_tok(path, hd.cell);
    Effects.remove(tok);
    // let l = Stack.cat(dn, {...stack, slope: tl});
    let l = {
      let bound =
        switch (tl) {
        | [] => stack.bound
        | [t, ..._] => Node(t)
        };
      Stack.mk(bound, ~slope=dn);
    };
    let r = {
      let (c_fill, up_fill) = Slope.Up.unroll(fill);
      let slope =
        up @ [Terr.of_wald(Wald.rev(hd.wald), ~cell=c_fill), ...up_fill];
      Stack.{slope, bound: Node(Terr.of_tok(t))};
    };
    let c = Cell.Space.merge(c_l, ~fill=Cell.dirty, c_r);
    let* (slope, fill) = Result.to_option(remold(~fill=c, (l, r)));
    let stack = Stack.cat(slope, {...stack, slope: tl});
    push(~repair=remold, t, ~fill, stack, ~onto=L);
  };
};
