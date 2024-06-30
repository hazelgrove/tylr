open Stds;

// let fill_space = (fill: Fill.t, fillable: Space.NT.t)({});

// assumes precedence-correctness already checked
// and that fill has been oriented
let fill_ = (~l=false, ~r=false, fill: Fill.t, nt: Mtrl.NT.t): option(Cell.t) => {
  let invalid = Invalid_argument("Baker.fill");
  switch (nt) {
  | Grout(_) => failwith("todo: fill grout")
  | Space(false) => Fill.is_empty(fill) ? Some(Cell.empty) : None
  | Space(true) =>
    let spc = Options.get_exn(invalid, Fill.is_space(fill));
    Fill.default(nt)
    |> Fill.pad(~side=L, ~spc)
    |> Options.get_fail("todo: shouldn't be possible")
    |> Option.some;
  | Tile((s, _)) =>
    switch (Fill.is_space(fill)) {
    | Some(spc) =>
      Fill.default(nt)
      |> Fill.pad(~side=L, ~spc)
      |> Options.get_fail("todo: shouldn't be possible")
      |> Option.some
    | None =>
      let cells =
        [l ? [Cell.empty] : [], Fill.to_list(fill), r ? [Cell.empty] : []]
        |> List.concat;
      let toks =
        Token.Grout.[
          l ? [pre(s)] : [],
          List.init(Fill.length(fill) - 1, _ => in_(s)),
          r ? [pos(s)] : [],
        ]
        |> List.concat;
      switch (toks) {
      | [] => Some(List.hd(cells))
      | [_, ..._] => Some(Cell.put(Meld.of_chain(Chain.mk(cells, toks))))
      };
    }
  };
};

let has_intermediate = (w: Walk.t) => Walk.height(w) > 1;
let is_bounded = (l: Mtrl.NT.t, fill: Fill.t, r: Mtrl.NT.t) => {
  open Options.Syntax;
  let+ l_has_intermediate =
    switch (Fill.face(~side=L, fill)) {
    | None => Some(false)
    | Some(f_l) =>
      Walker.enter(~from=L, l, Node(f_l))
      |> Walk.Set.min_elt_opt
      |> Option.map(has_intermediate)
    }
  and+ r_has_intermediate =
    switch (Fill.face(~side=R, fill)) {
    | None => Some(false)
    | Some(f_r) =>
      Walker.enter(~from=R, r, Node(f_r))
      |> Walk.Set.min_elt_opt
      |> Option.map(has_intermediate)
    };
  (l_has_intermediate, r_has_intermediate);
};

let bake_swing =
    (~fill=Fill.empty, ~from: Dir.t, sw: Walk.Swing.t): option(Cell.t) => {
  open Options.Syntax;
  let fill = fill |> Dir.pick(from, (Fun.id, Fill.rev)) |> Fill.squash;
  let (bot, top) = Walk.Swing.(bot(sw), top(sw));
  let (bound_l, bound_r) =
    Walk.Swing.is_eq(sw) ? (bot, bot) : Dir.order(from, (top, bot));
  let* (l, r) = is_bounded(bound_l, fill, bound_r);
  fill_(~l, fill, bot, ~r);
};

let mk_token = (t: Mtrl.T.t) => {
  let tok = Token.mk(t);
  Effects.perform(Insert(tok));
  tok;
};

let bake = (~from: Dir.t, ~fill=Fill.empty, w: Walk.t): option(Baked.t) =>
  w
  |> Chain.map_link(mk_token)
  |> Chain.unzip_loops
  // choose swing to fill that minimizes obligations.
  // currently simply chooses a single swing to fill even when there are
  // multiple fill elements. ideally this choice would distribute multiple
  // melds across multiple swings.
  |> Oblig.Delta.minimize(((pre, sw: Walk.Swing.t, suf)) => {
       open Options.Syntax;
       let bake_tl = ((toks, strs)) =>
         List.combine(toks, strs)
         |> List.map(((tok, sw)) => {
              let+ cell = bake_swing(~from, sw);
              (tok, (sw, cell));
            })
         |> Options.for_all
         |> Option.map(List.split);
       let+ cell = bake_swing(~fill, ~from, sw)
       and+ pre = bake_tl(pre)
       and+ suf = bake_tl(suf);
       Chain.zip(~pre, (sw, cell), ~suf);
     });

let bake_sans_fill = (~from: Dir.t, w: Walk.t) =>
  bake(~from, w)
  |> Options.get_fail("bug: expected bake to succeed sans fill");
