open Stds;

let bake_eq = (~fill=Fill.empty, sort: Mtrl.NT.t): option(Cell.t) => {
  open Options.Syntax;
  let+ l =
    switch (Fill.face(~side=L, fill)) {
    | None => Some(false)
    | Some(f_l) =>
      Walker.enter(~from=L, sort, Node(f_l))
      |> Walk.Set.min_elt_opt
      |> Option.map(w => Walk.height(w) > 1)
    }
  and+ r =
    switch (Fill.face(~side=R, fill)) {
    | None => Some(false)
    | Some(f_r) =>
      Walker.enter(~from=R, sort, Node(f_r))
      |> Walk.Set.min_elt_opt
      |> Option.map(w => Walk.height(w) > 1)
    };
  Fill.fill(~l, fill, sort, ~r);
};

let bake_lt =
    (~fill=Fill.empty, bound: Mtrl.NT.t, sort: Mtrl.NT.t): option(Cell.t) => {
  open Options.Syntax;
  let+ _l =
    switch (Fill.face(~side=L, fill)) {
    | None => Some(false)
    | Some(f_l) =>
      Walker.enter(~from=L, bound, Node(f_l))
      |> Walk.Set.min_elt_opt
      |> Option.map(w => Walk.height(w) > 1)
    }
  and+ r =
    switch (Fill.face(~side=R, fill)) {
    | None => Some(false)
    | Some(f_r) =>
      Walker.enter(~from=R, sort, Node(f_r))
      |> Walk.Set.min_elt_opt
      |> Option.map(w => Walk.height(w) > 1)
    };
  Fill.fill(fill, sort, ~r);
};

let bake_gt =
    (~fill=Fill.empty, sort: Mtrl.NT.t, bound: Mtrl.NT.t): option(Cell.t) => {
  open Options.Syntax;
  let+ l =
    switch (Fill.face(~side=L, fill)) {
    | None => Some(false)
    | Some(f_l) =>
      Walker.enter(~from=L, sort, Node(f_l))
      |> Walk.Set.min_elt_opt
      |> Option.map(w => Walk.height(w) > 1)
    }
  and+ _r =
    switch (Fill.face(~side=R, fill)) {
    | None => Some(false)
    | Some(f_r) =>
      Walker.enter(~from=R, bound, Node(f_r))
      |> Walk.Set.min_elt_opt
      |> Option.map(w => Walk.height(w) > 1)
    };
  Fill.fill(~l, fill, sort);
};

let bake_swing =
    (~fill=Fill.empty, ~from: Dir.t, sw: Walk.Swing.t): option(Cell.t) => {
  let fill = Dir.pick(from, (Fun.id, Fill.rev), fill);
  switch (from) {
  | _ when Walk.Swing.height(sw) <= 1 => bake_eq(~fill, Walk.Swing.bot(sw))
  | L => bake_lt(~fill, Walk.Swing.top(sw), Walk.Swing.bot(sw))
  | R => bake_gt(~fill, Walk.Swing.bot(sw), Walk.Swing.top(sw))
  };
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
