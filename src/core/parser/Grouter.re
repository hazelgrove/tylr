open Stds;

let split_bins = (num_bins, xs) => {
  open Lists.Syntax;
  let return = bins => return(List.rev(bins));
  let rec go = (~bins=[], num_bins, xs) =>
    switch (xs) {
    | _ when num_bins <= 0 => []
    | _ when num_bins == 1 => return([xs, ...bins])
    | [] => return(List.init(num_bins, Fun.const([])) @ bins)
    | [_, ..._] =>
      let* bin_size =
        Lists.range(~start=`inclusive, ~stop=`inclusive, 0, List.length(xs));
      let (bin, rest) = Lists.split_n(xs, bin_size);
      go(~bins=[bin, ...bins], num_bins - 1, rest);
    };
  go(num_bins, xs);
};

module Cells = {
  type t = Chain.t(Cell.t, unit);
  // let mk = (~l=?, ~r=?, cells) => {padding: (l, r), cells};
  let unit: _ => t = Chain.unit;
  let empty = unit(Cell.empty);
  let rev = Chain.rev(~rev_loop=Fun.id, ~rev_link=Fun.id);
  let is_unit =
    fun
    | ([c], []) => Some(c)
    | _ => None;
  let hd = Chain.hd;
  let cat = (l: t, r: t) => Chain.append(l, (), r);
  let concat = (css: list(t)) =>
    switch (Lists.Framed.ft(css)) {
    | None => empty
    | Some((pre, ft)) => List.fold_left(Fun.flip(cat), ft, pre)
    };
  // combine adjacent space cells except for those on the ends
  let squash = (~save_padding=false, cs: t) => {
    cs
    |> Chain.mapi_loop((i, c) => (i, c))
    |> Chain.fold_right(
         ((i, c), (), acc) =>
           switch (acc) {
           | _ when !save_padding && Cell.Space.is_space(c) =>
             Chain.map_hd(Cell.pad(~l=c), acc)
           | (_, [_, ..._]) when i != 0 && Cell.Space.is_space(c) =>
             Chain.map_hd(Cell.pad(~l=c), acc)
           | _ => Chain.link(c, (), acc)
           },
         ((_, c)) => unit(c),
       );
  };

  let face = (~side: Dir.t, cs: t) => {
    let cs = Dir.pick(side, (Fun.id, rev), cs);
    switch (Chain.unlink(cs)) {
    | Ok((c, (), cs)) when Cell.Space.is_space(c) =>
      let c = Chain.hd(cs);
      Cell.face(~side, c);
    | Ok((c, _, _))
    | Error(c) => Cell.face(~side, c)
    };
  };

  let is_space = (cs: t) =>
    List.for_all(Cell.Space.is_space, Chain.loops(cs));
  let is_empty = (cs: t) => List.for_all(Cell.is_empty, Chain.loops(cs));

  let split_padding = (cs: t) => {
    let cs = Chain.loops(cs);
    let (l, cs) =
      switch (cs) {
      | [c, ...cs] when Cell.Space.is_space(c) => (c, cs)
      | _ => (Cell.empty, cs)
      };
    let (cs, r) =
      switch (Lists.Framed.ft(cs)) {
      | Some((cs, c)) when Cell.Space.is_space(c) => (List.rev(cs), c)
      | _ => (cs, Cell.empty)
      };
    (l, cs, r);
  };

  // todo: just use lists
  let to_list = cs =>
    switch (Chain.unlink(cs)) {
    | Error(c) when Cell.is_empty(~require_unmarked=true, c) => []
    | _ => Chain.loops(cs)
    };
  let of_list =
    fun
    | [] => empty
    | [_, ..._] as cs =>
      Chain.mk(cs, List.init(List.length(cs) - 1, Fun.const()));
};

let rec degrout = (c: Cell.t): Cells.t =>
  switch (Cell.get(c)) {
  | Some(M(_, w, _) as m) when Option.is_some(Wald.is_grout(w)) =>
    Meld.to_chain(m)
    |> Chain.loops
    |> List.map(degrout)
    |> Cells.concat
    |> Cells.squash(~save_padding=true)
  | _ => Chain.unit(c)
  };

// output Some(b) if bounded, where b indicates whether pre/post grout needed
let is_bounded = (cs: Cells.t, nt: Mtrl.NT.t, ~from: Dir.t): option(bool) =>
  switch (Cells.face(~side=from, cs)) {
  | None => Some(false)
  | Some(f) =>
    Walker.enter(~from=L, nt, Node(f))
    |> Lists.hd
    |> Option.map(w => Walk.height(w) > 1)
  };

let default =
  fun
  | Mtrl.Space(_) => Cell.empty
  // grout case isn't quite right... but shouldn't arise
  | Grout(s)
  | Tile((s, _)) => Cell.put(Meld.Grout.op_(s));

// assumes cs already squashed sans padding
let regrout_swing = (cs: Cells.t, sw: Walk.Swing.t, ~from: Dir.t) => {
  let (bot, top) = Walk.Swing.(bot(sw), top(sw));
  switch (bot) {
  | Space(fillable) =>
    let squashed = Cells.squash(cs);
    (fillable ? Cells.is_space : Cells.is_empty)(squashed)
      ? Some(Cells.hd(squashed)) : None;
  | Grout(s)
  | Tile((s, _)) =>
    open Options.Syntax;
    let (nt_l, nt_r) =
      Walk.Swing.is_eq(sw) ? (bot, bot) : Dir.order(from, (top, bot));
    let+ has_pre = is_bounded(cs, nt_l, ~from=L)
    and+ has_pos = is_bounded(cs, nt_r, ~from=R);
    switch (Cells.split_padding(cs)) {
    | (l, [], r) => Cell.pad(~l, default(bot), ~r)
    | (l, [_, ..._] as cs, r) =>
      let cells =
        cs
        |> (has_pre ? List.cons(l) : Lists.map_hd(Cell.pad(~l)))
        |> (has_pos ? Lists.snoc(r) : Lists.map_ft(Cell.pad(~r)));
      let toks =
        Token.Grout.[
          has_pre ? [pre(s)] : [],
          List.init(List.length(cs) - 1, _ => in_(s)),
          has_pos ? [pos(s)] : [],
        ]
        |> List.concat;
      let chain = Chain.mk(cells, toks);
      switch (Chain.unlink(chain)) {
      | Error(c) => c
      | Ok(_) => Cell.put(Meld.of_chain(chain))
      };
    };
  };
};

let regrout_swings =
    (
      ~to_zero=true,
      ~from: Dir.t,
      cells: list(Cell.t),
      swings: list(Walk.Swing.t),
    ) =>
  cells
  |> List.map(degrout)
  |> Cells.concat
  |> Cells.to_list
  |> split_bins(List.length(swings))
  |> Oblig.Delta.minimize(~to_zero, c_bins =>
       List.combine(c_bins, swings)
       |> List.map(((c_bin, sw)) => {
            open Options.Syntax;
            let+ c = regrout_swing(Cells.of_list(c_bin), sw, ~from);
            (sw, c);
          })
       |> Options.for_all
     );

let mk_stance = (st: Walk.Stance.t) => {
  let tok = Token.mk(st);
  Effects.perform(Insert(tok));
  tok;
};
let mk_stances = stances => stances |> List.map(mk_stance) |> Option.some;

let regrout = (~to_zero, ~from, cs, (swings, stances): Walk.t) => {
  open Options.Syntax;
  let* cs = regrout_swings(~to_zero, ~from, cs, swings);
  let+ toks = Oblig.Delta.minimize(~to_zero, mk_stances, [stances]);
  Chain.mk(cs, toks);
};

let pick_regrout =
    (~to_zero=true, ~from: Dir.t, cs: list(Cell.t), ws: list(Walk.t)) =>
  Oblig.Delta.minimize(~to_zero, regrout(~to_zero, ~from, cs), ws);
