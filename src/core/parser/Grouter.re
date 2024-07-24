open Stds;

module Cells = {
  type t = list(Cell.t);

  let face = (~side: Dir.t, cs: t) =>
    switch (Dir.pick(side, (Fun.id, List.rev), cs)) {
    | [] => None
    | [c, next, ..._] when Cell.Space.is_space(c) => Cell.face(~side, next)
    | [c, ..._] => Cell.face(~side, c)
    };

  // combine adjacent space cells except for those on the ends
  let squash = (~save_padding=false, cs: t) =>
    switch (cs |> List.mapi((i, c) => (i, c)) |> Lists.Framed.ft) {
    | None => []
    | Some((pre, (_, ft))) =>
      pre
      |> List.fold_left(
           (acc, (i, c)) =>
             switch (acc) {
             | _ when !save_padding && Cell.Space.is_space(c) =>
               Lists.map_hd(Cell.pad(~l=c), acc)
             | [_, _, ..._] when i != 0 && Cell.Space.is_space(c) =>
               Lists.map_hd(Cell.pad(~l=c), acc)
             | _ => [c, ...acc]
             },
           [ft],
         )
    };

  let split_padding = (cs: list(Cell.t)) => {
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

  // output Some(b) if bounded, where b indicates whether pre/post grout needed
  let are_bounded = (cs: t, nt: Mtrl.NT.t, ~from: Dir.t): option(bool) =>
    switch (face(~side=from, cs)) {
    | None => Some(false)
    | Some(f) =>
      Walker.enter(~from=L, nt, Node(f))
      |> Lists.hd
      |> Option.map(w => Walk.height(w) > 1)
    };
};

let mk_stance = (st: Walk.Stance.t) => {
  let tok = Token.mk(st);
  Effects.perform(Insert(tok));
  tok;
};
let mk_stances = stances => stances |> List.map(mk_stance) |> Option.some;

let rec degrout = (c: Cell.t): Cells.t =>
  switch (Cell.get(c)) {
  | Some(M(_, w, _) as m) when Option.is_some(Wald.is_grout(w)) =>
    Meld.to_chain(m)
    |> Chain.loops
    |> List.concat_map(degrout)
    |> Cells.squash(~save_padding=true)
  | _ => [c]
  };

let default_cell =
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
    let valid =
      fillable ? Cell.Space.is_space : Cell.is_empty(~require_unmarked=false);
    List.for_all(valid, squashed)
      ? Lists.hd(squashed)
        |> Option.value(~default=Cell.empty)
        |> Option.some
      : None;
  | Grout(s)
  | Tile((s, _)) =>
    open Options.Syntax;
    let (nt_l, nt_r) =
      Walk.Swing.is_eq(sw) ? (bot, bot) : Dir.order(from, (top, bot));
    let+ has_pre = Cells.are_bounded(cs, nt_l, ~from=L)
    and+ has_pos = Cells.are_bounded(cs, nt_r, ~from=R);
    switch (Cells.split_padding(cs)) {
    | (l, [], r) => Cell.pad(~l, default_cell(bot), ~r)
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
      ~repair=false,
      ~from: Dir.t,
      cells: list(Cell.t),
      swings: list(Walk.Swing.t),
    ) =>
  cells
  |> (repair ? List.concat_map(degrout) : Fun.id)
  |> Lists.split_bins(List.length(swings))
  |> Oblig.Delta.minimize(~to_zero=!repair, c_bins =>
       List.combine(c_bins, swings)
       |> List.map(((c_bin, sw)) => {
            open Options.Syntax;
            let+ c = regrout_swing(c_bin, sw, ~from);
            (sw, c);
          })
       |> Options.for_all
     );

let regrout = (~repair, ~from, cs, (swings, stances): Walk.t) => {
  open Options.Syntax;
  let* cs = regrout_swings(~repair, ~from, cs, swings);
  let+ toks = Oblig.Delta.minimize(~to_zero=!repair, mk_stances, [stances]);
  Chain.mk(cs, toks);
};

let pick = (~repair=false, ~from: Dir.t, cs: list(Cell.t), ws: list(Walk.t)) =>
  Oblig.Delta.minimize(~to_zero=!repair, regrout(~repair, ~from, cs), ws);
