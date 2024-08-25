open Stds;

let rec split_cell_padding = (~side: Dir.t, c: Cell.t) =>
  switch (Cell.get(c)) {
  | None => Cell.(empty, c, empty)
  | Some(M(l, w, r) as m) when Option.is_some(Meld.Space.get(m)) =>
    switch (side) {
    | L =>
      switch (Wald.unlink(w)) {
      | Error(_) => Cell.(c, empty, empty)
      | Ok((spc, c, rest)) =>
        // pull off c with padding in case it has caret to simplify subsequent
        // decision making about where to insert grout relative to caret
        let pad_l = Cell.put(Meld.of_tok(~l, spc, ~r=c));
        let rest = Cell.put(Meld.mk(rest, ~r));
        (pad_l, rest, Cell.empty);
      }
    | R =>
      switch (Wald.unlink(Wald.rev(w))) {
      | Error(_) => Cell.(empty, empty, c)
      | Ok((spc, c, rest)) =>
        let rest = Cell.put(Meld.mk(~l, Wald.rev(rest)));
        let pad_r = Cell.put(Meld.of_tok(~l=c, spc, ~r));
        (Cell.empty, rest, pad_r);
      }
    }
  | Some(M(l, w, r)) =>
    switch (side) {
    | L =>
      let (p_l, l, _) = split_cell_padding(~side=L, l);
      Cell.(p_l, put(M(l, w, r)), empty);
    | R =>
      let (_, r, p_r) = split_cell_padding(r, ~side=R);
      Cell.(empty, put(M(l, w, r)), p_r);
    }
  };

module Cells = {
  type t = list(Cell.t);

  let cons = (c: Cell.t, cs: t) =>
    Cell.is_empty(~require_unmarked=true, c) ? cs : [c, ...cs];

  let face = (~side: Dir.t, cs: t) =>
    switch (Dir.pick(side, (Fun.id, List.rev), cs)) {
    | [] => None
    | [c, next, ..._] when Cell.Space.is_space(c) => Cell.face(~side, next)
    | [c, ..._] => Cell.face(~side, c)
    };

  // combine adjacent space cells except for those on the ends
  // if save_padding=true
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

  // this gets called when preparing cells to fill a chosen swing.
  let split_padding = (cs: list(Cell.t)) => {
    // note: splitting padding from left before right means eg when cs == [c]
    // where c holds the meld {} " " {|} " " {}, the caret | will be pulled
    // left side of any grout inserted between the spaces, which is afaict always
    // what we want after any modification (except maybe forward delete)
    let (l, cs) =
      switch (cs) {
      | [c, ...cs] =>
        let (l, c, _) = split_cell_padding(~side=L, c);
        (l, cons(c, cs));
      | _ => (Cell.empty, cs)
      };
    let (cs, r) =
      switch (Lists.Framed.ft(cs)) {
      | Some((cs, c)) =>
        let (_, c, r) = split_cell_padding(c, ~side=R);
        (List.rev(cons(c, cs)), r);
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

let bake_stance = (st: Walk.Stance.t) => {
  let tok = Token.mk(st);
  Effects.perform(Insert(tok));
  tok;
};
let bake_stances = stances => stances |> List.map(bake_stance) |> Option.some;

let rec degrout = (c: Cell.t): Cells.t =>
  switch (Cell.get(c)) {
  | Some(M(l, w, r)) when Option.is_some(Wald.is_grout(w)) =>
    let W((toks, cells)) = w;
    List.iter(Effects.remove, toks);
    // if there is leftover padding at the ends, pad it onto the inner cells
    // (or, in the case of convex grout, merge the padding into a single cell)
    // and pull off this padding later if grout is reinserted, the goal being
    // to maximally stabilize grout positioning
    let cells_l =
      switch (cells) {
      | [hd, ...tl] when Cell.Space.is_space(l) => [
          Cell.pad(~squash=false, ~l, hd),
          ...tl,
        ]
      | _ => [l, ...cells]
      };
    let cells_lr =
      switch (Lists.Framed.ft(cells_l)) {
      | Some((pre, ft)) when Cell.Space.is_space(r) =>
        Lists.Framed.put_ft(pre, Cell.pad(~squash=false, ft, ~r))
      | _ => cells_l @ [r]
      };
    List.concat_map(degrout, cells_lr);
  | _ => [c]
  };

let fill_default =
  fun
  | Mtrl.Space(_) => Cell.dirty
  // grout case isn't quite right... but shouldn't arise
  | Grout(s)
  | Tile((s, _)) =>
    Cell.put(
      Meld.of_tok(
        ~l=Cell.dirty,
        Effects.insert(Token.Grout.op_(s)),
        ~r=Cell.dirty,
      ),
    );

// assumes cs already squashed sans padding
let fill_swing = (cs: Cells.t, sw: Walk.Swing.t, ~from: Dir.t) => {
  let cs = Dir.pick(from, (List.rev, Fun.id), cs);
  let (bot, top) = Walk.Swing.(bot(sw), top(sw));
  switch (bot) {
  | Space(nt) =>
    let squashed = Cells.squash(cs);
    let valid =
      nt == Open
        ? Cell.Space.is_space : Cell.is_empty(~require_unmarked=false);
    List.for_all(valid, squashed)
      ? Lists.hd(squashed)
        |> Option.value(~default=Cell.dirty)
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
    | (l, cs, r) when List.for_all(Cell.Space.is_space, cs) =>
      // prioritize getting any carets in cs over to the left for now.
      // todo: parametrize this based on parsing mode
      let l = List.hd(Cells.squash([l, ...cs]));
      // let r = List.hd(Cells.squash(cs @ [r]));
      Cell.pad(~l, fill_default(bot), ~r);
    | (l, cs, r) =>
      // todo: need to do some degrouting here now that degrout pass no longer
      // handles convex grout
      let cells =
        cs
        |> (has_pre ? List.cons(l) : Lists.map_hd(Cell.pad(~l)))
        |> (has_pos ? Lists.snoc(r) : Lists.map_ft(Cell.pad(~r)));
      let toks =
        Token.Grout.[
          has_pre ? [Effects.insert(pre(s))] : [],
          List.init(List.length(cs) - 1, _ => Effects.insert(in_(s))),
          has_pos ? [Effects.insert(pos(s))] : [],
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

let fill_swings =
    (~repair, ~from, cells: list(Cell.t), swings: list(Walk.Swing.t)) =>
  cells
  |> Dir.pick(from, (List.rev, Fun.id))
  |> (repair ? List.concat_map(degrout) : Fun.id)
  |> Dir.pick(from, (List.rev, Fun.id))
  |> Lists.split_bins(List.length(swings))
  |> Oblig.Delta.minimize(~to_zero=!repair, c_bins =>
       List.combine(c_bins, swings)
       |> List.map(((c_bin, sw)) => {
            open Options.Syntax;
            let+ c = fill_swing(c_bin, sw, ~from);
            (sw, c);
          })
       |> Options.for_all
     );

let fill = (~repair, ~from, cs, (swings, stances): Walk.t) => {
  open Options.Syntax;
  let* cs = fill_swings(~repair, ~from, cs, swings);
  let+ toks =
    Oblig.Delta.minimize(~to_zero=!repair, bake_stances, [stances]);
  Chain.mk(cs, toks);
};

// pick a walk from ws that best accommodates the cells in cs, ie minimizes
// obligation delta. the given cells are expected to be oriented the same way as the
// given walks according to from.
let pick = (~repair=false, ~from: Dir.t, cs: list(Cell.t), ws: list(Walk.t)) =>
  Oblig.Delta.minimize(~to_zero=!repair, fill(~repair, ~from, cs), ws);
