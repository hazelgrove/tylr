open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;
open Stds;

let dbg = ref(false);

let rec split_cell_padding = (~side: Dir.t, c: Cell.t) =>
  switch (Cell.get(c)) {
  | None => Cell.(empty, c)
  | Some(m) when Option.is_some(Meld.Space.get(m)) =>
    Cell.Space.split(~side, c) |> Option.value(~default=(c, Cell.empty))
  | Some(M(l, w, r)) =>
    switch (side) {
    | L =>
      let (p_l, l) = split_cell_padding(~side=L, l);
      Cell.(p_l, put(M(l, w, r)));
    | R =>
      let (p_r, r) = split_cell_padding(r, ~side=R);
      Cell.(p_r, put(M(l, w, r)));
    }
  };

module Cells = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = list(Cell.t);

  let cons = (c: Cell.t, cs: t) =>
    Cell.is_empty(~require_unmarked=true, c) ? cs : [c, ...cs];

  let face = (~side: Dir.t, cs: t) =>
    switch (Dir.pick(side, (Fun.id, List.rev), cs)) {
    | [] => None
    | [c, next, ..._] when Cell.Space.is_space(c) => Cell.face(~side, next)
    | [c, ..._] => Cell.face(~side, c)
    };

  // combine adjacent space cells
  let squash = (cs: t) =>
    switch (cs |> Lists.Framed.ft) {
    | None => []
    | Some((pre, ft)) =>
      pre
      |> Lists.fold_left(~init=[ft], ~f=(acc, c) =>
           switch (acc) {
           | [hd, ...tl] when Cell.Space.is_space(hd) => [
               Cell.pad(c, ~r=hd),
               ...tl,
             ]
           | [hd, ...tl] when Cell.Space.is_space(c) => [
               Cell.pad(~l=c, hd),
               ...tl,
             ]
           | _ => [c, ...acc]
           }
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
        let (l, c) = split_cell_padding(~side=L, c);
        (l, cons(c, cs));
      | [] => (Cell.empty, cs)
      };
    let (cs, r) =
      switch (Lists.Framed.ft(cs)) {
      | Some((cs, c)) =>
        let (r, c) = split_cell_padding(~side=R, c);
        (List.rev(cons(c, cs)), r);
      | None => (cs, Cell.empty)
      };
    (l, squash(cs), r);
  };

  // output Some(b) if bounded, where b indicates whether pre/post grout needed
  let are_bounded = (cs: t, nt: Mtrl.NT.t, ~from: Dir.t): option(bool) =>
    switch (face(~side=from, cs)) {
    | None => Some(false)
    | Some(t) =>
      Walker.enter(~from, nt, Node(t.mtrl))
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
    // we wish to maximally stabilize grout positioning, ie grout that is removed
    // in this pass, if reinserted, should be reinserted in the same position.
    // we know that any cells within the wald w are not space, while the cells
    // l and r may be space. if either is space, pad them onto the inner cells (or
    // if no such inner cells, then merge l and r into a single space cell).
    // apply this padding with ~squash=false so that they can be pulled off later
    // when determining grout position.
    let cells_l =
      switch (cells) {
      | [hd, ...tl] when Cell.Space.is_space(l) =>
        let l = Cell.mark_degrouted(l, ~side=R);
        [Cell.pad(~squash=false, ~l, hd), ...tl];
      | [] when Cell.Space.is_space(l) =>
        let l = Cell.mark_degrouted(l, ~side=R);
        [l];
      | _ => [l, ...cells]
      };
    let cells_lr =
      switch (Lists.Framed.ft(cells_l)) {
      | Some((pre, ft)) when Cell.Space.is_space(r) =>
        let r = Cell.mark_degrouted(~side=L, r);
        Lists.Framed.put_ft(pre, Cell.pad(~squash=false, ft, ~r));
      | None when Cell.Space.is_space(r) =>
        let r = Cell.mark_degrouted(~side=L, r);
        [r];
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
  | Tile(((_, s), _)) =>
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
  if (dbg^) {
    P.log("--- Grouter.fill_swing");
    P.show("from", Dir.show(from));
    P.show("sw", Walk.Swing.show(sw));
    P.show("cs", Cells.show(cs));
  };
  let (bot, top) = Walk.Swing.(bot(sw), top(sw));
  switch (bot) {
  | Space(nt) =>
    if (dbg^) {
      P.log("--- Grouter.fill_swing/Space");
      P.show("nt", Space.NT.show(nt));
      P.show("cs", Cells.show(cs));
    };
    let squashed = Cells.squash(cs);
    if (dbg^) {
      P.show("squashed", Cells.show(squashed));
    };
    let valid =
      nt == Open
        ? Cell.Space.is_space : Cell.is_empty(~require_unmarked=false);
    List.for_all(valid, squashed)
      ? {
        let r =
          Lists.hd(squashed)
          |> Option.value(~default=Cell.dirty)
          |> Option.some;
        if (dbg^) {
          P.show("r", Fmt.to_to_string(Fmt.option(Cell.pp), r));
        };
        r;
      }
      : {
        None;
            // P.show("None", "None");
      };
  | Grout(s)
  | Tile(((_, s), _)) =>
    if (dbg^) {
      P.log("--- Grouter.fill_swing/Tile");
      P.show("s", Sort.show(s));
      P.show("cs", Cells.show(cs));
      // P.show("bot", Mtrl.NT.show(bot));
      // P.show("top", Mtrl.NT.show(top));
    };
    open Options.Syntax;
    let (nt_l, nt_r) =
      Walk.Swing.is_eq(sw) ? (bot, bot) : Dir.order(from, (top, bot));
    let+ has_pre = Cells.are_bounded(cs, nt_l, ~from=L)
    and+ has_pos = Cells.are_bounded(cs, nt_r, ~from=R);
    switch (Cells.split_padding(cs)) {
    | (l, cs, r) when List.for_all(Cell.Space.is_space, cs) =>
      if (dbg^) {
        P.log("--- Grouter.fill_swing/Tile/all space");
        P.show("l", Cell.show(l));
        P.show("cs", Cells.show(cs));
        P.show("r", Cell.show(r));
      };
      // prioritize getting any carets in cs over to the left for now.
      // todo: parametrize this based on parsing mode
      let l = List.hd(Cells.squash([l, ...cs]));
      // let r = List.hd(Cells.squash(cs @ [r]));
      let r = Cell.pad(~l, fill_default(bot), ~r);
      if (dbg^) {
        P.show("padded", Cell.show(r));
      };
      r;
    | (l, cs, r) =>
      if (dbg^) {
        P.log("--- Grouter.fill_swing/Tile/not all space");
        P.show("l", Cell.show(l));
        P.show("cs", Cells.show(cs));
        P.show("r", Cell.show(r));
      };
      let cells =
        cs
        |> (has_pre ? List.cons(l) : Lists.map_hd(Cell.pad(~l)))
        |> (has_pos ? Lists.snoc(r) : Lists.map_ft(Cell.pad(~r)));
      if (dbg^) {
        P.show("cells", Cells.show(cells));
      };
      let toks =
        Token.Grout.[
          has_pre ? [Effects.insert(pre(s))] : [],
          List.init(List.length(cs) - 1, _ => Effects.insert(in_(s))),
          has_pos ? [Effects.insert(pos(s))] : [],
        ]
        |> List.concat;
      let chain = Chain.mk(cells, toks);
      switch (Chain.unlink(chain)) {
      | Error(c) =>
        if (dbg^) {
          P.show("c", Cell.show(c));
        };
        c;
      | Ok(_) =>
        let r = Cell.put(Meld.of_chain(chain));
        if (dbg^) {
          P.show("r", Cell.show(r));
        };
        r;
      };
    };
  };
};

let fill_swings =
    (~repair, ~from, cells: list(Cell.t), swings: list(Walk.Swing.t)) => {
  if (dbg^) {
    P.log("--- Grouter.fill_swings");
    P.show("from", Dir.show(from));
    P.show("cells", Cells.show(Dir.pick(from, (List.rev, Fun.id), cells)));
    // P.show("swings", Fmt.to_to_string(Fmt.list(Walk.Swing.pp), swings));
  };
  cells
  |> Dir.pick(from, (List.rev, Fun.id))
  |> (repair ? List.concat_map(degrout) : Fun.id)
  // |> (dbg^ ? P.oshow("degrouted", Cells.show) : Fun.id)
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
};

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
let pick = (~repair=false, ~from: Dir.t, cs: list(Cell.t), ws: list(Walk.t)) => {
  // if (dbg^) {
  //   P.log("--- Grouter.pick");
  //   P.show("from", Dir.show(from));
  //   P.show("cs", Cells.show(cs));
  //   P.log("ws");
  //   ws |> List.iter(w => P.show("w", Walk.show(w)));
  // };
  Oblig.Delta.minimize(
    ~to_zero=!repair,
    fill(~repair, ~from, cs),
    ws,
  );
};
