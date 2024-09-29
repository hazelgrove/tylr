open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;
open Stds;

// module Block = Block;
// module Tree = Tree;

module Indent = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    // local indentation imposed by unidelimited containers
    // relative to current bidelimited container
    uni: int,
    // indentation levels of bidelimited container stack.
    // the indentation of each container consists of a global indentation level
    // of the container's parent container, paired with a local indentation level
    // induced by unidelimited container within the parent container.
    bi: list((int, int)),
  };

  let init = {bi: [], uni: 0};

  let peek = (ind: t) =>
    switch (ind.bi) {
    | [] => 0
    | [(glob, loc), ..._] => glob + loc
    };

  let curr = (ind: t) => ind.uni + peek(ind);

  let push = (ind: t) => {uni: 0, bi: [(peek(ind), ind.uni), ...ind.bi]};
  let pop = (ind: t) => {
    uni: Lists.hd(ind.bi) |> Option.map(snd) |> Option.value(~default=0),
    bi: Option.value(Lists.tl(ind.bi), ~default=[]),
  };
};

module State = {
  // layout traversal state
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    ind: Indent.t,
    // global location
    loc: Loc.t,
  };

  let init = {ind: Indent.init, loc: Loc.zero};

  let map = (f, s: t) => {...s, loc: f(s.loc)};

  // let indent = (n: int, s: t) => {
  //   ind: s.ind,
  //   rel: s.rel + n,
  //   loc: Loc.shift(n, s.loc),
  // };
  let push_ind = (s: t) => {...s, ind: Indent.push(s.ind)};
  let pop_ind = (s: t) => {...s, ind: Indent.pop(s.ind)};

  let return = (s: t, rel: int) => {
    loc: Loc.return(s.loc, ~ind=Indent.peek(s.ind) + rel),
    ind: {
      ...s.ind,
      uni: rel,
    },
  };

  let rec jump_block = (s: t, ~over as B(b): Block.t) =>
    b
    |> Chain.fold_left(
         sec => jump_sec(s, ~over=sec),
         (s, n, sec) => return(s, n) |> jump_sec(~over=sec),
       )
  and jump_sec = (s: t, ~over: Block.Section.t(_)) =>
    switch (over) {
    | Line(l) => map(Loc.shift(Block.Line.len(l)), s)
    | Block(b) =>
      let committed = push_ind(s);
      let jumped = jump_block(committed, ~over=b);
      {...s, loc: jumped.loc};
    };

  let jump_cell = (s: t, ~over: LCell.t) => {
    // let rel = s.rel;
    let jumped = jump_block(s, ~over=LCell.flatten(over));
    // LCell.is_space(over) ? jumped : {...jumped, rel};
    jumped;
  };
  let jump_tok = jump_block;

  // assuming terrace wald faces right
  let jump_terr = (~closed=false, s: t, ~over: LTerr.t) => {
    // let (ind, rel) = (s.ind, s.rel);
    let (ts, cs) = LTerr.unmk(over);
    List.combine(cs, ts)
    |> List.rev
    |> List.mapi((i, tc) => (i, tc))
    |> Lists.fold_left(~init=s, ~f=(s, (i, (cell, b_tok))) =>
         s
         |> jump_cell(~over=cell)
         |> (i == 0 ? push_ind : Fun.id)
         |> jump_tok(~over=b_tok)
       )
    |> (closed ? Fun.id : pop_ind);
  };
  // assuming dn slope
  let jump_slope = (s: t, ~over: LSlope.t) =>
    List.fold_right((terr, s) => jump_terr(s, ~over=terr), over, s);
};

let max_path = _ => failwith("todo");

let max_loc = (t: LCell.t) =>
  State.jump_block(State.init, ~over=LCell.flatten(t)).loc;

let rec mk_cell = (~delim=Delim.root, c: Cell.t): LCell.t =>
  Cell.get(c)
  |> Option.map(mk_meld)
  |> Option.map((M(l, _, _) as m: LMeld.t) => {
       let indent = Delim.indent(delim) && LCell.height(l) > 0 ? 2 : 0;
       LCell.nest_body_meld(indent, m);
     })
  |> (meld => Cell.Base.mk(~meld?, ()))
and mk_meld = (m: Meld.t): LMeld.t =>
  Meld.to_chain(m)
  |> Chain.fold_left_map(
       l => ((), mk_cell(l)),
       ((), tok, cell) => {
         let b_tok = Block.of_tok(tok);
         let t_cell = mk_cell(~delim=Node(tok), cell);
         ((), b_tok, t_cell);
       },
     )
  |> snd
  |> Meld.Base.of_chain;

let step_of_loc =
    (~state: State.t, ~block as B(b): Block.t, target: Loc.t)
    : Result.t(Step.t, State.t) =>
  b
  |> Chain.map_loop(sec => Block.len(Block.sec(sec)))
  |> Chain.fold_left(
       sec_len => {
         let loc_eol = Loc.shift(sec_len, state.loc);
         Loc.lt(loc_eol, target)
           ? Error((sec_len, loc_eol)) : Ok(target.col - state.loc.col);
       },
       (found, rel_indent, sec_len) => {
         open Result.Syntax;
         let/ (len, loc) = found;
         let len_sol = len + 1;
         let loc_sol =
           Loc.return(loc, ~ind=Indent.peek(state.ind) + rel_indent);
         let len_eol = len_sol + sec_len;
         let loc_eol = Loc.shift(sec_len, loc_sol);
         Loc.lt(loc_eol, target)
           ? Error((len_eol, loc_eol)) : Ok(len + target.col - loc_sol.col);
       },
     )
  |> Result.map_error(~f=((_, loc)) => {...state, loc});

let loc_of_step =
    (~state: State.t, ~block as B(b): Block.t, step: Step.t): Loc.t =>
  b
  |> Chain.map_loop(sec => Block.len(Block.sec(sec)))
  |> Chain.fold_left(
       sec_len =>
         sec_len < step
           ? Error((sec_len, Loc.shift(sec_len, state.loc)))
           : Ok(Loc.shift(step, state.loc)),
       (found, rel_indent, sec_len) => {
         open Result.Syntax;
         let/ (len, loc) = found;
         let len_sol = len + 1;
         let loc_sol =
           Loc.return(loc, ~ind=Indent.peek(state.ind) + rel_indent);
         let len_eol = len_sol + sec_len;
         let loc_eol = Loc.shift(sec_len, loc_sol);
         len_eol < step
           ? Error((len_eol, loc_eol))
           : Ok(Loc.shift(step - len_sol, loc_sol));
       },
     )
  |> Result.map_error(~f=snd)
  |> Result.either;

// returns a valid path into c whose loc is nearest the given target,
// where nearest is defined by the ordering relation Loc.lt
let path_of_loc =
    (~state=State.init, ~tree: LCell.t, target: Loc.t)
    : Result.t(Path.t, State.t) => {
  open Result.Syntax;
  let rec go = (~state, t: LCell.t) => {
    let s_end = State.jump_block(state, ~over=LCell.flatten(t));
    if (Loc.lt(s_end.loc, target)) {
      Error({...s_end, ind: state.ind});
    } else if (Loc.eq(s_end.loc, target)) {
      Ok(LCell.end_path(t, ~side=R));
    } else {
      switch (t.meld) {
      | None => Ok([])
      | Some(m) => go_meld(~state, m)
      };
    };
  }
  and go_meld = (~state: State.t, m: LMeld.t) =>
    Meld.Base.to_chain(m)
    |> Chain.mapi_loop((step, t_cell) => (step, t_cell))
    |> Chain.fold_left(
         ((step, t_cell)) =>
           go(~state, t_cell) |> Result.map(~f=Path.cons(step)),
         (found, b_tok, (step, t_cell)) => {
           let/ s = found;
           let/ s =
             step_of_loc(~state=s, ~block=b_tok, target)
             |> Result.map(~f=n => [step - 1, n]);
           go(~state=s, t_cell) |> Result.map(~f=Path.cons(step));
         },
       );
  go(~state, tree);
};

// todo: reorg this as unzipping layout zipper
let rec state_of_path =
        (~state=State.init, ~tree: LCell.t, path: Path.t)
        : (State.t, option(LCell.t)) =>
  switch (path) {
  | [] => (state, Some(tree))
  // need to handle space separately because indentation is updated differently
  | [hd, ...tl] when LCell.is_space(tree) =>
    switch (
      tree.meld
      |> Options.get_exn(Marks.Invalid)
      |> Meld.Base.to_chain
      |> Chain.unzip(hd)
    ) {
    | Loop(((b_toks, _), t_cell, _)) =>
      let state =
        State.jump_block(state, ~over=Block.hcats(List.rev(b_toks)));
      (state, Some(t_cell));
    | Link(((_, b_toks), b_tok, _)) =>
      switch (tl) {
      | [] =>
        let b = Block.hcats(List.rev(b_toks));
        let s = State.jump_block(state, ~over=b);
        (s, None);
      | [hd, ..._] =>
        let ind = state.ind;
        let b = Block.hcats(List.rev(b_toks));
        let s = State.jump_block(state, ~over=b);
        let loc = loc_of_step(~state={...s, ind}, ~block=b_tok, hd);
        ({...state, loc}, None);
      }
    }
  | [hd, ...tl] =>
    switch (
      tree.meld
      |> Options.get_exn(Marks.Invalid)
      |> Meld.Base.to_chain
      |> Chain.unzip(hd)
    ) {
    | Loop((pre, t_cell, suf)) =>
      // let (ind, rel) = (state.ind, state.rel);
      let state =
        List.combine(snd(pre), fst(pre))
        |> List.rev
        |> List.mapi((i, ct) => (i, ct))
        |> Lists.fold_left(~init=state, ~f=(state, (i, (t_cell, b_tok))) =>
             state
             |> State.jump_cell(~over=t_cell)
             |> (i == 0 ? State.push_ind : Fun.id)
             |> State.jump_tok(~over=b_tok)
           );
      let state = Chain.Affix.is_empty(suf) ? State.pop_ind(state) : state;
      state_of_path(~state, ~tree=t_cell, tl);
    | Link((pre, b_tok, _)) =>
      let state =
        pre
        |> Chain.fold_right(
             (t_cell, b_tok, state) =>
               state
               |> State.jump_tok(~over=b_tok)
               |> State.jump_cell(~over=t_cell),
             t_cell => State.jump_cell(state, ~over=t_cell) |> State.push_ind,
           );
      switch (tl) {
      | [] => (state, None)
      // let s_end = State.jump_block(state, ~over=b_tok);
      // (state.ind, (state.loc, s_end.loc));
      | [hd, ..._] =>
        let loc = loc_of_step(~state, ~block=b_tok, hd);
        // (state.ind, (loc, loc));
        ({...state, loc}, None);
      };
    }
  };
// let loc_of_path =
//     (~side=Dir.L, ~state=State.init, ~tree: LCell.t, path: Path.t) =>
//   Dir.pick(side, (fst, snd), snd(range_of_path(~state, ~tree, path)));

let map = (~tree: LCell.t, f: Loc.t => Loc.t, path: Path.t): Path.t =>
  switch (path_of_loc(~tree, f(fst(state_of_path(~tree, path)).loc))) {
  | Ok(path) => path
  | Error(_) => LCell.end_path(tree, ~side=R)
  };

let states = (~init: State.t, m: LMeld.t) =>
  Meld.Base.to_chain(m)
  |> Chain.fold_left_map(
       t_cell => (State.jump_cell(init, ~over=t_cell), init),
       (state, b_tok, t_cell) => {
         let s_mid = State.jump_tok(state, ~over=b_tok);
         let s_end = State.jump_cell(s_mid, ~over=t_cell);
         (s_end, state, s_mid);
       },
     );

// let row_ends = (~tree: LCell.t, row: Loc.Row.t): (Loc.Col.t, Loc.Col.t) => {
//   let (l, _) =
//     Loc.{row, col: 0}
//     |> path_of_loc(~tree)
//     |> Stds.Result.get_fail("unexpected")
//     |> state_of_path(~tree);
//   let (r, _) =
//     Loc.{row, col: Int.max_int}
//     |> path_of_loc(~tree)
//     |> Stds.Result.value(~default=Fun.const(LCell.end_path(tree, ~side=R)))
//     |> state_of_path(~tree);
//   (l.loc.col, r.loc.col);
// };
let nth_line = (tree: LCell.t, r: Loc.Row.t) =>
  Block.nth_line(LCell.flatten(tree), r);

let rec unzip = (~ctx=LCtx.empty, cur: Path.Cursor.t, c: LCell.t): LZipper.t => {
  let hd = Path.Cursor.hd(cur);
  switch (hd) {
  | Error(Point(_)) =>
    let cur = Cursor.Point(c);
    LZipper.mk(cur, ctx);
  | Error(Select(range)) =>
    let m = Options.get_exn(Marks.Invalid, c.meld);
    unzip_select(range, m, ~ctx);
  | Ok(step) =>
    let tl = Option.get(Path.Cursor.peel(step, cur));
    let m = Options.get_exn(Marks.Invalid, c.meld);
    switch (Meld.Base.unzip(step, m)) {
    | Loop((pre, cell, suf)) =>
      let ctx = LCtx.add((pre, suf), ctx);
      unzip(~ctx, tl, cell);
    // | Link((pre, tok, suf)) =>
    //   // if caret points to token, then take the entire meld as focused cell
    //   let cur = Cursor.map(Caret.map(Fun.const()), Fun.id, cur);
    //   (cur, c, frame);
    | Link((pre, tok, suf)) =>
      switch (cur) {
      | Point(_) => LZipper.mk(Cursor.Point(c), ctx)
      | Select(_) =>
        let (l, pre) = Chain.uncons(pre);
        let (r, suf) = Chain.uncons(suf);
        let ctx =
          ctx
          |> Chain.map_hd(
               LFrame.Open.cat((
                 LSlope.Dn.unroll(l) @ Option.to_list(LTerr.mk'(pre)),
                 LSlope.Up.unroll(r) @ Option.to_list(LTerr.mk'(suf)),
               )),
             );
        let cur = Cursor.Select(LZigg.of_tok(tok));
        LZipper.mk(cur, ctx);
      }
    };
  };
}
and unzip_select = (~ctx, sel: Path.Selection.t, meld: LMeld.t) => {
  let (l, r) = sel.range;
  let l_hd = Path.hd(l) |> Path.Head.get(() => 0);
  let r_hd = Path.hd(r) |> Path.Head.get(() => Meld.length(meld) - 1);
  let (pre, top, suf) = LMeld.split_subwald(l_hd, r_hd, meld);
  let (pre_eqs, (pre_dn, pre_up)) = {
    // l points to cell hd_pre
    let (hd_pre, tl_pre) = Chain.uncons(pre);
    let l_tl = l_hd mod 2 == 0 ? List.tl(l) : [];
    let z = unzip(Point(Caret.focus(l_tl)), hd_pre);
    let (eqs, flat) = LCtx.flatten(z.ctx);
    let eqs =
      Chain.Affix.is_empty(tl_pre)
        ? eqs : [(0, (-1)), ...LEqs.incr(~side=L, 1, eqs)];
    let flat =
      switch (LTerr.mk'(tl_pre)) {
      | None => flat
      | Some(t) => LFrame.Open.snoc(~side=L, t, flat)
      };
    (eqs, flat);
  };
  let (suf_eqs, (suf_dn, suf_up)) = {
    // r points to cell hd_suf
    let (hd_suf, tl_suf) = Chain.uncons(suf);
    let r_tl = r_hd mod 2 == 0 ? List.tl(r) : [];
    let z = unzip(Point(Caret.focus(r_tl)), hd_suf);
    let (eqs, flat) = LCtx.flatten(z.ctx);
    let eqs =
      Chain.Affix.is_empty(tl_suf)
        ? eqs : [((-1), 0), ...LEqs.incr(~side=R, 1, eqs)];
    let flat =
      switch (LTerr.mk'(tl_suf)) {
      | None => flat
      | Some(t) => LFrame.Open.snoc(~side=R, t, flat)
      };
    (eqs, flat);
  };
  let zigg = LZigg.mk(~up=pre_up, top, ~dn=suf_dn);
  let eqs = {
    let (dn, up) = Chain.hd(ctx);
    let pre_eqs = LEqs.incr(~side=L, List.length(dn), pre_eqs);
    let suf_eqs = LEqs.incr(~side=R, List.length(up), suf_eqs);
    (pre_eqs, suf_eqs);
  };
  let ctx = Chain.map_hd(LFrame.Open.cat((pre_dn, suf_up)), ctx);
  LZipper.mk(~eqs, Select(zigg), ctx);
};

let state_of_ctx = (ctx: LCtx.t) =>
  ctx
  |> Chain.fold_right(
       ((pre: LSlope.t, _), (l: LTerr.t, _), state) =>
         state
         |> State.jump_terr(~over=l, ~closed=true)
         |> State.jump_slope(~over=pre),
       ((pre, _)) => State.jump_slope(State.init, ~over=pre),
     );
