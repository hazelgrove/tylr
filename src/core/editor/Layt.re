// module type LINE = {
//   type t;
//   let nil: t;
//   let cat: (t, t) => t;
// };

module Line = {
  type t = list(Token.t);
  let nil = [];
  let cat = (@);
};
module Section = {
  type t('block) =
    | Line(Line.t)
    | Block('block);

  let hcat = (~hcat_block, l, r) =>
    switch (l, r) {
    | (Line(l), Line(r)) => Line(Line.cat(l, r))
    | (Line(l), Block(r)) => Block(cat_hd(l, r))
    | (Block(l), Line(r)) => Block(cat_ft(l, r))
    | (Block(l), Block(r)) => Block(hcat_block(l, r))
    };
};
module Block = {
  type t =
    | B(Chain.t(Section.t(t), int));

  let get = (f, B(b)) => f(b);
  let put = (f, x) => B(f(x));

  let sec = put(Chain.unit);
  let line = l => sec(Line(l));
  let wrap = (b: t) => sec(Block(b));

  let nil = line(Line.nil);
  let cons = (sec: section, ~indent=0) => Chain.link(sec, indent);

  let map_hd = get(Chain.map_hd);
  let map_ft = get(Chain.map_ft);

  let rec cat_hd = (line: Line.t, block: t) =>
    block
    |> map_hd(
         fun
         | Line(l) => Line.cat(line, l)
         | Block(b) => cat_hd(line, b),
       );
  let rec cat_ft = (block: t, line: Line.t) =>
    block
    |> map_ft(
         fun
         | Line(l) => Line.cat(l, line)
         | Block(b) => cat_ft(line, b),
       );

  let rec hcat = (l: t, r: t) => Chain.cat(hcat_block = hcat, l, r);
  let hcats = (bs: list(t)) => List.fold_right(hcat, bs, empty);
  let vcat = (l: t, ~indent=0, r: t) => Chain.append(l, indent, r);

  // blocks carry indentation info for all but the first section
  let nest_tl = (n: int) => Chain.map_link((+)(n));
  let unnest_ft = (n: int, b: Block.t) =>
    b
    |> Chain.fold_right_map(
         (sec, ind, should_unnest) =>
           (sec, ind - (should_unnest ? n : 0), false),
         sec => (sec, is_space(sec)),
       );
  let nest_body = (n: int, b: Block.t) =>
    b |> nest_tl(n) |> unnest_ft(n) |> fst;

  let of_tok = (tok: Token.t) =>
    switch (tok.mtrl) {
    | Grout(_)
    | Tile(_) => line([tok])
    | Space =>
      Strings.split('\n', tok.text)
      |> List.map(text => Block.line([{...tok, text}]))
      |> hcats
    };
};

// module Chain = {
//   include Chain;
//   type t = Chain.t(Block.t, Block.t);
//   let flatten = (bs: Chain.t(t, t)) =>
//     bs
//     |> Chain.fold_right(
//          (b_lp, b_lk, acc) => hcats([b_lp, b_lk, acc]),
//          Fun.id,
//        );
//   let nest_body = (n: int, bs: t): t =>
//     bs
//     |> Chain.map(Block.nest_tl(n), Block.nest_tl(n))
//     |> Chain.fold_right_map(
//          (b_cell, b_tok, should_unnest) => {
//             let _ = failwith("todo: should_unnest -> done");
//            let (b_tok, should_unnest) =
//              (should_unnest ? Block.unnest_ft(n) : Fun.id)(b_tok);
//            let (b_cell, should_unnest) =
//              (should_unnest ? Block.unnest_ft(n) : Fun.id)(b_cell);
//            (b_cell, b_tok, should_unnest);
//          },
//          b_cell => Block.unnest_ft(n, b_cell),
//        )
//     |> fst;
// };

// module Affix = {
//   type t = Chain.Affix.t(Block.t, Block.t);
//   let flatten = (~side: Dir.t, aff: t) =>
//     aff
//     |> List.concat_map(((lk, lp)) => [lk, lp])
//     |> Dir.pick(side, (List.rev, Fun.id))
//     |> Block.hcats;
// };

module Tree = {
  // todo: unify with existing structure by polymorphizing
  type t = option(meld)
  and meld =
    | M(t, wald, t)
  and wald =
    | W(Chain.t(Block.t, t));

  let map: (_, t) => t = Option.map;

  let to_chain = (M(l, W(w), r): meld) => Chain.consnoc(l, w, r);
  let of_chain = c => {
    let (l, w, r)  = Option.get_exn(Invalid_argument(""), Chain.unconsnoc(c));
    M(l, W(w), r);
  };

  let rec flatten = (t: t): Block.t =>
    t
    |> map(flatten_meld)
    |> Option.value(~default=Block.nil)
  and flatten_meld = (M(l, w, r): meld) =>
    Block.hcats([
      flatten(l),
      flatten_wald(w),
      flatten(r),
    ])
  and flatten_wald = (W(w): wald) =>
    w
    |> Chain.map_link(t => Block.wrap(flatten(t)))
    |> Chain.flatten;

  let rec nest_tl = (n: int) => map(n == 0 ? Fun.id : nest_tl_meld(n))
  and nest_tl_meld = (n, M(l, W(w), r): meld) =>
    M(nest_tl(n, l), W(Chain.map_loop(Block.nest_tl(n), w)), nest_tl(n, r));

  let rec unnest_ft = (n: int, t: t) =>
    n == 0
    ? (t, true)
    : t
      |> Option.map(unnest_ft_meld(n))
      |> Option.value(~default=(t, false))
  and unnest_ft_meld = (n, M(l, W(w), r): meld) => {
    let (r, done) = unnest_ft(n, r);
    let (w, done) = (done ? Fun.id : unnest_ft_wald(n))(w);
    let (l, done) = (done ? Fun.id : unnest_ft(n))(l);
    (M(l, W(w), r), done);
  }
  and unnest_ft_wald = (n, W(w): wald) =>
    w
    |> Chain.fold_right_map(
      (b_tok, t, done) => {
        let (t, done) = (done ? Fun.id : unnest_ft(n))(r);
        let (b_tok, done) = (done ? Fun.id : Block.unnest_ft(n))(b_tok);
        (b_tok, t, done);
      },
      Block.unnest_ft,
    );

  let nest_body = (n: int, t: t) =>
    t
    |> nest_tl(n)
    |> unnest_ft(n)
    |> fst;
  let nest_body_meld = (n: int, m: meld) =>
    m
    |> nest_tl_meld(n)
    |> unnest_ft_meld(n)
    |> fst;

  let rec height = (t: t) => t |> map(height_meld) |> Option.value(~default=0)
  and height_meld  = (M(l, _, r)) => height(l) + height(r);

  let rec of_cell = (~delim=Delim.root, c: Cell.t): t =>
    Cell.get(c)
    |> Option.map(of_meld(~delim))
    |> Option.map((M(l, _, _) as m: meld) => {
      let indent = Delim.indent(delim) && height(l) > 0 ? 2 : 0;
      nest_body_meld(indent, m);
    })
  and of_meld = (M(l, w, r): Meld.t): meld =>
    Meld.to_chain(m)
    |> Chain.mapi_loop((i, cell) => (i, cell))
    |> Chain.fold_left_map(
        ((_, l)) => {
          let t = of_cell(l);
          ((), t);
        },
        ((), tok, (i, cell)) => {
          let b_tok = Block.of_tok(tok);
          let t_cell = of_cell(~delim=Node(tok), cell);
          ((), b_tok, t_cell);
        },
      )
    |> snd;

  // let rec hcat = (T(t): t) =>
  //   t
  //   |> Chain.map(
  //     t => Option.map(hcat, t) |> Option.value(~default=Block.nil),
  //     Fun.id,
  //   )
  //   |> Chain.fold_right(
  //     (b_cell, b_tok, acc) => hcats([b_cell, b_tok, acc]),
  //     Fun.id,
  //   );

  // let rec nest_tl = (n: int, T(t): t) =>
  //   switch (Chain.unconsnoc(t)) {
  //   | Error(t) => Chain.unit(Option.map(nest_tl(n), t))
  //   | Ok(())
  //   }

  // let nest_body = (n: int, T(t): t) =>
  //   t
  //   |> Chain.map()
};

// let rec of_cell = (~delim=Delim.root, c: Cell.t): t =>
//   switch (Cell.get(c)) {
//   | None => nil
//   | Some(m) => Chain.hcat(of_meld(~delim, m))
//   }
// and of_meld = (~delim: Delim.t, m: Meld.t): Chain.t =>
//   Meld.to_chain(m)
//   |> Chain.mapi_loop((i, cell) => (i, cell))
//   |> Chain.fold_left_map(
//        ((_, l)) => {
//          let b = of_cell(l);
//          (Block.height(b), b);
//        },
//        (h, tok, (i, cell)) => {
//          let b_tok = of_tok(tok);
//          let b_cell =
//            of_cell(~delim=Node(tok), cell)
//            |> (i == Meld.length(m) - 1 ? Fun.id : Block.wrap);
//          (h, b_tok, b_cell);
//        },
//      )
//   |> (
//     ((h, bs)) => Chain.nest_body(Delim.indent(delim) && h > 0 ? 2 : 0, bs)
//   );

module Chain = {
  include Chain;
  type t = Chain.t(Tree.t, Block.t);
  let flatten = (c: Chain.t(t, t)) =>
    c
    |> Chain.fold_right(
         (t, b, acc) => Block.hcats([Tree.flatten(t), b, acc]),
         Tree.flatten,
       );
};

module Affix = {
  type t = Chain.Affix.t(Block.t, Tree.t);
  let flatten = (~side: Dir.t, aff: t) =>
    aff
    |> List.concat_map(((b, t)) => [b, Tree.flatten(t)])
    |> Dir.pick(side, (List.rev, Fun.id))
    |> Block.hcats;
};

module State = {
  // layout traversal state
   type t = {
    ind: Col.t,
    loc: Loc.t,
  };

  let init = {ind: 0, loc: Loc.zero};

  let map = (f, s: t) => {...s, loc: f(loc)};

  let indent = (n: int, s: t) => {ind: s.ind + n, loc: Loc.shift(n, s.loc)};
  let return = (s: t, ~ind: Col.t) => {ind, loc: Loc.return(s.loc, ~ind)};

  let rec jump_block = (s: t, ~over: Block.t) => {
    let ind = s.ind;
    over
    |> Chain.fold(
         sec => jump_sec(s, ~over=sec),
         (s, n, sec) => return(s, ~ind=ind + n) |> jump_sec(~over=sec),
       );
  }
  and jump_sec = (s: t, ~over: Section.t(Block.t)) =>
    switch (over) {
    | Line(l) => map(Loc.shift(Line.len(l)), s)
    | Block(b) => jump_block(~over=b)
    };
};

let rec cursor_tok = (~state, cur: Step.Cursor.t, B(b): Block.t): Loc.Cursor.t =>
  switch (cur) {
  | Select(sel) =>
    let (l, r) = Step.Selection.carets(sel);
    let l = Loc.Cursor.get_point(cursor_tok(~state, Point(l), B(b)));
    let r = Loc.Cursor.get_point(cursor_tok(~state, Point(r), B(b)));
    Select(Loc.Selection.of_carets(l, r));
  | Point({hand, path}) =>
    b
    |> Chain.map_loop(sec => Block.char_len(Block.sec(sec)))
    |> Chain.fold_left(
      sec_len =>
        sec_len < path
        ? Error((sec_len, Loc.shift(sec_len, state.loc)))
        : Ok(Point({hand, path: Loc.shift(path, state.loc)})),
      (found, rel_indent, sec_len) => {
        open Result.Syntax;
        let/ (num, loc) = found;
        let (num_sol, loc_sol) =
          (num + 1, Loc.return(loc, ~ind=state.indent + rel_indent));
        let (num_eol, loc_eol) =
          (num_sol + sec_len, Loc.shift(sec_len, loc_sol));
        num_eol < path
        ? Error((num_eol, loc_eol))
        : Ok(Point({hand, path: Loc.shift(path - num_sol, loc_sol)}))
      },
    )
    |> (
      fun
      | Ok(cur) => cur
      | Error((_, loc)) => Point({hand, path: loc})
    )
  };

let mk =
    (~state=State.init, ~delim=Delim.root, cell: Cell.t)
    : (option(Loc.Cursor.t), Tree.t) => {
  open Options.Syntax;

  let t = of_cell(~delim, cell);

  let rec cursor = (~state, cur: Path.Cursor.t, t: Tree.t): Loc.Cursor.t => {
    // let* cur = cell.marks.cursor;
    switch (Path.Cursor.hd(cur)) {
    | Error(Point(car)) =>
      Loc.Cursor.point(Caret.mk(car.hand, state.pos))
    | Error(Select(sel)) =>
      let (l, r) = Path.Selection.carets(sel);
      let l = cursor(~state, Point(l), t);
      let r = cursor(~state, Point(r), t);
      Select(
        Loc.Selection.mk(
          ~focus=sel.focus,
          Loc.Cursor.(get_point(l).path, get_point(r).path),
        ),
      );
    | Ok(step) =>
      let cur = Option.get(Path.Cursor.peel(step, cur));
      switch (
        t
        |> Options.get_exn(Marks.Invalid)
        |> to_chain
        |> Chain.unzip(step)
      ) {
      | Loop((pre, t_cell, _)) =>
        let state = State.jump_block(state, ~over=Affix.flatten(pre));
        cursor(~state, cur, t_cell);
      | Link((pre, b_tok, _)) =>
        let state = State.jump_block(state, ~over=Chain.flatten(pre));
        cursor_tok(~state, Path.Cursor.to_step(cur), b_tok);
      }
    };
  }
};

let rec cursor =
        (~state=State.init, ~delim=Delim.root, cell: Cell.t)
        : option(Loc.Cursor.t) => {
  open Options.Syntax;
  let* cur = cell.marks.cursor;
  switch (Path.Cursor.hd(cur)) {
  | Error(Point(car)) =>
    Some(Loc.Cursor.point(Caret.mk(car.hand, state.pos)))
  | Error(Select(sel)) =>
    let (l, r) = Path.Selection.carets(sel);
    let l = Option.get(cursor(~state, Cell.put_cursor(Point(l), cell)));
    let r = Option.get(cursor(~state, Cell.put_cursor(Point(r), cell)));
    Some(
      Select(
        Loc.Selection.mk(
          ~focus=sel.focus,
          Loc.Cursor.(get_point(l).path, get_point(r).path),
        ),
      ),
    );
  | Ok(step) =>
    switch (
      Cell.get(cell)
      |> Options.get_exn(Marks.Invalid)
      |> of_meld(~delim)
      |> Chain.unzip(step)
    ) {
    | Loop((pre, cell, _)) =>
      let state = State.jump_block(state, ~over=Affix.hcat(pre));
      cursor(~state, cell);
    | Link((pre, tok, _)) =>
      let state = State.jump_block(state, ~over=Chain.hcat(pre));
      cursor_tok(~state, tok);
    }
  };
};

// returns a valid path into c whose loc is nearest the given target,
// where nearest is defined by the ordering relation Loc.lt
let path_of_loc = (target: Loc.t, cell: Cell.t) => {
  open Result.Syntax;
  let rec go_cell =
          (~state=State.init, ~delim=Delim.root, cell: Cell.t)
          : Result.t(Path.t, State.t) => {
    let s_end = State.jump_block(state, ~over=of_cell(~delim, cell));
    if (Loc.lt(s_end.loc, target)) {
      Error({...s_end, ind: state.ind});
    } else if (Loc.eq(s_end.loc, target)) {
      Ok(Cell.end_path(cell, ~side=R));
    } else {
      switch (Cell.get(cell)) {
      | None => Ok([])
      | Some(m) => go_meld(~state, ~delim, m)
      };
    };
  }
  and go_meld = (~state: State.t, ~delim: Delim.t, m: Meld.t) =>
    of_meld(~delim, m)
    |> Chain.mapi_loop((step, b_cell) => (step, b_cell))
    |> Chain.fold_left(
         ((step, cell)) =>
           go_cell(~state, cell) |> Result.map(~f=Path.cons(step)),
         (found, tok, (step, cell)) => {
           let/ s = found;
           let/ s =
             go_tok(~state=s, tok, ~return=state.ind)
             |> Result.map(~f=Path.cons(step - 1));
           go_cell(~state=s, cell) |> Result.map(~f=Path.cons(step));
         },
       )
  and go_tok = (~state: State.t, tok: Token.t) => {
    let s_end = State.jump_block(state, ~over=of_tok(tok));
    ();
  };
  go_cell(cell);
};

let max_loc = (c: Cell.t) =>
  State.jump_block(State.init, ~over=of_cell(c)).loc;

// bounds goal pos to within start/end pos of program.
// returns none if the resulting goal pos is same as start pos.
let map_focus = (f: Loc.t => Loc.t, z: Zipper.t): option(Zipper.t) => {
  open Options.Syntax;
  let c = Zipper.zip(~save_cursor=true, z);
  let* cursor = cursor(c);
  // let* init_path = Cell.Marks.get_focus(c.marks);
  let* init_loc = Loc.Cursor.get_focus(cursor);
  let goal_loc = Loc.bound(f(init_pos), ~max=max_loc(c));
  let+ goal_path =
    Pos.eq(init_loc, goal_loc) ? None : Some(path_of_loc(goal_loc, c));
  c
  |> Cell.map_marks(Cell.Marks.put_focus(goal_path))
  |> Zipper.unzip
  |> Options.get_fail("bug: failed to unzip after putting path");
};
