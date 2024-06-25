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

// returns a valid path into c whose loc is nearest the given target,
// where nearest is defined by the ordering relation Loc.lt
let path_of_loc = (target: Loc.t, t: Tree.t) => {
  open Result.Syntax;
  let rec go =
          (~state=State.init, t: Tree.t)
          : Result.t(Path.t, State.t) => {
    let s_end = State.jump_block(state, ~over=Tree.flatten(t));
    if (Loc.lt(s_end.loc, target)) {
      Error({...s_end, ind: state.ind});
    } else if (Loc.eq(s_end.loc, target)) {
      Ok(Cell.end_path(cell, ~side=R));
    } else {
      switch (t) {
      | None => Ok([])
      | Some(m) => go_meld(~state, m)
      }
    };
  }
  and go_meld = (~state: State.t, m: Tree.meld) =>
    Tree.to_chain(m)
    |> Chain.mapi_loop((step, t_cell) => (step, t_cell))
    |> Chain.fold_left(
        ((step, t_cell)) =>
          go(~state, t_cell) |> Result.map(~f=Path.cons(step)),
        (found, b_tok, (step, t_cell)) => {
          let/ s = found;
          let/ s =
            go_tok(~state=s, b_tok)
            |> Result.map(~f=Path.cons(step - 1));
          go(~state=s, cell) |> Result.map(~f=Path.cons(step));
        },
      )
  and go_tok = (~state: State.t, B(b_tok): Block.t) => {
    let s_end = State.jump_block(state, ~over=b_tok);
    if (Loc.lt(s_end.loc, target)) {
      Error(s_end)
    } else {
      b_tok
      |> Chain.mapi_loop((i, sec) => (i, Block.char_len(Block.sec(sec))))
      |> Chain.fold_left(
        ((i, sec_len)) => {
          let loc_eol = Loc.shift(sec_len, state.loc);
          Loc.lt(loc_eol, target)
          ? Error((sec_len, {...state, loc: loc_eol}))
          : Ok([target.col - state.loc.col])
        },
        (found, rel_indent, sec_len) => {
          let/ (num, loc) = found;
          let num_sol = num + 1;
          let loc_sol = Loc.return(loc, ~ind=state.indent + rel_indent);
          let num_eol = num_sol + sec_len;
          let loc_eol = Loc.shift(sec_len, loc_sol);
          Loc.lt(loc_eol, target)
          ? Error((num_eol, loc_eol))
          : Ok([num + target.col - loc_sol.col])
        }
      )
    };
  };
  go_cell(cell);
};

let rec cursor_tok = (~state, cur: Step.Cursor.t, B(b): t): Loc.Cursor.t =>
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
           let (num_sol, loc_sol) = (
             num + 1,
             Loc.return(loc, ~ind=state.indent + rel_indent),
           );
           let (num_eol, loc_eol) = (
             num_sol + sec_len,
             Loc.shift(sec_len, loc_sol),
           );
           num_eol < path
             ? Error((num_eol, loc_eol))
             : Ok(Point({hand, path: Loc.shift(path - num_sol, loc_sol)}));
         },
       )
    |> (
      fun
      | Ok(cur) => cur
      | Error((_, loc)) => Point({hand, path: loc})
    )
  };

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
};

let mk =
    (~state=State.init, ~delim=Delim.root, cell: Cell.t)
    : (option(Loc.Cursor.t), Tree.t) => {
  open Options.Syntax;
  let t = Tree.of_cell(~delim, cell);
  let cur = cell.marks.cursor |> Option.map(cur => cursor(~state, cur, t));
  (cur, t);
};

let max_loc = (c: Cell.t) =>
  State.jump_block(State.init, ~over=of_cell(c)).loc;

// bounds goal pos to within start/end pos of program.
// returns none if the resulting goal pos is same as start pos.
let map_focus = (f: Loc.t => Loc.t, z: Zipper.t): option(Zipper.t) => {
  open Options.Syntax;
  let c = Zipper.zip(~save_cursor=true, z);
  let t = Tree.of_cell(c);
  let* cur = cell.marks.cursor |> Option.map(cur => cursor(cur, t))
  // let* cursor = cursor(c);
  // let* init_path = Cell.Marks.get_focus(c.marks);
  let* init_loc = Loc.Cursor.get_focus(cur);
  let goal_loc = Loc.bound(f(init_pos), ~max=max_loc(c));
  let+ goal_path =
    Pos.eq(init_loc, goal_loc) ? None : Some(Tree.path_of_loc(goal_loc, t));
  c
  |> Cell.map_marks(Cell.Marks.put_focus(goal_path))
  |> Zipper.unzip
  |> Options.get_fail("bug: failed to unzip after putting path");
};
