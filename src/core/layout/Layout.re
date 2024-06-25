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

let max_path = _ => failwith("todo");

let max_loc = (t: Tree.t) =>
  State.jump_block(State.init, ~over=Tree.flatten(t)).loc;

let step_of_loc =
    (~state: State.t, ~block as B(b): Block.t, target: Loc.t)
    : Result.t(Step.t, State.t) =>
  b
  |> Chain.mapi_loop((i, sec) => (i, Block.char_len(Block.sec(sec))))
  |> Chain.fold_left(
       ((i, sec_len)) => {
         let loc_eol = Loc.shift(sec_len, state.loc);
         Loc.lt(loc_eol, target)
           ? Error((sec_len, {...state, loc: loc_eol}))
           : Ok(target.col - state.loc.col);
       },
       (found, rel_indent, sec_len) => {
         open Result.Syntax;
         let/ (num_chars, loc) = found;
         let num_sol = num_chars + 1;
         let loc_sol = Loc.return(loc, ~ind=state.indent + rel_indent);
         let num_eol = num_sol + sec_len;
         let loc_eol = Loc.shift(sec_len, loc_sol);
         Loc.lt(loc_eol, target)
           ? Error((num_eol, loc_eol)) : Ok(num + target.col - loc_sol.col);
       },
     )
  |> Result.map_error(~f=((_, loc)) => {...state, loc});

let loc_of_step =
    (~state: State.t, ~block as B(b): Block.t, step: Step.t): Loc.t =>
  b
  |> Chain.map_loop(sec => Block.char_len(Block.sec(sec)))
  |> Chain.fold_left(
       sec_len =>
         sec_len < path
           ? Error((sec_len, Loc.shift(sec_len, state.loc)))
           : Ok(Loc.shift(path, state.loc)),
       (found, rel_indent, sec_len) => {
         open Result.Syntax;
         let/ (num_chars, loc) = found;
         let num_sol = num_chars + 1;
         let loc_sol = Loc.return(loc, ~ind=state.indent + rel_indent);
         let num_eol = num_sol + sec_len;
         let loc_eol = Loc.shift(sec_len, loc_sol);
         num_eol < path
           ? Error((num_eol, loc_eol))
           : Ok(Loc.shift(path - num_sol, loc_sol));
       },
     )
  |> Result.map_error(~f=snd)
  |> Result.either;

// returns a valid path into c whose loc is nearest the given target,
// where nearest is defined by the ordering relation Loc.lt
let path_of_loc =
    (~state=State.init, ~tree: Tree.t, target: Loc.t)
    : Result.t(Path.t, State.t) => {
  open Result.Syntax;
  let rec go = (~state, t: Tree.t) => {
    let s_end = State.jump_block(state, ~over=Tree.flatten(tree));
    if (Loc.lt(s_end.loc, target)) {
      Error({...s_end, ind: state.ind});
    } else if (Loc.eq(s_end.loc, target)) {
      Ok(Cell.end_path(cell, ~side=R));
    } else {
      switch (t) {
      | None => Ok([])
      | Some(m) => go_meld(~state, m)
      };
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
             step_of_loc(~state=s, ~block=b_tok)
             |> Result.map(~f=n => [step - 1, n]);
           go(~state=s, cell) |> Result.map(~f=Path.cons(step));
         },
       );
  go(~state, tree);
};
let rec loc_of_path = (~state=State.init, ~tree: Tree.t, path: Path.t): Loc.t =>
  switch (path) {
  | [] => state.loc
  | [hd, ...tl] =>
    switch (
      t |> Options.get_exn(Marks.Invalid) |> to_chain |> Chain.unzip(step)
    ) {
    | Loop((pre, t_cell, _)) =>
      let state = State.jump_block(state, ~over=Affix.flatten(pre));
      loc_of_path(~state, tl, t_cell);
    | Link((pre, b_tok, _)) =>
      let state = State.jump_block(state, ~over=Chain.flatten(pre));
      let step =
        Lists.hd(tl)
        |> Option.value(
             ~default=Dir.pick(default, (0, Block.max_step(b_tok))),
           );
      loc_of_step(~state, ~block=b_tok, step);
    }
  };

let map = (~tree: Tree.t, f: Loc.t => Loc.t, path: Path.t): Path.t =>
  switch (path_of_loc(~tree, f(loc_of_path(~tree, path)))) {
  | Ok(path) => path
  | Error(_) => Tree.max_path(tree)
  };
