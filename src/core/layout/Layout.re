open Stds;

module Block = Block;
module Tree = Tree;

module State = {
  // layout traversal state
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    ind: Loc.Col.t,
    loc: Loc.t,
  };

  let init = {ind: 0, loc: Loc.zero};

  let map = (f, s: t) => {...s, loc: f(s.loc)};

  let indent = (n: int, s: t) => {ind: s.ind + n, loc: Loc.shift(n, s.loc)};
  let return = (s: t, ~ind: Loc.Col.t) => {
    ind,
    loc: Loc.return(s.loc, ~ind),
  };

  let rec jump_block = (s: t, ~over as B(b): Block.t) => {
    let ind = s.ind;
    b
    |> Chain.fold_left(
         sec => jump_sec(s, ~over=sec),
         (s, n, sec) => return(s, ~ind=ind + n) |> jump_sec(~over=sec),
       );
  }
  and jump_sec = (s: t, ~over: Block.Section.t(_)) =>
    switch (over) {
    | Line(l) => map(Loc.shift(Block.Line.len(l)), s)
    | Block(b) => jump_block(s, ~over=b)
    };
};

let max_path = _ => failwith("todo");

let max_loc = (t: Tree.t) =>
  State.jump_block(State.init, ~over=Tree.flatten(t)).loc;

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
         let loc_sol = Loc.return(loc, ~ind=state.ind + rel_indent);
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
         let loc_sol = Loc.return(loc, ~ind=state.ind + rel_indent);
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
    (~state=State.init, ~tree: Tree.t, target: Loc.t)
    : Result.t(Path.t, State.t) => {
  open Result.Syntax;
  let rec go = (~state, t: Tree.t) => {
    let s_end = State.jump_block(state, ~over=Tree.flatten(tree));
    if (Loc.lt(s_end.loc, target)) {
      Error({...s_end, ind: state.ind});
    } else if (Loc.eq(s_end.loc, target)) {
      Ok(Tree.end_path(t, ~side=R));
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
             step_of_loc(~state=s, ~block=b_tok, target)
             |> Result.map(~f=n => [step - 1, n]);
           go(~state=s, t_cell) |> Result.map(~f=Path.cons(step));
         },
       );
  go(~state, tree);
};
// todo: reorg this as unzipping layout zipper
let rec state_of_path =
        (~state=State.init, ~tree: Tree.t, path: Path.t)
        : (State.t, option(Tree.t)) =>
  switch (path) {
  | [] => (state, Some(tree))
  // let s_end = State.jump_block(state, ~over=Tree.flatten(tree));
  // (state.ind, (state.loc, s_end.loc));
  | [hd, ...tl] =>
    switch (
      tree
      |> Options.get_exn(Marks.Invalid)
      |> Tree.to_chain
      |> Chain.unzip(hd)
    ) {
    | Loop((pre, t_cell, _)) =>
      let state =
        State.jump_block(state, ~over=Tree.flatten_affix(~side=L, pre));
      state_of_path(~state, ~tree=t_cell, tl);
    | Link((pre, b_tok, _)) =>
      let state = State.jump_block(state, ~over=Tree.flatten_chain(pre));
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
//     (~side=Dir.L, ~state=State.init, ~tree: Tree.t, path: Path.t) =>
//   Dir.pick(side, (fst, snd), snd(range_of_path(~state, ~tree, path)));

let map = (~tree: Tree.t, f: Loc.t => Loc.t, path: Path.t): Path.t =>
  switch (path_of_loc(~tree, f(fst(state_of_path(~tree, path)).loc))) {
  | Ok(path) => path
  | Error(_) => Tree.end_path(tree, ~side=R)
  };

let states = (~init: State.t, m: Tree.meld) =>
  Tree.to_chain(m)
  |> Chain.fold_left_map(
       t_cell => (State.jump_block(init, ~over=Tree.flatten(t_cell)), init),
       (state, b_tok, t_cell) => {
         let s_mid = State.jump_block(state, ~over=b_tok);
         let s_end = State.jump_block(s_mid, ~over=Tree.flatten(t_cell));
         (s_end, state, s_mid);
       },
     );
