open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;
open Stds;

module Ictx = {
  type t = {
    // left delimiter
    delim: Bound.t(Token.t),
    // indentation at start of cell (before any newlines)
    left: Col.t,
    // indentation to return to at end of cell (after all newlines)
    right: Col.t,
  };
  let init = {delim: Root, left: 0, right: 0};
  let middle = (~newline=true, ctx: t) => {
    let indent =
      ctx.delim |> Bound.map(Token.indent) |> Bound.get(~root=false);
    ctx.left + (indent && newline ? 2 : 0);
  };
  // let left = (~newline: bool, ctx: t): t => {
  //   let middle = middle(~newline, ctx);
  //   {...ctx, right: middle};
  // };
  // let mid = (~newline: bool, ~delim, ctx: t) => {
  //   let middle = middle(~newline, ctx);
  //   {delim, left: middle, right: middle};
  // };
  // let right = (~newline: bool, ~delim, ctx: t) => {
  //   let middle = middle(~newline, ctx);
  //   {...ctx, delim, left: middle};
  // };
};

module State = {
  type t = {
    ctx: Ictx.t,
    pos: Pos.t,
  };

  let init = {ctx: Ictx.init, pos: Pos.zero};

  // todo: unify this with load_cell_frame below
  let load_terr = (terr: Terr.R.t, ~closed=false, s: t): t => {
    let mid =
      Ictx.middle(~newline=Dims.of_cell(terr.cell).height > 0, s.ctx);
    let ctx =
      Ictx.{
        delim: Bound.Node(Terr.hd(terr)),
        left: mid,
        right: closed ? mid : s.ctx.right,
      };
    let pos =
      List.fold_right2(
        (tok, cell, pos) => {
          assert(!Token.Space.is(tok));
          pos
          |> Pos.skip(~over=Dims.of_cell(cell), ~return=mid)
          // note: this only works for non-space tokens
          |> Pos.skip_col(Token.length(tok));
        },
        Terr.tokens(terr),
        Terr.cells(terr),
        s.pos,
      );
    {ctx, pos};
  };

  let load_cell_frame = (~newline, (pre, suf), s: t): t =>
    switch (Terr.mk'(pre)) {
    | Some(pre) => load_terr(pre, ~closed=Option.is_some(Terr.mk'(suf)), s)
    | None =>
      let ctx = {...s.ctx, right: Ictx.middle(~newline, s.ctx)};
      {...s, ctx};
    };
  let load_tok_frame = (~newline, (pre, _suf), s: t) => {
    let mid = Ictx.middle(~newline, s.ctx);
    let pos =
      pre
      |> Chain.fold_right(
           (cell, tok, pos) => {
             assert(!Token.Space.is(tok));
             pos
             // note: this only works for non-space tokens
             |> Pos.skip_col(Token.length(tok))
             |> Pos.skip(~over=Dims.of_cell(cell), ~return=mid);
           },
           cell => Pos.skip(~over=Dims.of_cell(cell), ~return=mid, s.pos),
         );
    {...s, pos};
  };

  let load_closed = ((l, _): Frame.Closed.t) => load_terr(l, ~closed=true);

  // hack to deal with Token.length being incorrect for split tokens
  // let err =
  //   switch (open_) {
  //   | ([hd, ..._], _) when Frame.Open.zips(open_) =>
  //     let hd = Wald.hd(hd);
  //     Token.length(hd) - Utf8.length(hd);
  //   | _ => 0
  //   };
  let load_open = ((dn, _): Frame.Open.t) =>
    List.fold_right((terr, s) => load_terr(terr, s), dn);

  let load = (~init=init): (Ctx.t => t) =>
    Ctx.fold_root(
      (open_, closed, s) => s |> load_closed(closed) |> load_open(open_),
      open_ => load_open(open_, init),
    );

  let range = (s: t, dims: Dims.t) => (
    s.pos,
    Pos.skip(s.pos, ~over=dims, ~return=s.ctx.right),
  );
};

module State = {
  type t = {
    delim: Delim.t,
    ind: Col.t,
    loc: Loc.t,
  };

  let init = {
    delim: Delim.root,
    ind: 0,
    loc: Loc.zero,
  };

  let map = (f, s: t) => {...s, loc: f(loc)};

  let indent = (n: int, s: t) => {ind: s.ind + n, loc: Loc.shift(n, s.loc)};
  let return = (s: t, ~ind: Col.t) => {ind, loc: Loc.return(s.loc, ~ind)};

  let rec jump_block = (s: t, ~over: Dims2.Block.t) =>
    over
    |> Dims2.Block.fold(
         chunk => jump_chunk(s, ~over=chunk),
         (s, (), chunk) => return(s, ~ind=s.ind) |> jump_chunk(~over=chunk),
       )
  and jump_chunk = (s: t, ~over: Dims2.Chunk.t(_)) =>
    switch (over) {
    | Line(l) => map(Loc.shift(Line.len(l)), s)
    | Block({indent: n, block}) => indent(n, s) |> jump_block(~over=block)
    };

  let load_cell_frame = ((pre, suf), state: t) =>

};

let count_newlines =
  fun
  | None => 0
  | Some(tok: Token.t) => Strings.count('\n', tok.text);

let rec cursor_tok = (~state: State.t, tok: Token.t): option(Loc.Cursor.t) => {
  open Options.Syntax;
  let+ cur = tok.marks
  and+ (l, _, r) = Token.unzip(tok);
  switch (cur) {
  | Select(sel) =>
    let (l, r) = Step.Selection.carets(sel);
    let l =
      Option.get(cursor_tok(~state, Token.put_cursor(Point(l), tok)));
    let r =
      Option.get(cursor_tok(~state, Token.put_cursor(Point(r), tok)));
    Loc.Cursor.select(
      Loc.Selection.mk(
        ~focus=sel.focus,
        Loc.Cursor.(get_point(l).path, get_point(r).path),
      ),
    );
  | Point(car) =>
    let (n_l, n_r) = (count_newlines(l), count_newlines(r));
    let dims_l = Dims.of_tok(Option.value(l, ~default=Token.Space.empty));
    let return =
      n_r > 0 ? Ictx.middle(~newline=n_l > 0, state.ctx) : state.ctx.right;
    let p = Pos.skip(state.pos, ~over=dims_l, ~return);
    Point(Loc.Caret.mk(car.hand, p));
  };
};

let rec cursor = (~state=State.init, cell: Cell.t): option(Loc.Cursor.t) => {
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
    let M(l, _, _) as m = Options.get_exn(Marks.Invalid, Cell.get(cell));
    // todo: figure out better organization of this recurring newline calc
    let newline = Dims.of_cell(l).height > 0;
    switch (Meld.unzip(step, m)) {
    | Loop((pre, cell, suf)) =>
      let state = State.load_cell_frame(~newline, (pre, suf), state);
      cursor(~state, cell);
    | Link((pre, tok, suf)) =>
      let state = State.load_tok_frame(~newline, (pre, suf), state);
      cursor_tok(~state, tok);
    };
  };
};

// let cursor_range = (~state=State.init, cell: Cell.t): Range.t =>
//   switch (cursor(~state, cell)) {
//   | None => State.range(state, Dims.of_cell(cell))
//   | Some(Point({path: pos, _})) => (pos, pos)
//   | Some(Select({range, _})) => range
//   };

let fold =
    (
      init: 'acc,
      f_tok: (Ictx.t, Pos.t, Token.t) => 'acc,
      f_cell: Chain.t('acc, 'acc) => 'acc,
      c: Cell.t,
    ) => {
  let rec go = (~ctx=Ictx.init, ~pos=Pos.zero, c: Cell.t) =>
    switch (Cell.get(c)) {
    | None => init
    | Some(M(l, _, _) as m) =>
      let mid = Ictx.middle(~newline=Dims.of_cell(l).height > 0, ctx);
      let n = Meld.length(m);
      Meld.to_chain(m)
      |> Chain.mapi_loop((i, c) => (i, c))
      |> Chain.fold_left_map(
           ((_, c)) => {
             let ctx = {...ctx, right: mid};
             let acc = go(~ctx={...ctx, right: mid}, ~pos, c);
             (Pos.skip(pos, ~over=Dims.of_cell(l), ~return=mid), acc);
           },
           (pos, t, (i, c)) => {
             let t_acc = f_tok(ctx, pos, t);
             let pos = Pos.skip(pos, ~over=Dims.of_tok(t), ~return=mid);
             let return = i < n - 1 ? mid : ctx.right;
             let ctx = Ictx.{delim: Node(t), left: mid, right: return};
             let c_acc = go(~ctx, ~pos, c);
             (Pos.skip(pos, ~over=Dims.of_cell(c), ~return), t_acc, c_acc);
           },
         )
      |> snd
      |> f_cell;
    };
  go(c);
};

let pos_of_path = (path: Path.t, cell: Cell.t): Pos.t => {
  let rec go = (~ctx: Ictx.t, ~pos: Pos.t, path: Path.t, cell) => {
    let dims = Dims.of_cell(cell);
    let (l, r) = (pos, Pos.skip(pos, ~over=dims, ~return=ctx.right));
    switch (Cell.Space.get(cell)) {
    | Some(spc) =>
      // todo: consider joining with tok case below
      switch (path) {
      | []
      | [0, ..._] => l
      | [2, ..._] => r
      | [1, j, ..._] =>
        let h = Strings.count('\n', spc.text);
        let mid = Ictx.middle(~newline=h > 0, ctx);
        let (l, r) = Utf8.split(j, spc.text);
        let h_r = Strings.count('\n', r);
        Pos.skip(
          pos,
          ~over=Dims.of_space(l),
          ~return=h_r > 0 ? mid : ctx.right,
        );
      | _ => raise(Marks.Invalid)
      }
    | None =>
      let M(c_l, _, _) as m =
        Cell.get(cell) |> Options.get_exn(Marks.Invalid);
      let mid = Ictx.middle(~newline=Dims.of_cell(c_l).height > 0, ctx);
      switch (path) {
      | [] => l
      // | ([], End(R)) => r
      | [tok, ...tl] when tok mod 2 != 0 =>
        let (pre, _, _) = Meld.unzip_tok(tok, m);
        pre
        |> Chain.fold_left(
             cell => Pos.skip(pos, ~over=Dims.of_cell(cell), ~return=mid),
             (pos, tok, cell) =>
               pos
               |> Pos.skip(~over=Dims.of_tok(tok), ~return=mid)
               |> Pos.skip(~over=Dims.of_cell(cell), ~return=mid),
           )
        |> Pos.skip_col(Base.List.hd(tl) |> Option.value(~default=0));
      | [c, ...cs] =>
        let ((toks, cells), cell, suf) = Meld.unzip_cell(c, m);
        let pos =
          List.fold_right2(
            (tok, cell, pos) =>
              pos
              |> Pos.skip(~over=Dims.of_cell(cell), ~return=mid)
              |> Pos.skip_col(Token.length(tok)),
            toks,
            cells,
            pos,
          );
        let ctx = {
          let (delim, left) =
            switch (toks) {
            | [] => (ctx.delim, ctx.left)
            | [t, ..._] => (Bound.Node(t), mid)
            };
          let right =
            switch (suf) {
            | ([], _) => ctx.right
            | ([_, ..._], _) => mid
            };
          Ictx.{delim, left, right};
        };
        go(~pos, ~ctx, cs, cell);
      };
    };
  };
  go(~ctx=Ictx.init, ~pos=Pos.zero, path, cell);
};

// returns a valid path into c whose pos is nearest the given target,
// where nearest is defined by the ordering relation Pos.lt
let path_of_pos = (target: Pos.t, c: Cell.t): Path.t => {
  open Result.Syntax;
  let rec go_cell =
          (~ctx: Ictx.t, ~pos: Pos.t, cell: Cell.t): Result.t(Path.t, Pos.t) => {
    let c_end = Pos.skip(pos, ~over=Dims.of_cell(cell), ~return=ctx.right);
    if (Pos.lt(c_end, target)) {
      Error(c_end);
    } else if (Pos.eq(c_end, target)) {
      Ok(Cell.end_path(~side=R, cell));
    } else {
      switch (Cell.get(cell)) {
      | None => Ok([])
      | Some(m) => go_meld(~ctx, ~pos, m)
      };
    };
  }
  and go_meld =
      (~ctx: Ictx.t, ~pos: Pos.t, m: Meld.t): Result.t(Path.t, Pos.t) => {
    let M(l, _, _) = m;
    // indentation of meld's root tokens
    let mid = Ictx.middle(~newline=Dims.of_cell(l).height > 0, ctx);
    Meld.to_chain(m)
    |> Chain.mapi_loop((step, cell) => (step, cell))
    |> Chain.fold_left(
         ((step, cell)) => {
           go_cell(~ctx={...ctx, right: mid}, ~pos, cell)
           |> Result.map(~f=Path.cons(step))
         },
         (found, tok, (step, cell)) => {
           let/ pos = found;
           let/ pos =
             go_tok(~ctx, ~pos, tok) |> Result.map(~f=Path.cons(step - 1));
           let ctx =
             Ictx.{
               delim: Node(tok),
               left: mid,
               right: step == Meld.length(m) - 1 ? ctx.right : mid,
             };
           go_cell(~ctx, ~pos, cell) |> Result.map(~f=Path.cons(step));
         },
       );
  }
  // ctx is ctx of containing meld
  and go_tok =
      (~ctx: Ictx.t, ~pos: Pos.t, tok: Token.t): Result.t(Path.t, Pos.t) => {
    let _ = failwith("using ctx.right below doesn't always seem right...");
    let t_end = Pos.skip(pos, ~over=Dims.of_tok(tok), ~return=ctx.right);
    if (Pos.lt(t_end, target)) {
      Error(t_end);
    } else {
      let lines = String.split_on_char('\n', tok.text);
      let n = List.length(lines);
      lines
      |> List.mapi((i, line) => (i, line))
      |> List.fold_left(
           (found, (i, line)) => {
             let/ (chars, pos: Pos.t) = found;
             // count newline
             let chars = i == 0 ? chars : chars + 1;
             let row = i == 0 ? pos.row : pos.row + 1;
             // start of row
             let col =
               i == 0 ? ctx.left : i == n - 1 ? ctx.right : Ictx.middle(ctx);
             let len = Utf8.length(line);
             if (row < target.row) {
               Error((chars + len, Pos.{row, col: col + len}));
             } else if (col + len < target.col) {
               i == n - 1
                 ? Error((chars + len, Pos.{row, col: col + len}))
                 : Ok([chars + len]);
             } else {
               Ok([chars + target.col - col]);
             };
           },
           Error((0, pos)),
         )
      |> Result.map_error(~f=snd);
    };
  };

  if (Pos.leq(target, Pos.zero)) {
    Cell.end_path(~side=L, c);
  } else {
    switch (go_cell(~ctx=Ictx.init, ~pos=Pos.zero, c)) {
    | Ok(path) => path
    | Error(_) => Cell.end_path(~side=R, c)
    };
  };
};

let max_pos = (c: Cell.t) => {
  let dims = Dims.of_cell(c);
  Pos.{row: dims.height, col: Dims.Width.total(dims.width)};
};

// bounds goal pos to within start/end pos of program.
// returns none if the resulting goal pos is same as start pos.
let map_focus = (f: Pos.t => Pos.t, z: Zipper.t): option(Zipper.t) => {
  open Options.Syntax;
  let c = Zipper.zip(~save_cursor=true, z);
  let* cursor = cursor(c);
  // let* init_path = Cell.Marks.get_focus(c.marks);
  let* init_pos = Loc.Cursor.get_focus(cursor);
  let goal_pos = Pos.bound(f(init_pos), ~max=max_pos(c));
  let+ goal_path =
    Pos.eq(init_pos, goal_pos) ? None : Some(path_of_pos(goal_pos, c));
  c
  |> Cell.map_marks(Cell.Marks.put_focus(goal_path))
  |> Zipper.unzip
  |> Options.get_fail("bug: failed to unzip after putting path");
};

let vstep_focus = (d: Dir.t) =>
  map_focus(pos => {...pos, row: pos.row + Dir.pick(d, ((-1), 1))});

let skip_focus = (d2: Dir2.t, z: Zipper.t) =>
  switch (d2) {
  | H(L) => map_focus(pos => Pos.{...pos, col: 0}, z)
  | H(R) => map_focus(pos => Pos.{...pos, col: Int.max_int}, z)
  | V(L) => map_focus(_ => Pos.zero, z)
  | V(R) => map_focus(_ => max_pos(Zipper.zip(z)), z)
  };

let jump_focus = pos => map_focus(_ => pos);
