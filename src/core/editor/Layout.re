open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;
open Util;

module Int_ppx = {
  include Int;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = int;
  module Map = IntMap;
};
module Row = Int_ppx;
module Col = Int_ppx;

module Pos = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    row: Row.t,
    col: Col.t,
  };
  let zero = {row: 0, col: 0};

  let compare = (l, r) => {
    let c = Row.compare(l.row, r.row);
    c == 0 ? Col.compare(l.col, r.col) : c;
  };
  let eq = (l, r) => compare(l, r) == 0;
  let lt = (l, r) => compare(l, r) < 0;
  let leq = (l, r) => compare(l, r) <= 0;

  let min = (l, r) => lt(l, r) ? l : r;
  let max = (l, r) => lt(r, l) ? l : r;

  let bound = (~min as l=zero, ~max as r: t, pos: t) => min(max(l, pos), r);

  let skip_col = (n, pos) => {...pos, col: pos.col + n};

  let skip = (pos: t, ~over: Dims.t, ~return: Col.t) => {
    {
      // let h = Dims.Height.total(over.height);
      // let w = Dims.Width.total(over.width);
      row: pos.row + over.height,
      col:
        (over.height > 0 ? return : pos.col) + Dims.Width.total(over.width),
    };
  };
};

module Ictx = {
  type t = {
    // left delimiter
    delim: Bound.t(Token.t),
    // indentation at start of cell (before any newlines)
    left: Col.t,
    // indentation to return to at end of cell
    right: Col.t,
  };
  let init = {delim: Root, left: 0, right: 0};
  let middle = (~newline=true, ctx: t) => {
    let indent =
      ctx.delim |> Bound.map(Token.indent) |> Bound.get(~root=false);
    ctx.left + (indent && newline ? 2 : 0);
  };
};

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
      let n = Meld.total_length(m);
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
        let h = StringUtil.count('\n', spc.text);
        let mid = Ictx.middle(~newline=h > 0, ctx);
        let (l, r) = Utf8.split(j, spc.text);
        let h_r = StringUtil.count('\n', r);
        Pos.skip(
          pos,
          ~over=Dims.of_space(l),
          ~return=h_r > 0 ? mid : ctx.right,
        );
      | _ => raise(Path.Invalid)
      }
    | None =>
      let M(c_l, _, _) as m =
        Cell.get(cell) |> OptUtil.get_or_raise(Path.Invalid);
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
        |> Pos.skip_col(ListUtil.hd_opt(tl) |> Option.value(~default=0));
      | [c, ...cs] =>
        let ((toks, cells), cell, suf) = Meld.unzip(c, m);
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
    let _ = failwith("todo: double check Chain.mapi_loop index");
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
               right: step == Meld.total_length(m) - 1 ? ctx.right : mid,
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
  open OptUtil.Syntax;
  let c = Zipper.zip(z);
  let* init_path = Path.Marks.get_focus(c.marks);
  let init_pos = pos_of_path(init_path, c);
  let goal_pos = Pos.bound(f(init_pos), ~max=max_pos(c));
  let+ goal_path =
    Pos.eq(init_pos, goal_pos) ? None : Some(path_of_pos(goal_pos, c));
  c |> Cell.map_marks(Path.Marks.put_focus(goal_path)) |> Zipper.unzip;
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
