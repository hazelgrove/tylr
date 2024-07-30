open Stds;

module Action = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Step(Dir2.t)
    | Skip(Dir2.t)
    | Jump(Loc.t)
    | Hole(Dir.t);
};

// enters unmarked token or moves cursor in marked token.
// returns Some(_) if caret remains strictly within token,
// otherwise returns None if caret reaches token edge.
let hstep_tok = (d: Dir.t, tok: Token.t): option(Token.t) => {
  let (m, n) = (Token.length(tok), Utf8.length(tok.text));
  let (l, r) = (1, Token.is_complete(tok) ? m - 1 : n);
  switch (tok.marks) {
  | _ when m <= 1 => None
  | None =>
    let car = Caret.focus(Dir.pick(d, (r, l)));
    Some(Token.put_cursor(Point(car), tok));
  | Some(Point(car)) when Dir.pick(d, (car.path <= l, car.path >= r)) =>
    None
  | Some(Point(car)) =>
    let car = Step.Caret.shift(Dir.pick(d, ((-1), 1)), car);
    Some(Token.put_cursor(Point(car), tok));
  | Some(Select(sel)) =>
    let (l, r) = Step.Selection.carets(sel);
    let car = Caret.focus(Dir.pick(d, (l, r)).path);
    Some(Token.put_cursor(Point(car), tok));
  };
};

let hstep = (d: Dir.t, z: Zipper.t): option(Zipper.t) => {
  open Options.Syntax;
  let b = Dir.toggle(d);
  let+ ctx =
    switch (z.cur) {
    // move to d end of selection
    | Select({range: zigg, _}) =>
      return(Ctx.push_zigg(~onto=b, zigg, z.ctx))
    | Point(_) =>
      let (face, ctx) = Ctx.pull_face(~from=d, z.ctx);
      let+ tok = Bound.to_opt(face);
      switch (hstep_tok(d, tok)) {
      | Some(stepped) => Ctx.push(~onto=b, stepped, ctx)
      | None => Ctx.push(~onto=b, Token.clear_marks(tok), ctx)
      };
    };
  Zipper.(button(mk(ctx)));
};

let rec hstep_n = (n: int, z: Zipper.t): Zipper.t => {
  let move = (d, z) =>
    hstep(d, z) |> Options.get_exn(Invalid_argument("Move.move_n"));
  switch (n) {
  | _ when n < 0 => z |> move(L) |> hstep_n(n + 1)
  | _ when n > 0 => z |> move(R) |> hstep_n(n - 1)
  | _zero => z
  };
};

// bounds goal pos to within start/end pos of program.
// returns none if the resulting goal pos is same as start pos.
let map_focus = (f: Loc.t => Loc.t, z: Zipper.t): option(Zipper.t) => {
  open Options.Syntax;
  let c = Zipper.zip(~save_cursor=true, z);
  let* init = Option.bind(c.marks.cursor, Path.Cursor.get_focus);
  let goal = Layout.map(~tree=Tree.of_cell(c), f, init);
  goal == init
    ? None : c |> Cell.map_marks(Cell.Marks.put_focus(goal)) |> Zipper.unzip;
};

let vstep = (d: Dir.t) =>
  map_focus(loc => {...loc, row: loc.row + Dir.pick(d, ((-1), 1))});
let skip = (d2: Dir2.t) =>
  map_focus(loc =>
    switch (d2) {
    | H(L) => {...loc, col: 0}
    | H(R) => {...loc, col: Int.max_int}
    | V(L) => Loc.zero
    | V(R) => Loc.maximum
    }
  );
let jump = loc => map_focus(Fun.const(loc));

// todo: need to return none in some more cases when no visible movement occurs
let perform = (a: Action.t) =>
  switch (a) {
  | Step(H(d)) => hstep(d)
  | Step(V(d)) => vstep(d)
  | Skip(d2) => skip(d2)
  | Jump(loc) => jump(loc)
  | Hole(_) => failwith("todo: move to hole")
  };
