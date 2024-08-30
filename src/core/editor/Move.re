open Stds;

// todo: add options for skipping walds/melds and use in selection
[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  // single step
  | Step(Dir2.t)
  // skip to end
  | Skip(Dir2.t)
  // jump to absolute loc
  | Jump(Loc.t)
  // jump to next hole
  | Hole(Dir.t);

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

// returns token with updated cursor after moving in direction d.
// carets at the token edges are pruned. returns a flag indicating
// whether the moved caret reached the token edge.
let hstep_tok = (d: Dir.t, tok: Token.t): (Token.t, bool) => {
  let (m, n) = (Token.length(tok), Utf8.length(tok.text));
  // 1-step-shy-of-tok-end caret positions
  let (l, r) = (1, Token.is_complete(tok) ? m - 1 : n);
  switch (tok.marks) {
  // exit token
  | _ when m <= 1 || n <= 0 => (Token.clear_marks(tok), true)
  | Some(Point(car)) when Dir.pick(d, (car.path <= l, car.path >= r)) => (
      Token.clear_marks(tok),
      true,
    )
  // enter token
  | None =>
    let car = Caret.focus(Dir.pick(d, (r, l)));
    (Token.put_cursor(Point(car), tok), false);
  // move within token
  | Some(Point(car)) =>
    let car = Step.Caret.shift(Dir.pick(d, ((-1), 1)), car);
    (Token.put_cursor(Point(car), tok), false);
  // move to end of selection
  | Some(Select(sel)) =>
    let (l, r) = Step.Selection.carets(sel);
    let car = Caret.focus(Dir.pick(d, (l, r)).path);
    (Token.put_cursor(Point(car), tok), false);
  };
};

let hstep = (d: Dir.t, z: Zipper.t): option(Zipper.t) => {
  open Options.Syntax;
  let b = Dir.toggle(d);
  let+ ctx =
    switch (z.cur) {
    | Select({range: zigg, _}) =>
      // move to d end of selection
      return(Ctx.push_zigg(~onto=b, zigg, z.ctx))
    | Point(_) =>
      let (face, ctx) = Ctx.pull(~from=d, z.ctx);
      let+ tok = Bound.to_opt(face);
      let (stepped, exited) = hstep_tok(d, tok);
      ctx
      |> Ctx.push(~onto=b, stepped)
      |> (exited ? Fun.id : Ctx.push(~onto=d, stepped));
    };
  Zipper.mk(Ctx.button(ctx));
};
let rec hstep_n = (n: int, z: Zipper.t): Zipper.t => {
  let step = (d, z) =>
    hstep(d, z) |> Options.get_exn(Invalid_argument("Move.hstep_n"));
  switch (n) {
  | _ when n < 0 => z |> step(L) |> hstep_n(n + 1)
  | _ when n > 0 => z |> step(R) |> hstep_n(n - 1)
  | _zero => z
  };
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

let hole = (d: Dir.t, z: Zipper.t): option(Zipper.t) => {
  open Options.Syntax;
  let c = Zipper.zip(~save_cursor=true, z);
  let normal = Zipper.normalize(~cell=c);
  switch (Options.get_exn(Zipper.Bug__lost_cursor, c.marks.cursor)) {
  | Select(_) => hstep(d, z)
  | Point({path, _}) =>
    let+ (path, _) =
      c.marks.obligs
      |> Path.Map.filter((_, mtrl: Mtrl.T.t) => mtrl != Space(Unmolded))
      |> Dir.pick(
           d,
           (
             Path.Map.find_last_opt(p => Path.lt(normal(p), path)),
             Path.Map.find_first_opt(p => Path.gt(normal(p), path)),
           ),
         );
    c |> Cell.put_cursor(Point(Caret.focus(path))) |> Zipper.unzip_exn;
  };
};

// todo: need to return none in some more cases when no visible movement occurs
let perform =
  fun
  | Step(H(d)) => hstep(d)
  | Step(V(d)) => vstep(d)
  | Skip(d2) => skip(d2)
  | Jump(loc) => jump(loc)
  | Hole(d) => hole(d);
