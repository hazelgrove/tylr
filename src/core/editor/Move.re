open Stds;

// todo: add options for skipping walds/melds and use in selection
[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  // single step
  | Step(Dir2.t)
  // skip to end
  | Skip(Dir2.t)
  // jump to absolute loc
  | Jump(Loc.t);

let unselect = (~toward=?, z: Zipper.t) =>
  switch (z.cur) {
  | Point(_) => z
  | Select({focus: d, range: zigg}) =>
    let onto = Dir.toggle(Option.value(toward, ~default=d));
    Zipper.mk(Ctx.push_zigg(~onto, zigg, z.ctx));
  };

// bounds goal pos to within start/end pos of program.
// returns none if the resulting goal pos is same as start pos.
let map_focus =
    (~round_tok=?, ~save_anchor=false, f: Loc.t => Loc.t, z: Zipper.t)
    : option(Zipper.t) => {
  open Options.Syntax;
  let c = Zipper.zip(~save_cursor=true, z);
  let* init = Option.bind(c.marks.cursor, Path.Cursor.get_focus);
  let goal =
    init
    |> Layout.map(~round_tok?, ~tree=Layout.mk_cell(c), f)
    |> Zipper.normalize(~cell=c);
  goal == init
    ? None
    : c
      |> Cell.map_marks(Cell.Marks.put_focus(~save_anchor, goal))
      |> Zipper.unzip;
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

let push_site = (~onto: Dir.t, site: Zipper.Site.t, ctx: Ctx.t) =>
  switch (site) {
  | Between => ctx
  | Within(tok) => Ctx.push(~onto, tok, ctx)
  };
let push_sites = (site: Zipper.Site.cursor, ctx: Ctx.t) =>
  switch (site) {
  | Point(Between) => ctx
  | Point(Within(tok)) =>
    ctx |> Ctx.push(~onto=L, tok) |> Ctx.push(~onto=R, tok)
  | Select((l, r)) => ctx |> push_site(~onto=L, l) |> push_site(~onto=R, r)
  };

let hstep = (d: Dir.t, z: Zipper.t): option(Zipper.t) => {
  open Options.Syntax;
  let b = Dir.toggle(d);
  // P.log("--- Move.hstep");
  let (cur_site, ctx_sans_sites) = Zipper.cursor_site(z);
  let+ z =
    switch (cur_site) {
    | Select(_) => return(unselect(~toward=d, z))
    | Point(site) =>
      // P.log("--- Move.hstep/Point");
      let (face, ctx) =
        switch (site) {
        | Between => Ctx.pull(~from=d, ctx_sans_sites)
        | Within(tok) => (Delim.tok(tok), ctx_sans_sites)
        };
      // P.show("face", Delim.show(face));
      // P.show("ctx", Ctx.show(ctx));
      let+ tok = Bound.to_opt(face);
      let (stepped, exited) = hstep_tok(d, tok);
      // P.show("stepped", Token.show(stepped));
      // P.show("exited", string_of_bool(exited));
      ctx
      |> Ctx.push(~onto=b, stepped)
      |> (exited ? Fun.id : Ctx.push(~onto=d, stepped))
      |> Zipper.mk;
    };
  Zipper.rebutton(z);
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

let vstep = (~round_tok=?, ~save_anchor=false, d: Dir.t, z: Zipper.t) =>
  switch (z.cur) {
  | Select(_) => Some(unselect(~toward=d, z))
  | Point(_) =>
    z
    |> map_focus(~round_tok?, ~save_anchor, loc =>
         {...loc, row: loc.row + Dir.pick(d, ((-1), 1))}
       )
  };

let skip = (~round_tok=?, ~save_anchor=false, d2: Dir2.t) =>
  map_focus(~round_tok?, ~save_anchor, loc =>
    switch (d2) {
    | H(L) => {...loc, col: 0}
    | H(R) => {...loc, col: Int.max_int}
    | V(L) => Loc.zero
    | V(R) => Loc.maximum
    }
  );

let jump = (~round_tok=?, ~save_anchor=false, loc) =>
  map_focus(~round_tok?, ~save_anchor, Fun.const(loc));

// todo: need to return none in some more cases when no visible movement occurs
let perform =
  fun
  | Step(H(d)) => hstep(d)
  | Step(V(d)) => vstep(d)
  | Skip(d2) => skip(d2)
  | Jump(loc) => jump(loc);
