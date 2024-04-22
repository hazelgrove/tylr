open Util;

module Action = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Step(Dir2.t)
    | Skip(Dir2.t)
    | Jump(Layout.Pos.t)
    | Hole(Dir.t);
};

let move = (d: Dir.t, z: Zipper.t): option(Zipper.t) => {
  open OptUtil.Syntax;
  let b = Dir.toggle(d);
  let+ ctx =
    switch (z.cur) {
    | Point () =>
      let+ (tok, ctx) = Melder.Ctx.pull(~from=d, z.ctx);
      // todo: add movement granularity
      switch (Token.pull(~from=b, tok)) {
      | None => Melder.Ctx.push_fail(~onto=b, tok, ctx)
      | Some((c, tok)) =>
        ctx
        |> Melder.Ctx.push_fail(~onto=d, tok)
        |> Melder.Ctx.push_fail(~onto=b, c)
      };
    | Select((_, zigg)) =>
      z.ctx
      |> Melder.Ctx.push_zigg(~onto=b, zigg)
      |> Melder.Ctx.close
      |> Option.some
    };
  Zipper.mk(ctx);
};

let rec move_n = (n: int, z: Zipper.t): Zipper.t => {
  let move = (d, z) =>
    move(d, z) |> OptUtil.get_or_raise(Invalid_argument("Move.move_n"));
  switch (n) {
  | _ when n < 0 => z |> move(L) |> move_n(n + 1)
  | _ when n > 0 => z |> move(R) |> move_n(n - 1)
  | _zero => z
  };
};

// bounds goal pos to within start/end pos of program.
// returns none if the resulting goal pos is same as start pos.
let map_pos =
    (f: Layout.Pos.t => Layout.Pos.t, z: Zipper.t): option(Zipper.t) => {
  open OptUtil.Syntax;
  let c = Zipper.zip(z);
  let* init_path = Path.Marks.get_focus(c.marks);
  let init_pos = Layout.pos_of_path(init_path, c);
  let goal_pos = Layout.Pos.bound(f(init_pos), ~max=Layout.max_pos(c));
  let+ goal_path =
    Layout.Pos.eq(init_pos, goal_pos)
      ? None : Some(Layout.path_of_pos(goal_pos, c));
  c |> Cell.map_marks(Path.Marks.put_focus(goal_path)) |> Zipper.unzip;
};

// todo: need to return none in some more cases when no visible movement occurs
let perform = (a: Action.t, z: Zipper.t) =>
  switch (a) {
  | Step(H(d)) => move(d, z)
  | Step(V(d)) =>
    z |> map_pos(pos => {...pos, row: pos.row + Dir.pick(d, ((-1), 1))})
  | Skip(H(d)) =>
    z |> map_pos(pos => {...pos, col: Dir.pick(d, ((-1), Int.max_int))})
  | Skip(V(d)) =>
    z
    |> map_pos(_ =>
         Dir.pick(d, (Layout.Pos.zero, Layout.max_pos(Zipper.zip(z))))
       )
  | Jump(pos) => z |> map_pos(_ => pos)
  | Hole(_) => failwith("todo: move to hole")
  };
