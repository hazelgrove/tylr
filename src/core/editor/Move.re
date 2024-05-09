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
  open Options.Syntax;
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
    move(d, z) |> Options.get_or_raise(Invalid_argument("Move.move_n"));
  switch (n) {
  | _ when n < 0 => z |> move(L) |> move_n(n + 1)
  | _ when n > 0 => z |> move(R) |> move_n(n - 1)
  | _zero => z
  };
};

// todo: need to return none in some more cases when no visible movement occurs
let perform = (a: Action.t, z: Zipper.t) =>
  switch (a) {
  | Step(H(d)) => move(d, z)
  | Step(V(d)) => Layout.vstep_focus(d, z)
  | Skip(d2) => Layout.skip_focus(d2, z)
  | Jump(pos) => Layout.jump_focus(pos, z)
  | Hole(_) => failwith("todo: move to hole")
  };
