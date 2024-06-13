open Stds;

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
    // move to d end of selection
    | Select({range: zigg, _}) =>
      return(Ctx.push_zigg(~onto=b, zigg, z.ctx))
    | Point(_) =>
      let+ (tok, ctx) = Ctx.pull(~from=d, z.ctx);
      // todo: add movement granularity
      switch (Token.pull(~from=b, tok)) {
      | None => Ctx.push(~onto=b, tok, ctx)
      | Some((c, tok)) =>
        ctx |> Ctx.push(~onto=d, tok) |> Ctx.push(~onto=b, c)
      };
    };
  Zipper.(button(mk(ctx)));
};

let rec move_n = (n: int, z: Zipper.t): Zipper.t => {
  let move = (d, z) =>
    move(d, z) |> Options.get_exn(Invalid_argument("Move.move_n"));
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
