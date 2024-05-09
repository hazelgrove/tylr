open Util;

module Action = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Un(Dir.t)
    | All
    | Wald
    | Meld
    | Move(Move.Action.t);
};

let unselect = (~toward=?, ~save_anchor=false, z: Zipper.t) =>
  switch (z.cur) {
  | Point () => z
  | Select((d, zigg)) =>
    let onto = Dir.toggle(Option.value(toward, ~default=d));
    let fill =
      save_anchor ? Fill.unit(Cell.point(~foc=false, ())) : Fill.empty;
    Zipper.mk(Melder.Ctx.push_zigg(~onto, zigg, ~fill, z.ctx));
  };

let select = (d: Dir.t, z: Zipper.t): option(Zipper.t) => {
  open Options.Syntax;
  let b = Dir.toggle(d);
  switch (z.cur) {
  | Point () =>
    let+ (tok, ctx) = Melder.Ctx.pull(~from=d, z.ctx);
    let cur = Cursor.Select((d, Zigg.of_tok(tok)));
    Zipper.mk(~cur, ctx);
  | Select((side, zigg)) =>
    if (side == d) {
      let+ (tok, ctx) = Melder.Ctx.pull(~from=d, z.ctx);
      let zigg = Melder.Zigg.grow(~side, tok, zigg);
      Zipper.mk(~cur=Select((d, zigg)), ctx);
    } else {
      let (tok, rest) = Melder.Zigg.pull(~side=d, zigg);
      let ctx = Melder.Ctx.(close(push_fail(~onto=b, tok, z.ctx)));
      let cur =
        switch (rest) {
        | None => Cursor.Point()
        | Some(sel) => Select((b, sel))
        };
      Some(Zipper.mk(~cur, ctx));
    }
  };
};

let perform = (a: Action.t, z: Zipper.t): option(Zipper.t) =>
  switch (a) {
  | Un(d) => Some(unselect(~toward=d, z))
  | All
  | Wald
  | Meld => failwith("todo Select.perform")
  | Move(a) =>
    switch (a) {
    | Step(H(d)) => select(d, z)
    | Step(V(d)) => Layout.vstep_focus(d, z)
    | Skip(d2) => Layout.skip_focus(d2, z)
    | Jump(pos) => Layout.jump_focus(pos, z)
    | Hole(_) => failwith("unimplemented")
    }
  };
