open Virtual_dom.Vdom;

module T = Token;
open Tylr_core;
open Util;

module Shape = {
  type t =
    | Straight
    // Dir arg is side of token the caret is on
    | Bent(Dir.t, Tip.t);
  let rec mk = (~side=?, ctx: Ctx.t) => {
    let (zipped, ctx) = Zipper.zip_init(Zipper.mk(ctx));
    switch (Ctx.zip_step(~zipped, ctx)) {
    | _ when !Cell.is_empty(zipped) => Straight
    | None
    | Some((Eq (), _, _)) => Straight
    | Some((Neq(side), zipped, ctx)) when Cell.Space.is_space(zipped) =>
      mk(~side, ctx)
    | Some((Neq(d), _, _)) =>
      side == Some(d) ? Bent(Dir.toggle(d), Conc) : Bent(d, Conv)
    };
  };
  // what direction the bent caret points
  let dir =
    fun
    | Straight => None
    | Bent(L, Conv)
    | Bent(R, Conc) => Some(Dir.L)
    | Bent(L, Conc)
    | Bent(R, Conv) => Some(R);
};
module Profile = {
  type t = {
    loc: Loc.t,
    hand: Caret.Hand.t,
    shape: Shape.t,
  };
  let mk = (~loc: Loc.t, hand: Caret.Hand.t, ctx: Ctx.t) => {
    loc,
    hand,
    shape: Shape.mk(ctx),
  };
};

let adj =
  fun
  | Shape.Straight => 0.
  | Bent(side, Conv) => Dir.pick(side, ((-1.), 1.)) *. T.convex_adj
  | Bent(side, Conc) => Dir.pick(side, ((-1.), 1.)) *. T.concave_adj;

let path = (shape: Shape.t) => {
  open Svgs.Path;
  // let width = 0.1;
  let run =
    switch (Shape.dir(shape)) {
    | None => 0.
    | Some(L) => -. T.tip_width
    | Some(R) => T.tip_width
    };
  List.concat([
    [M({x: run, y: 0.5})],
    // adjust scale of caret to account for rounded linecap/join
    [L_({dx: -. run, dy: (-0.5)}), L_({dx: run, dy: 0.5})] |> scale(0.925),
    [L_({dx: -. run, dy: 0.5}), L_({dx: run, dy: (-0.5)})] |> scale(0.925),
  ])
  |> transpose({dx: adj(shape), dy: 0.});
};

let mk = (~font, p: Profile.t) =>
  Svgs.Path.view(path(p.shape))
  |> Nodes.add_classes(["caret"])
  |> Stds.Lists.single
  |> Box.mk(~font, ~loc=p.loc)
  |> Stds.Lists.single
  |> Node.div(~attrs=[Attr.class_("caret-container")]);
