open Virtual_dom.Vdom;
open Tylr_core;
open Util.Svgs;

module Profile = {
  type t = {
    pos: Layout.Pos.t,
    len: int,
    sort: Sort.t,
    tips: Tip.s,
  };

  let mk = (~pos: Layout.Pos.t, ~null as (l, r): (bool, bool), tok: Token.t) => {
    let len = Token.length(tok);
    let (sort, tips) =
      switch (tok.mtrl) {
      | Space () => raise(Invalid_argument("Dec.Token.Profile.mk"))
      | Grout(g) => g
      | Tile(t) => (Tile.T.sort(t), Tip.(l ? Conv : Conc, r ? Conv : Conc))
      };
    {pos, len, sort, tips};
  };
};

let tip_width = 0.32;
let concave_adj = 0.25;
let convex_adj = (-0.13);

let shadow_dx = "0.1";
let shadow_dy = "0.037";
let shadow_adj = 0.015;

let child_border_thickness = 0.05;

let t = child_border_thickness /. 0.5;
// let short_tip_height = (1. -. t) *. 0.5;
let short_tip_width = (1. -. t) *. tip_width;

let run: Tip.t => float =
  fun
  | Conv => +. short_tip_width
  | Conc => -. short_tip_width;

let adj: Tip.t => float =
  fun
  | Conv => convex_adj
  | Conc => concave_adj;

let tip = (t: Tip.t): Path.t => [
  H_({dx: +. adj(t)}),
  L_({dx: +. run(t), dy: 0.5}),
  L_({dx: -. run(t), dy: 0.5}),
  H_({dx: -. adj(t)}),
];

let path = ((l, r): Tip.s, length: int): Path.t =>
  List.flatten([
    Path.[m(~x=0, ~y=0), h(~x=length)],
    tip(r),
    Path.[h(~x=0)],
    Path.scale(-1., tip(l)),
  ]);

let path_view = (prof: Profile.t) =>
  Util.Svgs.Path.view(
    ~attrs=[
      Attr.classes([
        "tile-path",
        "raised",
        "indicated",
        Sort.to_str(prof.sort),
      ]),
    ],
    path(prof.tips, prof.len),
  );

let mk = (prof: Profile.t) => Box.mk(~pos=prof.pos, [path_view(prof)]);

let drop_shadow = (sort: Sort.t) =>
  Util.Nodes.filter(
    ~attrs=[Attr.id("raised-drop-shadow-" ++ Sort.to_str(sort))],
    [
      Node.create_svg(
        "feDropShadow",
        ~attrs=[
          Attr.classes(["tile-drop-shadow"]),
          Attr.create("dx", shadow_dx),
          Attr.create("dy", shadow_dy),
          Attr.create("stdDeviation", "0"),
        ],
        [],
      ),
    ],
  );
