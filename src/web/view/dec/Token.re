open Virtual_dom.Vdom;
open Tylr_core;
open Util.Svgs;

module Style = {
  type t = {
    sort: Sort.t,
    shape: Tip.s,
  };
  let mk = (~null as (l, r), mtrl: Mtrl.T.t): option(t) =>
    switch (mtrl) {
    | Space(_) => None
    | Grout((sort, shape)) => Some({sort, shape})
    | Tile(t) =>
      let sort = Tile.T.sort(t);
      let shape = Tip.(l ? Conv : Conc, r ? Conv : Conc);
      Some({sort, shape});
    };
};

module Profile = {
  type t = {
    loc: Loc.t,
    len: int,
    style: option(Style.t),
  };

  let mk = (~loc: Loc.t, ~null: (bool, bool), tok: Token.t) => {
    loc,
    len: Token.length(tok),
    style: Style.mk(~null, tok.mtrl),
  };
};
let tip_width = 0.32;
let concave_adj = 0.25;
let convex_adj = (-0.13);

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

let mk = (prof: Profile.t) =>
  prof.style
  |> Option.map((Style.{sort, shape}) =>
       Util.Svgs.Path.view(path(shape, prof.len))
       |> Util.Nodes.add_classes([
            "tile-path",
            "raised",
            "indicated",
            Sort.to_str(sort),
          ])
     )
  |> Option.to_list
  |> Box.mk(~loc=prof.loc);

let drop_shadow = (sort: Sort.t) =>
  Util.Nodes.filter(
    ~attrs=[Attr.id("raised-drop-shadow-" ++ Sort.to_str(sort))],
    [
      Node.create_svg(
        "feDropShadow",
        ~attrs=[
          Attr.classes(["tile-drop-shadow"]),
          Attr.create("dx", "0"),
          Attr.create("dy", "-0.06"),
          Attr.create("stdDeviation", "0.015"),
        ],
        [],
      ),
    ],
  );
