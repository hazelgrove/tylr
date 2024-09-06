open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;

open Virtual_dom.Vdom;
open Tylr_core;
open Util.Svgs;

module Style = {
  [@deriving (show({with_path: false}), sexp, yojson)]
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
  [@deriving (show({with_path: false}), sexp, yojson)]
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

let hexagon = (style: Style.t, length: int): Node.t =>
  List.flatten([
    Path.[m(~x=0, ~y=0), h(~x=length)],
    tip(snd(style.shape)),
    Path.[h(~x=0)],
    Path.scale(-1., tip(fst(style.shape))),
  ])
  |> Util.Svgs.Path.view
  |> Util.Nodes.add_classes([
       "tile-path",
       "raised",
       "indicated",
       Sort.to_str(style.sort),
     ]);
let top_bar = (style: Style.t, length: int): Node.t => {
  // we draw this bar with rounded linecaps so need to truncate length
  // to get rounded ends to align with the hexagon vertices
  let roundcap_trunc = 0.1;
  Util.Svgs.Path.view(
    Path.[
      M({y: 0., x: -. adj(fst(style.shape)) +. roundcap_trunc}),
      H_({
        dx:
          Float.of_int(length)
          +. adj(fst(style.shape))
          +. adj(snd(style.shape))
          -. 2.
          *. roundcap_trunc,
      }),
    ],
  )
  |> Util.Nodes.add_classes(["tok-bar", Sort.to_str(style.sort)]);
};

let mk = (prof: Profile.t) =>
  prof.style
  |> Option.map(style =>
       [hexagon(style, prof.len), top_bar(style, prof.len)]
     )
  |> Option.to_list
  |> List.concat
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
