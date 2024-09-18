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

  let mk = (~loc: Loc.t, ~null: (bool, bool), b_tok: Block.t) => {
    loc,
    len: Block.len(b_tok),
    style: Style.mk(~null, Block.mtrl(b_tok)),
  };
};
let tip_width = 0.3;
let concave_adj = 0.2;
let convex_adj = (-0.1);

// how much to truncate half-height of decorations to leave line height padding
// and avoid overlapping decorations on adjacent lines
let v_trunc = 0.05;

// how much to shift horizontal strokes to align with the edges of hexgaon decs
let stroke_shift = 0.03;

let run: Tip.t => float =
  fun
  | Conv => +. tip_width
  | Conc => -. tip_width;

let adj: Tip.t => float =
  fun
  | Conv => convex_adj
  | Conc => concave_adj;

let tip = (t: Tip.t): Path.t => [
  H_({dx: +. adj(t)}),
  L_({dx: +. run(t), dy: 0.5 -. v_trunc}),
  L_({dx: -. run(t), dy: 0.5 -. v_trunc}),
  H_({dx: -. adj(t)}),
];

let hexagon = (style: Style.t, length: int): Node.t =>
  List.flatten([
    Path.[m(~x=0, ~y=0) |> cmdfudge(~y=v_trunc), h(~x=length)],
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
  // todo: need to specialize to tip shape
  let roundcap_trunc = 0.06;
  Util.Svgs.Path.view(
    Path.[
      M({
        y: v_trunc +. stroke_shift,
        x: -. adj(fst(style.shape)) +. roundcap_trunc,
      }),
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
