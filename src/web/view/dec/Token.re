open Virtual_dom.Vdom;
open Tylr_core;
open Util.Svgs;

module Profile = {
  type t = {
    len: int,
    sort: Sort.t,
    tips: Tip.s,
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
let short_tip_height = (1. -. t) *. 0.5;
let short_tip_width = (1. -. t) *. tip_width;

let run: Tip.t => float =
  fun
  | Conv => +. short_tip_width
  | Conc => -. short_tip_width;

let adj: Tip.t => float =
  fun
  | Conv => convex_adj
  | Conc => concave_adj;

let l_hook = (l: Tip.t): list(Path.cmd) => [
  H_({dx: -. adj(l)}),
  L_({dx: -. run(l), dy: (-0.5)}),
  L_({dx: +. run(l), dy: (-0.5)}),
  H_({dx: +. adj(l)}),
];

let r_hook = (r: Tip.t): list(Path.cmd) => [
  H_({dx: +. adj(r)}),
  L_({dx: +. run(r), dy: 0.5}),
  L_({dx: -. run(r), dy: 0.5}),
  H_({dx: -. adj(r)}),
];

let path = ((l, r): Tip.s, length: int): Path.t =>
  List.flatten(
    Path.[
      [m(~x=0, ~y=0), h(~x=length)],
      r_hook(r),
      [h(~x=0)],
      l_hook(l),
    ],
  );

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

let mk = (prof: Profile.t) =>
  Box.mk(
    ~width=prof.len,
    ~height=1,
    [Util.Svgs.Path.view(~attrs=[], path(prof.tips, prof.len))],
  );
