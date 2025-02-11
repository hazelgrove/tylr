open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;

open Virtual_dom.Vdom;
open Util.Svgs;
open Stds;

module T = Token;
open Tylr_core;
module L = Layout;

module Style = {
  type t =
    | Space
    | Inner
    | Outer;
  let to_str =
    fun
    | Space => "outer"
    | Inner => "inner"
    | Outer => "outer";
};

let point_of_loc = (loc: Loc.t) =>
  Point.{x: Float.of_int(loc.col), y: Float.of_int(loc.row)};

module Contour = {
  module Profile = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = {
      row: int,
      cols: list((int, int)),
    };

    let mk = (states: list((L.State.t, L.State.t))) => {
      assert(states != []);
      let row = fst(List.hd(states)).loc.row;
      let cols =
        states
        |> Lists.folding_map(
             ~init=Int.max_int,
             ~f=(min_sol: int, (s_sol: L.State.t, s_eol: L.State.t)) => {
               let sol = Int.min(min_sol, s_sol.loc.col);
               let eol = s_eol.loc.col;
               (sol, (sol, eol));
             },
           );
      {row, cols};
    };
  };
};

module Inner = {
  module Profile = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = {
      is_space: bool,
      contour: Contour.Profile.t,
    };

    let mk = (~is_space, states: list((L.State.t, L.State.t))) => {
      is_space,
      contour: Contour.Profile.mk(states),
    };
  };

  // magic number used to align with scaling transform applied to e
  let h_pad = 0.1;
  let v_trunc = T.v_trunc -. 0.06;

  let intersection = ((sol, eol), (sol', eol')) => (
    max(sol, sol'),
    min(eol, eol'),
  );

  let view_contour = (row: int, cols: list((int, int))) => {
    assert(cols != []);
    let n = List.length(cols);
    Chain.mk(
      List.mapi((i, c) => (i, c), cols),
      List.init(n - 1, Fun.const()),
    )
    |> Chain.fold_left_map(
         ((_, (sol, eol))) => {
           let min = Point.mk_(~x=sol, ~y=row);
           let rect =
             Rect.mk_(~min, ~width=eol - sol, ~height=1)
             |> Rect.pad(~x=h_pad, ~y=-. v_trunc);
           ((sol, eol), rect);
         },
         ((last_sol, last_eol), (), (i, (sol, eol))) => {
           let glue_rect = {
             let (sol', eol') =
               intersection((sol, eol), (last_sol, last_eol));
             let min = Point.mk_(~x=sol', ~y=row + i);
             Rect.mk_(~min, ~width=eol' - sol', ~height=0)
             // add extra vertical padding to ensure strict intersection with
             // lines as the orthogonal polygon contour algorithm can be finnicky
             |> Rect.pad(~x=h_pad, ~y=v_trunc +. 0.1);
           };
           let line_rect = {
             let min = Point.mk_(~x=sol, ~y=row + i);
             Rect.mk_(~min, ~width=eol - sol, ~height=1)
             |> Rect.pad(~x=h_pad, ~y=-. v_trunc);
           };
           ((sol, eol), glue_rect, line_rect);
         },
       )
    |> snd
    |> Chain.to_list(Fun.id, Fun.id)
    |> (
      rects => {
        rects |> List.iter(rect => P.show("rect", Rect.show(rect)));
        rects;
      }
    )
    |> OrthogonalPolygon.mk(~corner_radii=(0.2, 0.075))
    |> Util.Svgs.Path.view
    |> Util.Nodes.add_classes(["silhouette", "inner"]);
  };

  let disjoint = ((sol, eol), (sol', eol')) => sol > eol' || eol < sol';

  let view = (~font, p: Profile.t) => {
    p.is_space
      ? []
      : p.contour.cols
        |> Lists.group(~break=disjoint)
        |> Lists.folding_map(~init=p.contour.row, ~f=(row, cols) =>
             (row + List.length(cols), view_contour(row, cols))
           )
        |> Box.mk(~font, ~loc=Loc.zero)
        |> Stds.Lists.single;
  };
};

module Outer = {
  module Profile = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = Contour.Profile.t;

    let mk = Contour.Profile.mk;
  };

  let view_contour = (row: int, cols: list((int, int))) => {
    open Util.Svgs;
    assert(cols != []);
    cols
    |> List.mapi((i, (sol, eol)) => {
         let min = Point.mk_(~x=sol, ~y=row + i);
         Rect.mk_(~min, ~width=eol - sol, ~height=1)
         |> Rect.pad(~x=0.55, ~y=0.075)
         // extra left padding to account for vertical child borders
         |> Rect.pad_left(~x=0.05);
       })
    |> OrthogonalPolygon.mk(~corner_radii=(0.2, 0.075))
    |> Path.view
    |> Util.Nodes.add_classes(["silhouette", "outer"]);
  };

  let disjoint = ((sol, eol), (sol', eol')) => sol > eol' || eol < sol';

  let view = (~font, p: Profile.t) => {
    p.cols
    |> Lists.group(~break=disjoint)
    |> Lists.folding_map(~init=p.row, ~f=(row, cols) =>
         (row + List.length(cols), view_contour(row, cols))
       )
    |> Box.mk(~font, ~loc=Loc.zero);
  };

  let blur =
    Util.Nodes.filter(
      ~attrs=[Attr.id("silhouette-outer-blur")],
      [
        Node.create_svg(
          "feGaussianBlur",
          ~attrs=[
            Attr.create("in", "SourceGraphic"),
            Attr.create("stdDeviation", "0.06,0.03"),
          ],
          [],
        ),
      ],
    );
};
