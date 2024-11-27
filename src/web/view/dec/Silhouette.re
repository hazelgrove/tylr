open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;

open Virtual_dom.Vdom;
open Util.Svgs;

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

module Inner = {
  module Profile = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = {
      is_space: bool,
      state: L.State.t,
      block: Block.t,
    };
    let mk = (~is_space, ~state, block) => {is_space, state, block};
  };

  let blur =
    Util.Nodes.filter(
      ~attrs=[Attr.id("silhouette-inner-blur")],
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

  // magic number used to align with scaling transform applied to e
  let h_pad = 0.1;
  let v_trunc = T.v_trunc -. 0.06;

  let view = (~font, p: Profile.t) => {
    p.is_space
      ? []
      : Block.flatten(p.block)
        |> Chain.fold_left_map(
             line => {
               let min = point_of_loc(p.state.loc);
               let len = Block.Line.len(line);
               let s_eol = L.State.map(Loc.shift(len), p.state);
               let rect =
                 Rect.{min, width: Float.of_int(len), height: 1.}
                 |> Rect.pad(~x=h_pad, ~y=-. v_trunc);
               (s_eol, rect);
             },
             (s, ind, line) => {
               let s_sol = L.State.return(s, 0);
               let width = ind + Block.Line.len(line);
               let glue_rect = {
                 let intersection = min(s.loc.col - s_sol.loc.col, width);
                 Rect.{
                   min: point_of_loc(s_sol.loc),
                   width: Float.of_int(intersection),
                   height: 0.,
                 }
                 // add extra vertical padding to ensure strict intersection with
                 // lines as the orthogonal polygon contour algorithm can be finicky
                 |> Rect.pad(~x=h_pad, ~y=v_trunc +. 0.1);
               };
               let min = point_of_loc(s_sol.loc);
               let s_eol = L.State.map(Loc.shift(width), s_sol);
               let line_rect =
                 Rect.{min, width: Float.of_int(width), height: 1.}
                 |> Rect.pad(~x=h_pad, ~y=-. v_trunc);
               (s_eol, glue_rect, line_rect);
             },
           )
        |> snd
        |> Chain.to_list(Fun.id, Fun.id)
        |> OrthogonalPolygon.mk(~corner_radii=(0.2, 0.075))
        |> Util.Svgs.Path.view
        |> Util.Nodes.add_classes(["silhouette", "inner"])
        |> Stds.Lists.single
        |> Box.mk(~font, ~loc=Loc.zero)
        |> Stds.Lists.single;
  };
};

module Outer = {
  module Profile = {
    open Util.Svgs;
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = list(Rect.t);
    let mk = (~state: L.State.t, block: Block.t) => {
      Block.flatten(block)
      |> Chain.fold_left_map(
           line => {
             let min = point_of_loc(state.loc);
             let len = Block.Line.len(line);
             let s = L.State.map(Loc.shift(len), state);
             let rect =
               Rect.{min, width: Float.of_int(len), height: 1.}
               |> Rect.pad(~x=0.55, ~y=0.075)
               |> Rect.pad_left(~x=0.05);
             (s, rect);
           },
           (state, ind, line) => {
             let state = L.State.return(state, 0);
             let min = point_of_loc(state.loc);
             let width = ind + Block.Line.len(line);
             let s = L.State.map(Loc.shift(width), state);
             let rect =
               Rect.{min, width: Float.of_int(width), height: 1.}
               |> Rect.pad(~x=0.55, ~y=0.075)
               |> Rect.pad_left(~x=0.05);
             (s, (), rect);
           },
         )
      |> snd
      |> fst;
    };
  };

  let view = (~font, p: Profile.t) => {
    p
    |> Util.Svgs.OrthogonalPolygon.mk(~corner_radii=(0.35, 0.15))
    |> Util.Svgs.Path.view
    |> Util.Nodes.add_classes(["silhouette", "outer"])
    |> Stds.Lists.single
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
