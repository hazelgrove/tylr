open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;
open Virtual_dom.Vdom;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  outer: list(Silhouette.Outer.Profile.t),
  inner: list(Silhouette.Inner.Profile.t),
  cells: list(Child.Profile.t),
  tokens: list(Token.Profile.t),
};

let mk = (~outer=[], ~inner=[], ~cells=[], ~tokens=[], ()) => {
  outer,
  inner,
  cells,
  tokens,
};
let empty = {outer: [], inner: [], tokens: [], cells: []};
let cat = (l: t, r: t) => {
  let outer = l.outer @ r.outer;
  let inner = l.inner @ r.inner;
  let cells = l.cells @ r.cells;
  let tokens = l.tokens @ r.tokens;
  {outer, inner, tokens, cells};
};
let concat = List.fold_left(cat, empty);

let view = (~font, {outer, inner, tokens, cells}: t): list(Node.t) =>
  List.concat([
    List.map(Silhouette.Outer.view(~font), outer),
    List.concat_map(Silhouette.Inner.view(~font), inner),
    List.map(Child.view(~font), cells),
    List.map(Token.view(~font), tokens),
  ]);
