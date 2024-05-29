open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;

module Base = {
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t('s, 'g, 't) =
    | Space('s)
    | Grout('g)
    | Tile('t);
};
include Base;

let tile = t => Tile(t);

let is_space =
  fun
  | Space(_) => true
  | Grout(_)
  | Tile(_) => false;
let is_grout =
  fun
  | Grout(_) => true
  | Space(_)
  | Tile(_) => false;

let map = (~space, ~grout, ~tile) =>
  fun
  | Space(s) => Space(space(s))
  | Grout(g) => Grout(grout(g))
  | Tile(t) => Tile(tile(t));

module T = {
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t = Base.t(unit, Grout.T.t, Tile.T.t);
  module Map =
    Map.Make({
      type nonrec t = t;
      let compare = compare;
    });
  let padding =
    fun
    | Space(_) => Padding.none
    | Grout(_) => Padding.mk()
    | Tile((lbl, _)) => Label.padding(lbl);
};
module NT = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Base.t(bool, Grout.NT.t, Tile.NT.t);
  let compare = (l: t, r: t) =>
    switch (l, r) {
    | _ when l == r => 0
    | (Space(false), _) => (-1)
    | (_, Space(false)) => 1
    | (Space(true), _) => (-1)
    | (_, Space(true)) => 1
    | (Grout(l), Grout(r)) => Grout.NT.compare(l, r)
    | (Grout(_), _) => (-1)
    | (_, Grout(_)) => 1
    | (Tile(l), Tile(r)) => Tile.NT.compare(l, r)
    };
  let root = Tile(Tile.NT.root);
  let sort = _ => failwith("todo Mtrl.NT.sort");
  // let root = Tile(Sort.root);
  // let bounds =
  //   fun
  //   | Bound.Node(Tile((sort, mold: Mold.t))) when sort == mold.sort =>
  //     Mold.bounds(mold)
  //   // hack: morally grout may need different bounds but shouldn't matter rn
  //   | _ => (Root, Root);
  module Map =
    Map.Make({
      type nonrec t = t;
      let compare = compare;
    });
};

// module T = Molded;
// module NT = Sorted;
// module Sym = {
//   include Sym;
//   [@deriving (show({with_path: false}), sexp, yojson, ord)]
//   type t = Sym.t(T.t, NT.t);
//   let is_null =
//     fun
//     | T(_) => false
//     | NT(Space) => true
//     | NT(Grout | Tile(_)) => false;
// };

// module Regex = {
//   include Regex;
//   [@deriving (show({with_path: false}), sexp, yojson, ord)]
//   type t = Regex.t(Sym.t);
// };
// module RFrame = {
//   include RFrame;
//   [@deriving (show({with_path: false}), sexp, yojson, ord)]
//   type t = RFrame(Sym.t);
// };
