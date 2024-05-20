module Base = {
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t('g, 't) =
    | Space
    | Grout('g)
    | Tile('t);
};
include Base;

let tile = t => Tile(t);

let is_space =
  fun
  | Space => true
  | Grout(_)
  | Tile(_) => false;
let is_grout =
  fun
  | Grout(_) => true
  | Space
  | Tile(_) => false;

let map = (~grout, ~tile) =>
  fun
  | Space => Space
  | Grout(g) => Grout(grout(g))
  | Tile(t) => Tile(tile(t));

module T = {
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t = Base.t((Sort.t, Tip.s), (Label.t, Mold.t));
  module Map =
    Map.Make({
      type nonrec t = t;
      let compare = compare;
    });
  let padding =
    fun
    | Space => Padding.none
    | Grout(_) => Padding.mk()
    | Tile((lbl, _)) => Label.padding(lbl);
};
module NT = {
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t = Base.t(Sort.t, (Sort.t, Mold.t));
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

module Space_or = {
  type t('spc, 'or) =
    | Space('spc)
    | Or('or);
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
