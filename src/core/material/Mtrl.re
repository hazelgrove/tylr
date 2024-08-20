module Base = {
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t('s, 'g, 't) =
    | Space('s)
    | Grout('g)
    | Tile('t);
};
include Base;

let space = s => Space(s);
let grout = g => Grout(g);
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
let is_tile =
  fun
  | Tile(_) => true
  | Space(_)
  | Grout(_) => false;

let map = (~space, ~grout, ~tile) =>
  fun
  | Space(s) => Space(space(s))
  | Grout(g) => Grout(grout(g))
  | Tile(t) => Tile(tile(t));

module Sorted = {
  type t = Base.t(unit, Sort.t, Sort.t);
  // currently used specifically for walk comparison based on
  // stance sorted mtrl at each prec level
  let compare = (l: t, r: t) =>
    switch (l, r) {
    | (Space (), Space ()) => 0
    | (Space (), _) => (-1)
    | (_, Space ()) => 1
    | (Grout(l), Grout(r)) => Sort.compare(l, r)
    | (Grout(_), _) => (-1)
    | (_, Grout(_)) => 1
    | (Tile(l), Tile(r)) => Sort.compare(l, r)
    };
};

module T = {
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t = Base.t(Space.T.t, Grout.T.t, Tile.T.t);
  // let compare = (l: t, r: t) =>
  module Map =
    Map.Make({
      type nonrec t = t;
      let compare = compare;
    });
  let sort: t => Sorted.t =
    map(~space=Fun.const(), ~grout=Grout.T.sort, ~tile=Tile.T.sort);
  let padding =
    fun
    | Space(_) => Padding.none
    | Grout(g) => Grout.T.padding(g)
    | Tile((lbl, _)) => Label.padding(lbl);
};
module NT = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Base.t(Space.NT.t, Grout.NT.t, Tile.NT.t);
  let compare = (l: t, r: t) =>
    switch (l, r) {
    | _ when l == r => 0
    | (Space(l), Space(r)) => Space.NT.compare(l, r)
    | (Space(_), _) => (-1)
    | (_, Space(_)) => 1
    | (Grout(l), Grout(r)) => Grout.NT.compare(l, r)
    | (Grout(_), _) => (-1)
    | (_, Grout(_)) => 1
    | (Tile(l), Tile(r)) => Tile.NT.compare(l, r)
    };
  let is_open: t => bool = (!=)(Space(Space.NT.Closed));
  let root = Tile(Tile.NT.root);
  let sort: t => Sorted.t = map(~space=Fun.const(), ~grout=Fun.id, ~tile=fst);
  let bounds: t => _ =
    fun
    | Space(_) => Bound.(Root, Root)
    | Grout(_) => (Node(0), Node(0))
    | Tile((_, Root)) => (Root, Root)
    | Tile((_, Node(mold))) => Mold.bounds(mold);
  module Map =
    Map.Make({
      type nonrec t = t;
      let compare = compare;
    });
};

module Sym = {
  include Sym;
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t = Sym.t(T.t, NT.t);
  let of_tile: Tile.Sym.t => t = Sym.map(tile, tile);
  let of_grout: Grout.Sym.t => t = Sym.map(grout, grout);
  let all =
    List.concat([
      Space.Sym.all |> List.map(Sym.map(space, space)),
      Sort.all
      |> List.concat_map(Grout.Sym.all)
      |> List.map(Sym.map(grout, grout)),
      Sort.all
      |> List.concat_map(Tile.Sym.all)
      |> List.map(Sym.map(tile, tile)),
    ]);
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
