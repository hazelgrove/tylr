open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;
open Util;

// stance phase of gait cycle is when foot is on the ground.
// in this setting, this is when grammar walk reaches a T sym.
module Stance = Mtrl.T;

// swing phase of gait cycle is when foot is in the air.
// in this setting, this is when grammar walk reaches an NT sym.
module Swing = {
  // a swing is represented by a non-empty list of NTs where all
  // but the last are swung "into" (ie expanded in a grammar derivation
  // step) and the last is swung "over" to arrive at the next stance.
  [@deriving (sexp, yojson, ord)]
  type t = Chain.t(Mtrl.NT.t, unit);
  let pp = (out, sw: t) => {
    let pp_lps = Fmt.(list(~sep=semi, Mtrl.NT.pp));
    Fmt.pf(out, "[@[<hv>%a@]]", pp_lps, Chain.loops(sw));
  };
  let show = Fmt.to_to_string(pp);
  let unit: Mtrl.NT.t => t = Chain.unit;
  let empty: t = unit(Space(false));
  let space: t = unit(Space(true));
  let mk = (nts: list(Mtrl.NT.t)) => {
    let n = List.length(nts);
    Chain.mk(nts, List.init(n - 1, _ => ()));
  };
  let height = (sw: t) => List.length(Chain.loops(sw)) - 1;
  let mk_eq = Chain.unit;
  let is_eq = s => height(s) == 0;
  let is_neq = s => !is_eq(s);
  let top = Chain.ft;
  let bot = Chain.hd;
  // let rev = sw => Chain.rev(sw);
  // let has_sort = s =>
  //   Chain.loops(s)
  //   |> List.exists(
  //        fun
  //        | Bound.Root
  //        | Node(Molded.{mtrl: Mtrl.Tile(_), _}) => true
  //        | Node({mtrl: Space | Grout, _}) => false,
  //      );
};

[@deriving (show({with_path: false}), sexp, yojson, ord)]
type t = Chain.t(Swing.t, Stance.t);

let empty: t = Chain.unit(Swing.empty);
let is_empty = (==)(empty);

let space: t = Chain.unit(Swing.space);

let strides = Chain.loops;
let height = w => List.length(List.filter(Swing.is_neq, strides(w)));

// let has_sort = w => List.exists(Swing.has_sort, strides(w));

let hd = Chain.hd;
let ft = Chain.ft;
// let rev = w => Chain.rev(~rev_loop=Swing.rev, w);

let is_eq = w => List.for_all(Swing.is_eq, Chain.loops(w));
// note: stricter than !is_eq
let is_neq = (w: t) => Swing.is_neq(hd(w)) && Swing.is_neq(ft(w));

let unit: _ => t = Chain.unit;

let append = Chain.append;
let cons = (bound: Mtrl.NT.t, w) =>
  is_empty(w)
    ? unit(Swing.unit(bound)) : Chain.map_hd(Chain.link(bound, ()), w);

module End = {
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t = Bound.t(Stance.t);
  module Map =
    Map.Make({
      type nonrec t = t;
      let compare = compare;
    });
};

module Set =
  Set.Make({
    type nonrec t = t;
    let compare = compare;
  });

module Index = {
  include End.Map;
  type t = End.Map.t(Set.t);
  let single = (dst, walk) => singleton(dst, Set.singleton(walk));
  let find = (end_: End.t, map: t) =>
    switch (find_opt(end_, map)) {
    | None => []
    | Some(ws) => Set.elements(ws)
    };
  let add = (dst, w) =>
    update(
      dst,
      fun
      | None => Some(Set.singleton(w))
      | Some(ws) => Some(Set.add(w, ws)),
    );
  let filter = f => map(Set.filter(f));
  let map = f => map(Set.map(f));
  let iter = f => iter((dst, ws) => Set.iter(f(dst), ws));
  let union: (t, t) => t = union((_, l, r) => Some(Set.union(l, r)));
  let union_all: list(t) => t = List.fold_left(union, empty);
  let to_list = bindings;
  let of_list = bs => of_seq(List.to_seq(bs));
  module Syntax = {
    let return = single;
    let ( let* ) = (ind, f) =>
      to_list(ind)
      |> List.concat_map(((dst, walks)) =>
           Set.elements(walks) |> List.map(w => (dst, w))
         )
      |> List.map(f)
      |> union_all;
  };
};
