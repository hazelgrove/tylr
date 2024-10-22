open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;

// stance phase of gait cycle is when foot is on the ground.
// in this setting, this is when grammar walk reaches a T sym.
module Stance = Mtrl.T;

// swing phase of gait cycle is when foot is in the air.
// in this setting, this is when grammar walk reaches an NT sym.
module Swing = {
  // a swing is represented by a non-empty list of NTs where all
  // but the last are swung "into" (ie expanded in a grammar derivation
  // step) and the last is swung "over" to arrive at the next stance.
  [@deriving (sexp, yojson)]
  type t = Chain.t(Mtrl.NT.t, unit);
  let pp = (out, sw: t) => {
    let pp_lps = Fmt.(list(~sep=semi, Mtrl.NT.pp));
    Fmt.pf(out, "[@[<hv>%a@]]", pp_lps, Chain.loops(sw));
  };
  let show = Fmt.to_to_string(pp);
  let unit: Mtrl.NT.t => t = Chain.unit;
  let empty: t = unit(Space(Closed));
  let space: t = unit(Space(Open));
  let root: t = unit(Tile(Tile.NT.root));
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
  let is_open = (sw: t) => Mtrl.NT.is_open(bot(sw));
  let compare = (l: t, r: t) => {
    let c = Int.compare(height(l), height(r));
    c == 0
      ? List.compare(Mtrl.NT.compare, Chain.loops(l), Chain.loops(r)) : c;
  };
  let bounds = (sw: t, ~from: Dir.t) => {
    let (l_bot, r_bot) = Mtrl.NT.bounds(bot(sw));
    let (l_top, r_top) = Mtrl.NT.bounds(top(sw));
    Dir.pick(from, ((l_top, r_bot), (l_bot, r_top)));
  };
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

module T = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Chain.t(Swing.t, Stance.t);

  //NOTE:milan
  //thin walk = Chain.t(int, int);
  //

  let unit: _ => t = Chain.unit;
  let empty = unit(Swing.empty);
  let is_empty = (==)(empty);

  let hd = Chain.hd;
  let ft = Chain.ft;
  // let rev = w => Chain.rev(~rev_loop=Swing.rev, w);

  let space = unit(Swing.space);
  let root = unit(Swing.root);

  let stances = Chain.links;
  let swings = Chain.loops;
  let height = w => List.length(List.filter(Swing.is_neq, swings(w)));

  let append = Chain.append;
  let cons = (bound: Mtrl.NT.t, w) =>
    is_empty(w)
      ? unit(Swing.unit(bound)) : Chain.map_hd(Chain.link(bound, ()), w);

  let is_eq = w => List.for_all(Swing.is_eq, Chain.loops(w));
  // note: stricter than !is_eq
  let is_neq = (w: t) => Swing.is_neq(hd(w)) && Swing.is_neq(ft(w));
  let is_valid = w => is_eq(w) || is_neq(w);

  // let has_sort = w => List.exists(Swing.has_sort, strides(w));

  let has_stance = (st: Stance.t, w: t) =>
    List.exists((==)(st), Chain.links(w));

  // a list of sorted mtrls, each describing the stances of each prec level in
  // top-down order
  let stance_sorts = (w: t): list(Mtrl.Sorted.t) =>
    w
    |> Chain.fold_left(Fun.const([]), (mtrls, st, sw) =>
         Swing.is_neq(sw) ? mtrls : [Mtrl.T.sort(st), ...mtrls]
       );

  let compare = (l: t, r: t) => {
    assert(is_valid(l) && is_valid(r));
    open Stds.Compare.Syntax;
    let/ () = Int.compare(height(l), height(r));
    // top-down lexicographic comparison
    let/ () =
      List.compare(Mtrl.Sorted.compare, stance_sorts(l), stance_sorts(r));
    let/ () = Int.compare(Chain.length(l), Chain.length(r));
    Chain.compare(Swing.compare, Stance.compare, l, r);
  };
};
include T;

module End = {
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t = Bound.t(Stance.t);
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t_ = t;
  module Map =
    Stds.Maps.Make({
      [@deriving (show({with_path: false}), sexp, yojson, ord)]
      type t = t_;
      let compare = compare;
    });
};

// module Set =
//   Set.Make({
//     type nonrec t = t;
//     let compare = compare;
//   });

module Index = {
  include End.Map;
  type t = End.Map.t(list(T.t));
  let single = (dst, walk) => singleton(dst, [walk]);
  let find = (end_: End.t, map: t) =>
    switch (find_opt(end_, map)) {
    | None => []
    | Some(ws) => ws
    };
  let add = (dst, w) =>
    update(
      dst,
      fun
      | None => Some([w])
      | Some(ws) => Some([w, ...ws]),
    );
  let filter = f => map(List.filter(f));
  let map = f => map(List.map(f));
  let iter = f => iter((dst, ws) => List.iter(f(dst), ws));
  let union: (t, t) => t = union((_, l, r) => Some(l @ r));
  let union_all: list(t) => t = List.fold_left(union, empty);
  let to_list = bindings;
  let of_list = bs => of_seq(List.to_seq(bs));
  let size = map =>
    to_list(map)
    |> List.map(((_, ws)) => List.length(ws))
    |> List.fold_left((+), 0);
  let sort = End.Map.map(List.stable_sort(T.compare));
  module Syntax = {
    let return = single;
    let ( let* ) = (ind, f) =>
      to_list(ind)
      |> List.concat_map(((dst, walks)) =>
           walks |> List.map(w => (dst, w))
         )
      |> List.map(f)
      |> union_all;
  };
};
