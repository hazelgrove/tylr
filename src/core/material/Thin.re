open Sexplib.Std;
open Walk;
open Stds;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;

module NTMap = {
  include Maps.Make({
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = Mtrl.NT.t;
    let compare = Mtrl.NT.compare;
  });
};

//TODO: rename to t(terminal) map
module StanceMap = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  include Maps.Make({
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = Stance.t;
    let compare = Stance.compare;
  });
};

let stances = ref(StanceMap.empty);
let nts = ref(NTMap.empty);

module ThinSwing = {
  [@deriving (show({with_path: false}), sexp)]
  type t = Chain.t(int, unit);

  let t_of_swing = (swing: Swing.t) => {
    Chain.map_loop(
      nt =>
        try(NTMap.find(nt, nts^)) {
        | _ => failwith("nt failed: " ++ Mtrl.NT.show(nt))
        },
      swing,
    );
  };
};

module ThinWalk = {
  [@deriving (show({with_path: false}), sexp)]
  type t = Chain.t(ThinSwing.t, int);

  //Convert the walk into int chain based on stance and swing maps
  let t_of_walk = (walk: Walk.T.t) => {
    Chain.map(
      swing => ThinSwing.t_of_swing(swing),
      stance =>
        try(StanceMap.find(stance, stances^)) {
        | _ => failwith("stance failed: " ++ Stance.show(stance))
        },
      walk,
    );
  };
};

module ThinEnd = {
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t = Bound.t(int);

  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t_ = t;

  let t_of_end = (end_: End.t) => {
    switch (end_) {
    | Bound.Root => Bound.Root
    | Node(stance) => Node(StanceMap.find(stance, stances^))
    };
  };

  module Map = {
    include Maps.Make({
      [@deriving (show({with_path: false}), sexp, yojson, ord)]
      type t = t_;
      let compare = compare;
    });

    let make = (f: 'x => 'b, map) => {
      map
      |> End.Map.to_seq
      |> Seq.map(((end_, v)) => (t_of_end(end_), f(v)))
      |> of_seq;
    };
  };
};

module ThinIndex = {
  [@deriving (show({with_path: false}), sexp)]
  type t = ThinEnd.Map.t(list(ThinWalk.t));

  let t_of_index = (index: Index.t) => {
    index
    |> Index.to_list
    |> List.map(((end_, walks)) => {
         (ThinEnd.t_of_end(end_), List.map(ThinWalk.t_of_walk, walks))
       })
    |> List.to_seq
    |> ThinEnd.Map.of_seq;
  };
};

module ThinNT = {
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t = int;

  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t_ = t;

  let t_of_nt = (nt: Mtrl.NT.t) =>
    try(NTMap.find(nt, nts^)) {
    | _ => failwith("nt failed: " ++ Mtrl.NT.show(nt))
    };

  module Map = {
    include Maps.Make({
      [@deriving (show({with_path: false}), sexp, yojson, ord)]
      type t = t_;
      let compare = compare;
    });

    let make = (f: 'x => 'b, map) => {
      map
      |> Mtrl.NT.Map.to_seq
      |> Seq.map(((end_, v)) => (t_of_nt(end_), f(v)))
      |> of_seq;
    };
  };
};

module FlippedStanceMap = {
  include Map.Make({
    type t = int;
    let compare = Int.compare;
  });
};

module FlippedNTMap = {
  include Map.Make({
    type t = int;
    let compare = Int.compare;
  });
};

let walk_map_of_thin =
    (thin: ThinEnd.Map.t(ThinIndex.t), stances_flipped, nts_flipped)
    : End.Map.t(Index.t) => {
  let end_of_thin = (end_: ThinEnd.t) => {
    switch (end_) {
    | Bound.Root => Bound.Root
    | Node(stance) => Node(FlippedStanceMap.find(stance, stances_flipped))
    };
  };
  let swing_of_thin = (thin: ThinSwing.t) => {
    Chain.map_loop(nt => FlippedNTMap.find(nt, nts_flipped), thin);
  };
  let walk_of_thinwalk = (thin: Chain.t(ThinSwing.t, int)) => {
    Chain.map(
      swing => swing_of_thin(swing),
      stance => FlippedStanceMap.find(stance, stances_flipped),
      thin,
    );
  };

  let index_of_thin = (thin: ThinIndex.t): Index.t => {
    thin
    |> ThinEnd.Map.to_seq
    |> Seq.map(((end_, walks)) =>
         (end_of_thin(end_), List.map(walk_of_thinwalk, walks))
       )
    |> Index.of_seq;
  };
  thin
  |> ThinEnd.Map.to_seq
  |> Seq.map(((end_, v)) => (end_of_thin(end_), index_of_thin(v)))
  |> End.Map.of_seq;
};

let enter_map_of_thin =
    (thin: ThinNT.Map.t(ThinIndex.t), stances_flipped, nts_flipped)
    : Mtrl.NT.Map.t(Index.t) => {
  let end_of_thin = (end_: ThinEnd.t) => {
    switch (end_) {
    | Bound.Root => Bound.Root
    | Node(stance) => Node(FlippedStanceMap.find(stance, stances_flipped))
    };
  };
  let swing_of_thin = (thin: ThinSwing.t) => {
    Chain.map_loop(nt => FlippedNTMap.find(nt, nts_flipped), thin);
  };
  let walk_of_thinwalk = (thin: Chain.t(ThinSwing.t, int)) => {
    Chain.map(
      swing => swing_of_thin(swing),
      stance => FlippedStanceMap.find(stance, stances_flipped),
      thin,
    );
  };
  let index_of_thin = (thin: ThinIndex.t): Index.t => {
    thin
    |> ThinEnd.Map.to_seq
    |> Seq.map(((end_, walks)) =>
         (end_of_thin(end_), List.map(walk_of_thinwalk, walks))
       )
    |> Index.of_seq;
  };

  let nt_of_thin = (thin: ThinNT.t) => {
    FlippedNTMap.find(thin, nts_flipped);
  };

  thin
  |> ThinNT.Map.to_seq
  |> Seq.map(((nt, v)) => (nt_of_thin(nt), index_of_thin(v)))
  |> Mtrl.NT.Map.of_seq;
};
