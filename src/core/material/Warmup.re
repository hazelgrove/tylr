open Walk;
open Walker;
open Sexplib.Std;
open Stds;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;

//TODO: look for any bad mistakes in our logic

module NTMap = {
  include Maps.Make({
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = Mtrl.NT.t;
    let compare = Mtrl.NT.compare;
  });
};

//TODO: rename to t(terminal) map
module StanceMap = {
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
        | _ =>
          failwith(
            "nt failed: " ++ Mtrl.NT.show(nt),
            // ++ Sexplib.Std.string_of_sexp(Swing.sexp_of_t(swing)),
          )
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
        | _ =>
          failwith(
            "stance failed: " ++ Stance.show(stance),
            // ++ Sexplib.Std.string_of_sexp(Stance.sexp_of_t(stance)),
          )
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

let init_stances_swings = () => {
  let enter_all = Walker.walk_all(~from=L, Root);

  enter_all
  |> Walk.Index.to_list
  |> List.rev_map(fst)
  |> List.iteri((idx, end_) => {
       switch (end_) {
       | Bound.Root => ()
       | Node(stance) => stances := StanceMap.add(stance, idx, stances^)
       }
     });

  let idx = ref(0);
  enter_all
  |> Walk.Index.to_list
  |> List.rev_map(snd)
  |> List.concat
  |> List.iter(walk => {
       Walk.swings(walk)
       |> List.iter(swing => {
            Chain.loops(swing)
            |> List.iter(nt =>
                 switch (NTMap.find_opt(nt, nts^)) {
                 | Some(_) => ()
                 | None =>
                   nts := NTMap.add(nt, idx^, nts^);
                   idx := idx^ + 1;
                 }
               )
          })
     });

  print_endline(
    "Stances len: " ++ string_of_int(StanceMap.cardinal(stances^)),
  );
  print_endline("nts len: " ++ string_of_int(NTMap.cardinal(nts^)));
};

//NOTE: warmup function does "everything" - currently isn't being used
let warmup = () => {
  print_endline("Warmup function called");

  init_stances_swings();

  let (ts, nts) =
    Mtrl.Sym.all |> List.partition_map(Sym.get(Either.left, Either.right));

  let root_l_walk: Index.t = walk_all(~from=L, Root);
  let root_r_walk: Index.t = walk_all(~from=R, Root);

  print_endline("Getting ts_l_walks");
  let ts_l_walks =
    List.map(
      t => {
        let walk_l: Index.t = walk_all(~from=L, Node(t));
        (Bound.Node(t), walk_l);
      },
      ts,
    )
    |> List.to_seq
    |> End.Map.of_seq
    |> End.Map.add(Bound.Root, root_l_walk);

  print_endline("Converting ts_l_walks to sexp");
  let ts_l_walked_maps_sexp =
    ThinEnd.Map.sexp_of_t(
      ThinIndex.sexp_of_t,
      ThinEnd.Map.make(ThinIndex.t_of_index, ts_l_walks),
    );

  print_endline("Getting ts_r_walks");
  let ts_r_walks =
    List.map(
      t => {
        let walk_r: Index.t = walk_all(~from=R, Node(t));
        (Bound.Node(t), walk_r);
      },
      ts,
    )
    |> List.to_seq
    |> End.Map.of_seq
    |> End.Map.add(Bound.Root, root_r_walk);

  print_endline("Getting nts_r_walks");
  let nts_r_walks =
    List.map(
      nt => {
        let walk_r: Index.t = enter_all(~from=R, nt);
        (nt, walk_r);
      },
      nts,
    )
    |> List.to_seq
    |> Mtrl.NT.Map.of_seq;

  print_endline("Getting nts_l_walks");
  let nts_l_walks =
    List.map(
      nt => {
        let walk_l: Index.t = enter_all(~from=L, nt);
        (nt, walk_l);
      },
      nts,
    )
    |> List.to_seq
    |> Mtrl.NT.Map.of_seq;

  print_endline("Converting ts_l_walks to string");
  let ts_l_walked_maps_str =
    Sexplib.Std.string_of_sexp(ts_l_walked_maps_sexp);

  print_endline("Converting ts_r_walks to string");
  let ts_r_walked_maps_str =
    Sexplib.Std.string_of_sexp(
      End.Map.sexp_of_t(
        Index.sexp_of_t(Sexplib.Std.sexp_of_list(Walk.sexp_of_t)),
        ts_r_walks,
      ),
    );

  print_endline("Converting nts_l_walks to string");
  let nts_l_enter_maps_str =
    Sexplib.Std.string_of_sexp(
      Mtrl.NT.Map.sexp_of_t(
        Index.sexp_of_t(Sexplib.Std.sexp_of_list(Walk.sexp_of_t)),
        nts_l_walks,
      ),
    );

  print_endline("Converting nts_r_walks to string");
  let nts_r_enter_maps_str =
    Sexplib.Std.string_of_sexp(
      Mtrl.NT.Map.sexp_of_t(
        Index.sexp_of_t(Sexplib.Std.sexp_of_list(Walk.sexp_of_t)),
        nts_r_walks,
      ),
    );

  print_endline("ts_l_walked_maps_str");
  print_endline(ts_l_walked_maps_str);
  print_endline("nts_l_enter_maps_str");
  print_endline(nts_l_enter_maps_str);

  let _ =
    Core.Out_channel.write_all("walk_l_map.txt", ~data=ts_l_walked_maps_str);
  let _ =
    Core.Out_channel.write_all("walk_r_map.txt", ~data=ts_r_walked_maps_str);
  let _ =
    Core.Out_channel.write_all("enter_map.txt", ~data=nts_l_enter_maps_str);
  let _ =
    Core.Out_channel.write_all("enter_map.txt", ~data=nts_r_enter_maps_str);

  ();
};
