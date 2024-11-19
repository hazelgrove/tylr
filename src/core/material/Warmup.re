open Walker;
open Walk;
open Thin;

let init_stances_swings = () => {
  let enter_all: Index.t = Walker.walk_all(~from=L, Root);

  //TODO: write the importing logic to read the serialized walks/enters and see how that goes; use the full reconstructed (not thin maps) at runtime
  //TODO: serialize the indices

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
                 // print_endline("nt already exists: " ++ Mtrl.NT.show(nt))
                 | None =>
                   // print_endline("nt added: " ++ Mtrl.NT.show(nt));
                   nts := NTMap.add(nt, idx^, nts^);
                   idx := idx^ + 1;
                 }
               )
          })
     });

  let walk_count = ref(0);
  let total_walk_size = ref(0);

  enter_all
  |> Walk.Index.to_list
  |> List.rev_map(snd)
  |> List.concat
  |> List.iteri((idx, walk) => {
       walk_count := idx + 1;
       total_walk_size := total_walk_size^ + Chain.length(walk);
       ();
     });

  print_endline("Walk Count: " ++ string_of_int(walk_count^));
  print_endline(
    "Average Walk Size: " ++ string_of_int(total_walk_size^ / walk_count^),
  );

  print_endline(
    "Stances len: " ++ string_of_int(StanceMap.cardinal(stances^)),
  );
  print_endline("nts len: " ++ string_of_int(NTMap.cardinal(nts^)));
};

let process_ts_l_walks = (ts, root_r_walk) => {
  print_endline("Getting ts_l_walks");

  let ts_l_walks =
    List.map(
      t => {
        let walk_l: Walk.Index.t = Walker.walk_all(~from=L, Node(t));
        (Bound.Node(t), walk_l);
      },
      ts,
    )
    |> List.to_seq
    |> Walk.End.Map.of_seq
    |> End.Map.add(Bound.Root, root_r_walk);

  print_endline("Converting ts_l_walks to sexp");
  let _ts_l_walked_maps_sexp =
    ThinEnd.Map.sexp_of_t(
      ThinIndex.sexp_of_t,
      ThinEnd.Map.make(ThinIndex.t_of_index, ts_l_walks),
    );

  let _ = Sexplib.Sexp.save("walk_l_map.txt", _ts_l_walked_maps_sexp);

  ();
};

let process_ts_r_walk = (ts, root_r_walk) => {
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

  print_endline("Converting ts_r_walks to sexp");
  let _ts_r_walked_maps_sexp =
    ThinEnd.Map.sexp_of_t(
      ThinIndex.sexp_of_t,
      ThinEnd.Map.make(ThinIndex.t_of_index, ts_r_walks),
    );

  let _ = Sexplib.Sexp.save("walk_r_map.txt", _ts_r_walked_maps_sexp);
  ();
};

let process_nts_l_walk = nts => {
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

  print_endline("Converting nts_l_walks to sexp");
  let nts_l_enter_maps_sexp =
    ThinNT.Map.sexp_of_t(
      ThinIndex.sexp_of_t,
      ThinNT.Map.make(ThinIndex.t_of_index, nts_l_walks),
    );
  let _ = Sexplib.Sexp.save("enter_l_map.txt", nts_l_enter_maps_sexp);
  ();
};

let process_nts_r_walk = nts => {
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

  print_endline("Converting nts_r_walks to sexp");
  let nts_r_enter_maps_sexp =
    ThinNT.Map.sexp_of_t(
      ThinIndex.sexp_of_t,
      ThinNT.Map.make(ThinIndex.t_of_index, nts_r_walks),
    );
  let _ = Sexplib.Sexp.save("enter_r_map.txt", nts_r_enter_maps_sexp);
  ();
};

let warmup = () => {
  print_endline("Warmup function called");

  init_stances_swings();

  let (ts, nts_list) =
    Mtrl.Sym.all |> List.partition_map(Sym.get(Either.left, Either.right));

  let root_l_walk: Index.t = walk_all(~from=L, Root);
  let root_r_walk: Index.t = walk_all(~from=R, Root);

  print_endline("got root walks");

  process_ts_l_walks(ts, root_l_walk);
  Gc.full_major();
  process_ts_r_walk(ts, root_r_walk);
  Gc.full_major();
  process_nts_l_walk(nts_list);
  Gc.full_major();
  process_nts_r_walk(nts_list);
  Gc.full_major();

  let stances_sexp = StanceMap.sexp_of_t(Sexplib.Conv.sexp_of_int, stances^);
  let nts_sexp = NTMap.sexp_of_t(Sexplib.Conv.sexp_of_int, nts^);

  let _ = Sexplib.Sexp.save("stances.txt", stances_sexp);
  let _ = Sexplib.Sexp.save("nts.txt", nts_sexp);

  ();
};
