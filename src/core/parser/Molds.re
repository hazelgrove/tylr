let add = (lbl, mold) =>
  Label.Map.update(
    lbl,
    fun
    | None => Some([mold])
    | Some(ms) => Some([mold, ...ms]),
  );

let index: Label.Map.t(list(Mold.t)) =
  Walker.enter_all(~from=L, Mtrl.NT.root)
  |> Walk.Index.to_list
  |> List.rev_map(fst)
  |> List.fold_left(
       map =>
         fun
         | Bound.Node(Mtrl.Tile((lbl, mold))) => add(lbl, mold, map)
         | _ => map,
       Label.Map.empty,
     );

let with_label = lbl =>
  switch (Label.Map.find_opt(lbl, index)) {
  | None => []
  | Some(ms) => List.rev(ms)
  };
