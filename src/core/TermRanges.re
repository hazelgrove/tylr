include Id.Map;
type range = (Id.t, Id.t);
type nonrec t = t(range);

let union = union((_, range, _) => Some(range));

let rec mk = (seg: Segment.t) => {
  let rec go = (skel: Skel.t): (range, t) => {
    let root_id = Piece.id(List.nth(seg, Skel.root_index(skel)));
    let (range, unichild_map) =
      switch (skel) {
      | Op(_) => ((root_id, root_id), empty)
      | Pre(_, r) =>
        let ((_, r), map) = go(r);
        ((root_id, r), map);
      | Post(l, _) =>
        let ((l, _), map) = go(l);
        ((l, root_id), map);
      | Bin(l, _, r) =>
        let ((l, _), map_l) = go(l);
        let ((_, r), map_r) = go(r);
        ((l, r), union(map_l, map_r));
      };
    let map = Id.Map.add(root_id, range, unichild_map);
    (range, map);
  };
  let unichild_map = snd(go(Segment.skel(seg)));
  let bichild_map =
    Segment.children(seg) |> List.map(mk) |> List.fold_left(union, empty);
  union(unichild_map, bichild_map);
};
