let insert_tiles =
    (
      (steps, tile_index): ZPath.tile_path,
      tiles: list(UHExp.tile),
      e: UHExp.t,
    )
    : option(UHExp.t) => {
  switch (ZPath.get_subexp(steps, e)) {
  | None => None
  | Some((wrapper, (_, ts))) =>
    ts
    |> ListUtil.split_at(tile_index)
    |> Option.map(((prefix, suffix)) =>
         wrapper(UHExp.mk(List.concat([prefix, tiles, suffix])))
       )
  };
};

let remove_tiles =
    (start_index: int, end_index: int, (_, tiles): UHExp.t)
    : option((list(UHExp.tile), UHExp.t)) => {
  tiles
  |> ListUtil.split_sublist(start_index, end_index)
  |> Option.map(((prefix, removed, suffix)) =>
      (removed, UHExp.mk(prefix @ suffix))
    );
};

/*
let remove_tiles =
    (steps: ZPath.steps, (start_index, end_index): (int, int), e: UHExp.t)
    : option((list(UHExp.tile), UHExp.t)) => {
  switch (ZPath.get_subexp(steps, e)) {
  | None => None
  | Some((wrapper, (_, tiles))) =>
    tiles
    |> ListUtil.split_sublist(start_index, end_index)
    |> Option.map(((prefix, removed, suffix)) =>
         (removed, wrapper(UHExp.mk(prefix @ suffix)))
       )
  };
};
*/

let move_tiles =
    // TODO create selection type
    (
      (remove_steps: ZPath.steps, remove_range: (int, int)),
      insert_path: ZPath.tile_path,
      e: UHExp.t,
    )
    : option(UHExp.t) =>
  e
  |> remove_tiles(remove_steps, remove_range)
  |> Option.map(((tiles, e)) => e |> insert_tiles(insert_path, tiles))
  |> Option.join;

let restructure =
    (
      (anchor_steps, anchor_index) as anchor_path: ZPath.tile_path,
      (focus_steps, focus_index) as focus_path: ZPath.tile_path,
      (target_steps, target_index): ZPath.tile_path,
      e: UHExp.t,
    )
    : option(UHExp.t) => {
  let anchor_before_focus = ZPath.is_before(anchor_path, focus_path);
  // 1 + (2 + 3[)] + 4
  //               ---
  //               remove these tiles
  //               steps: []
  //               range: (3, 5)
  //           |
  //           insert them here
  //           steps: [(2, 1)]
  //           insert index: 3
  // --> 1 + (2 + 3 + 4[)]

  // 1 + ((2 + 3[) + 4)] + 5
  //                     ---
  //                     remove these tiles
  //            |
  //            insert them here
  // --> 1 + ((2 + 3 + 5) + 4)

  // 1 + (2 + 3[) + (]4 + 5)
  //                  -
  //                  remove this tile
  //           |
  //           insert it here
  // --> 1 + (2 + 3 4) + (_ + 5)

  // 1 + [if 2 < 3 then] 4 else 5
  // ---
  // remove these tiles
  //                    |
  //                    insert them here
  // --> if 2 < 3 then 1 + 4 else 5

  // 1 + (2 + [3)] + 4 --> 1 + (2 + _ + 4 3)
  let (
    remove_range: (ZPath.steps, (int, int)),
    insertion_path: ZPath.tile_path,
  ) =
    switch (target_steps == anchor_steps, target_steps == focus_steps) {
    | (true, true) =>
      failwith(
        "restructuring mode should only be entered when selection contains unmatched delimiter",
      )
    | (false, false) =>
      // TODO return None
      failwith("invalid target")
    | (true, false) =>
      // TODO think I'm not considering illegal cases here,
      // where target is on other side in outer tile context
      let remove_range =
        anchor_before_focus
          ? (target_index, anchor_index) : (anchor_index, target_index);
      ((target_steps, remove_range), focus_path);
    | (false, true) =>
      let remove_range =
        anchor_before_focus
          ? (focus_index, target_index) : (target_index, focus_index);
      ((target_steps, remove_range), anchor_path);
    };
  e |> move_tiles(remove_range, insertion_path);
};

/**
 * assumptions:
 * - same erasure
 * - anchor before focus
 */
let rec restructure = (
  anchor: ZExp.t,
  focus: ZExp.t,
  target: ZExp.t,
): option(ZExp.t) => {
  let anchor_i = List.length(anchor.ztiles.prefix);
  let focus_i = List.length(focus.ztiles.prefix);
  let target_i = List.length(target.ztiles.prefix);
  let same_ztile = anchor_i == focus_i && focus_i == target_i;
  switch (
    anchor.ztiles.z,
    focus.ztiles.z,
    target.ztiles.z,
  ) {
  | (Z, Z, Z) =>
    // restructuring mode should only be entered when
    // selection contains an unmatched delimiter
    None

  | (Z, _not_Z, Z) when target_i <= anchor_i =>


  | (ParenZ(anchor), ParenZ(focus), ParenZ(target)) when same_ztile =>
    restructure(anchor, focus, target)
    |> Option.map(ze => ZExp.put_ztile(ParenZ(ze), target))
  | (IfZ_if(anchor, then_clause), IfZ_if(focus, _), IfZ_if(target, _))
      when same_ztile =>
    restructure(anchor, focus, target)
    |> Option.map(ze => ZExp.put_ztile(IfZ_if(ze, then_clause), target))



    if (target_i <= anchor_i && anchor_i <= focus_i) {

      // remove tiles between target and anchor
      // insert them at focus
    } else if (focus_i < anchor_i && anchor_i <= target_i) {

    } else {
      None
    };
  | (_not_Z, Z, Z) =>
    if (target_i <= focus_i && focus_i <= anchor_i) {
    } else if (anchor_i < focus_i && focus_i <= target_i) {

    } else {
      None
    }
  }
};