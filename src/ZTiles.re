type t('ztile, 'tile) = ZList.t(option('ztile), 'tile);

let z_index = (ztiles: t(_)): int => List.length(ztiles.prefix);

let erase =
    (~erase_ztile: 'ztile => 'tile, ztiles: t('ztile, 'tile)): list('tile) =>
  switch (ztiles.z) {
  | None => ztiles.prefix @ ztiles.suffix
  | Some(ztile) => ztiles.prefix @ [erase_ztile(ztile), ...ztiles.suffix]
  };

let wrap = ztile => ZList.{prefix: [], z: Some(ztile), suffix: []};

let place_before = tiles => ZList.{prefix: [], z: None, suffix: tiles};
let place_after = tiles => ZList.{prefix: tiles, z: None, suffix: []};

let move_left =
    (
      ~enter_from_right: 'tile => option('ztile),
      prefix: list('tile),
      suffix: list('tile),
    )
    : option(t('ztile, 'tile)) =>
  ListUtil.split_last_opt(prefix)
  |> Option.map(((prefix, tile)) =>
       switch (enter_from_right(tile)) {
       | None => ZList.{prefix, z: None, suffix: [tile, ...suffix]}
       | Some(_) as z => {prefix, z, suffix}
       }
     );

let move_right =
    (
      ~enter_from_left: 'tile => option('ztile),
      prefix: list('tile),
      suffix: list('tile),
    )
    : option(t('ztile, 'tile)) =>
  switch (suffix) {
  | [] => None
  | [tile, ...suffix] =>
    Some(
      switch (enter_from_left(tile)) {
      | None => {prefix: prefix @ [tile], z: None, suffix}
      | Some(_) as z => {prefix, z, suffix}
      },
    )
  };
