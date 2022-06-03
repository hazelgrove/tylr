open Core;

[@deriving show]
type tip_shape = (Nib.t, int);

[@deriving show]
type piece_shape = (tip_shape, tip_shape);

let piece_shape_of_nibs = ((l, r): Nibs.t): piece_shape => (
  (l, 0),
  (r, 0),
);
[@deriving show]
type measurement = {
  origin: int,
  length: int,
};
[@deriving show]
type padding =
  | None
  | Bi
  | Pre
  | Post;

let padding: string => padding =
  fun
  | "fun"
  | "let" => Post
  | "=>"
  | "+"
  | "-"
  | "*"
  | "/"
  | ","
  | "="
  | "in"
  | "?"
  | ":" => Bi
  | "("
  | ")"
  | "["
  | "]"
  | "}"
  | _ => None;

let relativize_measurements: (int, list(measurement)) => list(measurement) =
  parent_origin =>
    List.map(({origin, length}) =>
      {origin: origin - parent_origin, length}
    );

/*
  let rec to_measured = (~origin=0, layout: t): measured =>
    switch (layout) {
    | Atom(t, ann) =>
      let measurement = {origin, length: token_length(t)};
      {layout: AtomM(t, ann), measurement};
    | Cat(ls, ann) =>
      let (ms, final) =
        List.fold_left(
          ((ms, origin), l) => {
            let m = to_measured(l, ~origin);
            ([m, ...ms], origin + m.measurement.length);
          },
          ([], origin),
          ls,
        );
      let measurement = {origin, length: final - origin};
      {layout: CatM(List.rev(ms), ann), measurement};
    };

  let select_piece_idxs = xs =>
    List.map(idx => {
      // NOTE: This re-indexing is because of delims, NOT padding
      let i = 2 * idx + 1;
      assert(i >= 0 && i < List.length(xs));
      List.nth(xs, i);
    });

  let get_closed_children = (mold: Mold.t, ms: list(measured)): list(measured) => {
    List.map((==)(mold.sorts.out), mold.sorts.in_)
    |> ListUtil.p_indices((==)(false))
    |> select_piece_idxs(ms);
  };

  let get_open_children = (mold: Mold.t, ms: list(measured)): list(measured) => {
    List.map((==)(mold.sorts.out), mold.sorts.in_)
    |> ListUtil.p_indices((==)(true))
    |> select_piece_idxs(ms);
  };
 */
