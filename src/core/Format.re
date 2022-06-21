//TODO(andrew): autoformatter

let id_local: ref(int) = ref(0);

let mk_id = (): int => {
  let uid = id_local^;
  id_local := id_local^ + 1;
  uid;
};
let set_id_gen = (id: int) => {
  id_local := id;
};

[@deriving show]
type padding =
  | None
  | Bi
  | Pre
  | Post;

let pad_prefs: Tile.t => (padding, padding) =
  // external, internal
  t =>
    switch (t.label) {
    | [
        "/" | "=" | "+" | "!=" | ">" | "<" | "<=" | ">=" | "|" | "||" | "&" |
        "&&" |
        "++" |
        "|>" |
        "+=",
      ] => (
        Bi,
        None,
      )
    | ["*" | ","] => (None, None)
    | ["."] => (None, None)
    | ["-"] => (None, None) //TODO mold specific logic
    | ["!"] => (None, None)
    | [_] => (None, None)
    | ["[", "]"]
    | ["(", ")"]
    | ["?", ":"] => (None, None)
    | ["fun", "->"]
    | ["if", "then", "else"]
    | ["let", "=", "in"]
    | ["case", "of"]
    | ["|", "->"] => (Post, Bi)
    | _ => (None, None)
    };

let should_pad_front: Tile.t => bool =
  t =>
    switch (pad_prefs(t)) {
    | (Bi | Pre, _) => true
    | _ => false
    };

let should_pad_back: Tile.t => bool =
  t =>
    switch (pad_prefs(t)) {
    | (Bi | Post, _) => true
    | _ => false
    };

let remove_spaces: Segment.t => Segment.t =
  List.fold_left(
    (acc, p) => {
      switch ((p: Piece.t)) {
      | Whitespace({content, _}) when content == Whitespace.space => acc
      | _ => acc @ [p]
      }
    },
    [],
  );

let new_space = _ => Piece.Whitespace({content: " ", id: mk_id()});

type last_seen =
  | Space
  | Linebreak
  | Other;

let collapse_space_runs: Segment.t => Segment.t =
  /*  collapse spaces: for each runs of spaces+linebreaks, eliminate the spaces.
      if the result is empty, return a single space */
  ps => {
    let (_, ps) =
      List.fold_left(
        ((last_seen: last_seen, acc), p: Piece.t) => {
          switch (p) {
          | Whitespace({content, _}) when content == Whitespace.space => (
              Space,
              last_seen != Other ? acc : acc @ [p],
            )
          | Whitespace({content, _}) when content == Whitespace.linebreak =>
            /* if the last thing we saw was a space, lose it */
            (
              Linebreak,
              last_seen == Space && acc != []
                ? fst(Util.ListUtil.split_last(acc)) @ [p] : acc @ [p],
            )
          | _ => (Other, acc @ [p])
          }
        },
        (Other, []),
        ps,
      );
    ps;
  };

let rec get_leading_spaces: Segment.t => Segment.t =
  fun
  | [] => []
  | [p, ...ps] =>
    switch ((p: Piece.t)) {
    | Whitespace({content, _}) when content == Whitespace.space => [
        p,
        ...get_leading_spaces(ps),
      ]
    | _ => []
    };

let should_back_pad_whole_seg: Relatives.t => bool =
  r =>
    switch (Relatives.parent(r)) {
    | None => false
    | Some(p) =>
      switch (p) {
      | Tile(t) =>
        switch (pad_prefs(t)) {
        | (_, Bi) => true
        | _ => false
        }
      | _ => false
      }
    };

let should_front_pad_whole_seg' = t =>
  switch (pad_prefs(t)) {
  | (_, Bi) => true
  | _ => false
  };

let should_front_pad_whole_seg: Relatives.t => bool =
  r =>
    switch (Relatives.parent(r)) {
    | None => false
    | Some(p) =>
      switch (p) {
      | Tile(t) => should_front_pad_whole_seg'(t)
      | _ => false
      }
    };

let front_pad_seg = (should_front_pad, seg) => {
  should_front_pad
    ? switch (seg) {
      | [] => []
      | [p, ..._] when Piece.is_whitespace(p) => seg
      | _ => [new_space(), ...seg]
      }
    : seg;
};

let back_pad_seg = (should_back_pad, seg) => {
  should_back_pad
    ? switch (List.rev(seg)) {
      | [] => []
      | [p, ..._] when Piece.is_whitespace(p) => seg
      | _ => seg @ [new_space()]
      }
    : seg;
};

let rec pad_according_to_prefs: Segment.t => Segment.t =
  /*  Fold through pieces, adding spaces before and after each tile
      according to their preferences. (this may result in doubles
      and spaces adjacent to linebreaks) */
  ps =>
    List.fold_left(
      (acc, p) => {
        switch ((p: Piece.t)) {
        | Tile(t) =>
          let t = pad_inside(t);
          acc
          @ (should_pad_front(t) ? [new_space()] : [])
          @ [Piece.Tile(t)]
          @ (should_pad_back(t) ? [new_space()] : []);
        | _ => acc @ [p]
        }
      },
      [],
      ps,
    )
and pad_inside: Tile.t => Tile.t =
  ({children, _} as tile) => {
    let should_front_pad = should_front_pad_whole_seg'(tile);
    {
      ...tile,
      children: List.map(format_segment(should_front_pad), children),
    };
  }
and format_segment: (bool, Segment.t) => Segment.t =
  (should_front_pad, x) =>
    x
    |> remove_spaces
    |> pad_according_to_prefs
    |> front_pad_seg(should_front_pad)
    |> back_pad_seg(should_front_pad)
    |> collapse_space_runs;

let format =
    ({siblings: (l_sibs, r_sibs), _} as relatives: Relatives.t, id_gen) => {
  /*  note that this currently doesn't go into ancestors */
  // when making a decision about what to add, use is_whitespace
  // when makeing a decision about what to remove, use is SPACE
  set_id_gen(id_gen);
  let trailing_spaces = l_sibs |> List.rev |> get_leading_spaces |> List.rev;
  let should_inside_pad =
    switch (Relatives.parent(relatives)) {
    | None => false
    | Some(p) =>
      switch (p) {
      | Tile(t) => should_front_pad_whole_seg'(t)
      | _ => false
      }
    };
  let l_sibs = format_segment(should_inside_pad, l_sibs);
  //supress trailing spaces; want to leave user in control of ws before caret
  let l_sibs =
    switch (List.rev(l_sibs)) {
    | [Whitespace({content, _}), ...rest] when content == Whitespace.space =>
      List.rev(rest)
    | _ => l_sibs
    };
  let l_sibs = l_sibs @ trailing_spaces;
  let l_sibs =
    should_front_pad_whole_seg(relatives)
      ? switch (l_sibs) {
        | _ when l_sibs == [] => [] //respect user's empty head
        // don't add if there's already a space (shouldn't be?)
        | [p, ..._] when Piece.is_whitespace(p) => l_sibs
        | _ => [new_space(), ...l_sibs]
        }
      : l_sibs;
  let l_sibs_ends_in_ws =
    switch (List.rev(l_sibs)) {
    | [p, ..._] when Piece.is_whitespace(p) => true
    | _ => false
    };
  let r_sibs =
    l_sibs == [] && should_inside_pad ? [new_space()] @ r_sibs : r_sibs;
  //TODO: if l_sibs ends up empty, possibly left-pad the r-sibs based on parent pref
  //TODO: not sure this works
  let r_sibs = format_segment(should_inside_pad, r_sibs);
  // if the l_sibs ends in a ws or lb and the rsibs ends in a ws, remove the ws
  let r_sibs =
    switch (l_sibs_ends_in_ws, r_sibs) {
    | (true, [Whitespace({content, _}), ...rest])
        when content == Whitespace.space => rest
    | (true, []) => []
    | _ =>
      should_back_pad_whole_seg(relatives)
        ? switch (List.rev(r_sibs)) {
          // don't add if there's already a space
          | [p, ..._] when Piece.is_whitespace(p) => r_sibs
          | _ => r_sibs @ [new_space()]
          }
        : r_sibs
    };
  //TODO:!!!!!!!!!!!!!!!!!!!!!!!!!
  // TODO: if any spaces get added to the right of the caret, move them to the left
  let (l_sibs, r_sibs) =
    switch (r_sibs) {
    | [Whitespace({content, _}) as p, ...rest]
        when content == Whitespace.space => (
        l_sibs @ [p],
        rest,
      )
    | _ => (l_sibs, r_sibs)
    };
  ((l_sibs, r_sibs), id_local^);
};
