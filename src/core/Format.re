//TODO(andrew): autoformatter

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
        "*" | "/" | "=" | "+" | "!=" | ">" | "<" | "<=" | ">=" | "|" | "||" |
        "&" |
        "&&" |
        "++" |
        "|>" |
        "+=" |
        ",",
      ] => (
        Bi,
        None,
      )
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
    | ["|", "->"] => (Bi, Bi)
    | _ => (None, None)
    };

let should_pad_outside: Tile.t => bool =
  t =>
    switch (pad_prefs(t)) {
    | (Bi, _) => true
    | _ => false
    };

let should_pad_inside: Tile.t => bool =
  t =>
    switch (pad_prefs(t)) {
    | (_, Bi) => true
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
          @ (should_pad_outside(t) ? [Piece.space] : [])
          @ [Piece.Tile(t)]
          @ (should_pad_outside(t) ? [Piece.space] : []);
        | _ => acc @ [p]
        }
      },
      [],
      ps,
    )
and pad_inside: Tile.t => Tile.t =
  ({children, _} as tile) => {
    ...tile,
    children: List.map(pad_according_to_prefs, children),
  };

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
          | Whitespace({content, _}) when content == Whitespace.linebreak => (
              Linebreak,
              acc @ [p],
            )
          | _ => (Other, acc @ [p])
          }
        },
        (Other, []),
        ps,
      );
    ps;
  };

let format_segment: Segment.t => Segment.t =
  x => x |> remove_spaces |> pad_according_to_prefs |> collapse_space_runs;

let get_trailing_spaces: Segment.t => Segment.t =
  ps =>
    ps
    |> List.rev
    |> List.fold_left(
         (acc, p) => {
           switch ((p: Piece.t)) {
           | Whitespace({content, _}) when content == Whitespace.space =>
             acc @ [p]
           | _ => acc
           }
         },
         [],
       )
    |> List.rev;

let should_back_pad: Relatives.t => bool =
  r =>
    switch (Relatives.parent(r)) {
    | None => false
    | Some(p) =>
      switch (p) {
      | Tile(t) => should_pad_inside(t)
      | _ => false
      }
    };

let should_front_pad: Relatives.t => bool =
  r =>
    switch (Relatives.parent(r)) {
    | None => false
    | Some(p) =>
      switch (p) {
      | Tile(t) => should_pad_inside(t)
      | _ => false
      }
    };

let format: Zipper.t => Zipper.t =
  ({relatives: {siblings: (l_sibs, r_sibs), _}, _} as z) => {
    let trailing_spaces = get_trailing_spaces(l_sibs);
    let l_sibs = format_segment(l_sibs);
    let l_sibs =
      should_front_pad(z.relatives) ? [Piece.space, ...l_sibs] : l_sibs;
    let l_sibs = l_sibs @ trailing_spaces;
    let r_sibs = format_segment(r_sibs);
    let r_sibs =
      should_back_pad(z.relatives) ? r_sibs @ [Piece.space] : r_sibs;
    {
      ...z,
      relatives: {
        ...z.relatives,
        siblings: (l_sibs, r_sibs),
      },
    };
  };

/*
 always: dont space-pad infix grout

 always: dont delete space immediately before caret; need it to type normally

 on format:
 regularize spaces around ops. default to the space/lack to the left
 (optional: space-pad or no-pad all ops)

 insert spaces around+between: fun, let, cond, case, rule

 then: get rid of double spaces, spaces at beginning/end of lines
 (again xcept near caret)


  */
