open Util;

module Make = (O: Orientation.S) => {
  module Match = Match.Make(O);

  [@deriving show]
  type t = Aba.t(Base.Segment.t, Match.t);

  let init = ([], []);

  let push_match = (m: Match.t, (hd, tl): t) =>
    switch (Match.complete(m)) {
    | None => ([], [(m, hd), ...tl])
    | Some(tile) => ([Base.Piece.Tile(tile), ...hd], tl)
    };

  let push = (p: Piece.t, (hd, tl) as stack: t) =>
    switch (p) {
    | Tile(_)
    | Grout(_) => ([p, ...hd], tl)
    | Shard(s) =>
      switch (tl) {
      | [] => push_match(Match.init(s), stack)
      | [(match, seg), ...tl] =>
        switch (Match.extend(s, hd, match)) {
        | Some(match) => push_match(match, (seg, tl))
        | None => push_match(Match.init(s), stack)
        }
      }
    };

  let flatten = (stack: t): Base.Segment.t =>
    stack |> Aba.map_to_list(Fun.id, Match.flatten) |> List.flatten;
};
