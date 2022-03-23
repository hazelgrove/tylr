open Util;

module Make = (O: Orientation.S) => {
  [@deriving show]
  type t = Aba.t(Shard.t, Base.Segment.t);

  let init = s => (s, []);

  let extend = (s: Shard.t, seg: Base.Segment.t, (hd, tl): t) =>
    Shard.is_next(O.d, s, hd) ? Some((s, [(seg, hd), ...tl])) : None;

  let flatten = (m: t): Base.Segment.t =>
    m |> Aba.map_to_list(s => [Base.Piece.Shard(s)], Fun.id) |> List.flatten;

  let complete = ((hd, _) as m: t): option(Base.Tile.t) => {
    let label = snd(hd.label);
    let num_shards = List.length(Aba.get_a(m));
    num_shards == List.length(label)
      ? Some(
          Base.Tile.{
            label,
            mold: failwith("todo complete"),
            children:
              Aba.get_b(m) |> List.map(ListUtil.rev_if(O.d == Left)),
          },
        )
      : None;
  };
};
