open Virtual_dom.Vdom;
open Node;
open Util;
open Core;

// set segments as space-padded
let pad_segment = true;

let span_c = cls => span([Attr.class_(cls)]);

module Text = {
  // Layout.text'
  let of_token = (t: Token.t): Node.t =>
    Token.is_delim(t) ? span_c("delim", [text(t)]) : text(t);
  let of_shard = (shard: Shard.t): Node.t =>
    span_c("extra-bold-delim", [text(Shard.Label.token(shard.label))]);
  let of_grout = _ => text(Unicode.nbsp);

  let rec of_segment = (seg: Segment.t): list(Node.t) => {
    let spaces =
      List.init(List.length(seg) + 1, _ => Node.text(Unicode.nbsp));
    seg
    |> List.map(Piece.get(of_grout, of_shard, of_tile))
    |> (pad_segment ? ListUtil.interleave(spaces) : Fun.id);
  }
  and of_tile = (t: Tile.t): Node.t =>
    span(
      [],
      List.concat(
        ListUtil.map_alt(
          t => [of_token(t)],
          of_segment,
          t.label,
          t.children,
        ),
      ),
    );
};

module Decos = {
  // let of_selection = (~font_metrics, ms: Measured.selection) =>
  //   switch (ms.content) {
  //   | [] => []
  //   | [({origin, _}, _), ..._] =>
  //     let length =
  //       ms.content
  //       |> List.fold_left((len, (m: Measurement.t, _)) => len + m.length, 0);
  //     [SelectedBoxDec.view(~font_metrics, {origin, length})];
  //   };
  // let targets = (~font_metrics, z: Zipper.t) => {
  //   let rec go_segment =
  //           (meta: list(Shard.t), seg: Measured.segment): list(Node.t) =>
  //     if (List.for_all(
  //           (s: Shard.t) =>
  //             List.exists(
  //               (s': Shard.t) => s.tile_id == s'.tile_id,
  //               Measured.shards(seg),
  //             ),
  //           meta,
  //         )) {
  //       ListUtil.splits(seg)
  //       |> List.map(((l, r)) => {
  //            let shards_l = Measured.shards(l);
  //            let shards_r = Measured.shards(r);
  //            let valid =
  //              meta
  //              |> List.for_all((s: Shard.t) => {
  //                   let after_l =
  //                     shards_l
  //                     |> List.for_all(s' => Shard.index(s') < Shard.index(s));
  //                   let before_r =
  //                     shards_r
  //                     |> List.for_all(s' => Shard.index(s') > Shard.index(s));
  //                   after_l && before_r;
  //                 });
  //            if (valid) {
  //              let measurement =
  //                switch (l, r) {
  //                | ([], []) => failwith("impossible")
  //                | (_, [(m, _), ..._]) =>
  //                  Layout.{origin: m.origin - 1, length: 1}
  //                | ([(m, _), ..._], _) =>
  //                  Layout.{origin: m.origin + m.length, length: 1}
  //                };
  //              let profile =
  //                CaretPosDec.Profile.{
  //                  style: `Sibling,
  //                  measurement,
  //                  color: Color.Exp,
  //                  just_failed: None,
  //                };
  //              [CaretPosDec.view(~font_metrics, profile)];
  //            } else {
  //              [];
  //            };
  //          })
  //       |> List.concat;
  //     } else {
  //       seg
  //       |> List.filter_map(
  //            fun
  //            | (_, Measured.Tile(t)) => Some(t)
  //            | _ => None,
  //          )
  //       |> List.map((t: Measured.tile) =>
  //            List.concat(List.map(go_segment(meta), t.children))
  //          )
  //       |> List.concat;
  //     };
  //   switch (Backpack.pop(z.backpack)) {
  //   | Some((([_, ..._] as meta, _), _)) =>
  //     let seg = Zipper.zip(z);
  //     go_segment(meta, snd(Measured.of_segment(seg)));
  //   | _ => []
  //   };
  // };
  // let of_zipper = (~font_metrics, z: Zipper.t) => {
  //   let mz = Measured.of_zipper(z);
  //   List.concat([of_selection(~font_metrics, mz.selection)]);
  // };
};

let view =
    (
      ~font_metrics as _: FontMetrics.t,
      ~just_failed as _: option(FailedInput.t)=None,
      ~show_neighbor_tiles as _: bool=false,
      z: Zipper.t,
    )
    : Node.t =>
  div(
    Attr.[id("under-the-rail"), class_("code")],
    [
      span_c("code-text", Text.of_segment(Zipper.zip(z))),
      // ...Decos.of_zipper(~font_metrics, z),
    ],
  );
