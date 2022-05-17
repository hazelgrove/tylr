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
  let of_selection = (~font_metrics, ms: Measured.selection) =>
    switch (ms.content) {
    | [] => []
    | [({origin, _}, _), ..._] =>
      let length =
        ms.content
        |> List.fold_left((len, (m: Measurement.t, _)) => len + m.length, 0);
      [SelectedBoxDec.view(~font_metrics, {origin, length})];
    };

  let of_zipper = (~font_metrics, z: Zipper.t) => {
    let mz = Measured.of_zipper(z);
    List.concat([of_selection(~font_metrics, mz.selection)]);
  };
};

let view =
    (
      ~font_metrics: FontMetrics.t,
      ~just_failed as _: option(FailedInput.t)=None,
      ~show_neighbor_tiles as _: bool=false,
      z: Zipper.t,
    )
    : Node.t =>
  div(
    Attr.[id("under-the-rail"), class_("code")],
    [
      span_c("code-text", Text.of_segment(Zipper.zip(z))),
      ...Decos.of_zipper(~font_metrics, z),
    ],
  );
