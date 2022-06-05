open Virtual_dom.Vdom;
open Node;
open Core;
open Util;

let span_c = cls => span([Attr.class_(cls)]);

module Text = {
  let rec of_segment = (~indent=0, seg: Segment.t): list(Node.t) =>
    seg
    |> List.map(
         fun
         | Piece.Whitespace({content: c, _}) when c == Whitespace.linebreak => [
             span_c("whitespace", [text(Whitespace.linebreak)]),
             Node.br([]),
             Node.text(
               String.concat("", List.init(indent, _ => Unicode.nbsp)),
             ),
           ]
         | Piece.Whitespace({content: c, _}) when c == Whitespace.space => [
             span_c("whitespace", [text("·")]),
           ]
         | Piece.Whitespace(w) => [Node.text(w.content)]
         | Grout(_) => [Node.text(Unicode.nbsp)]
         | Tile(t) => of_tile(t, ~indent),
       )
    |> List.concat
  and of_tile = (t: Tile.t, ~indent): list(Node.t) => {
    let span =
      List.length(t.label) == 1
        ? Node.span([])
        : span_c(Tile.is_complete(t) ? "delim" : "extra-bold-delim");
    Aba.mk(t.shards, t.children)
    |> Aba.join(
         i => [span([Node.text(List.nth(t.label, i))])],
         of_segment(~indent=indent + 1),
       )
    |> List.concat;
  };
};

let backpack_sel_view = ({focus: _, content}: Selection.t): t => {
  // TODO(andrew): Maybe use sort at caret instead of root
  let text_view = Text.of_segment(content);
  div([Attr.classes(["code-text", "backpack-selection"])], text_view);
};

let selection_length = (sel: Selection.t): int => {
  let seg = sel.content;
  let map = snd(Measured.of_segment(seg));
  switch (ListUtil.hd_opt(seg), ListUtil.last_opt(seg)) {
  | (None, _)
  | (_, None) => 0
  | (Some(first), Some(last)) =>
    let first = Measured.find_p(first, map);
    let last = Measured.find_p(last, map);
    last.last.col - first.origin.col;
  };
};

let backpack_view =
    (
      ~font_metrics: FontMetrics.t,
      ~origin: Measured.point,
      backpack: Backpack.t,
    )
    : Node.t => {
  let length =
    backpack |> List.map(selection_length) |> List.fold_left(max, 0);
  let height = List.length(backpack);
  let style =
    Printf.sprintf(
      "position: absolute; left: %fpx; top: %fpx;",
      Float.of_int(origin.col) *. font_metrics.col_width,
      Float.of_int(origin.row - height) *. font_metrics.row_height,
    );
  let selections_view =
    div(
      [Attr.create("style", style), Attr.classes(["backpack"])],
      List.map(backpack_sel_view, List.rev(backpack)),
    );

  let genie_profile = RestructuringGenieDec.Profile.{length, height, origin};
  let genie_view = RestructuringGenieDec.view(~font_metrics, genie_profile);
  div([Attr.classes(["backpack"])], [selections_view, genie_view]);
};

module Deco = (M: {
                 let font_metrics: FontMetrics.t;
                 let map: Measured.t;
               }) => {
  let font_metrics = M.font_metrics;

  let rec holes = (seg: Segment.t): list(Node.t) =>
    seg
    |> List.map(
         fun
         | Piece.Whitespace(_) => []
         | Tile(t) => t.children |> List.map(holes) |> List.concat
         | Grout(g) => [
             EmptyHoleDec.view(
               ~font_metrics, // TODO(d) fix sort
               {
                 measurement: Measured.find_g(g, M.map),
                 mold: Mold.of_grout(g, Exp),
               },
             ),
           ],
       )
    |> List.concat;

  let caret = (z: Zipper.t): list(Node.t) => {
    let origin = Zipper.caret_point(M.map, z);
    let side =
      switch (Zipper.indicated_piece(z)) {
      | Some((_, side)) => side
      | _ => Right
      };
    [
      CaretDec.simple_view(
        ~font_metrics,
        ~side,
        ~origin,
        ~shape=Zipper.caret_direction(z),
      ),
      backpack_view(~font_metrics, ~origin, z.backpack),
    ];
  };

  let children = (p: Piece.t): list(Measured.measurement_lin) =>
    switch (p) {
    | Whitespace(_)
    | Grout(_) => []
    | Tile(t) =>
      let m = Measured.find_t(t, M.map);
      let token = List.nth(t.label);
      Aba.mk(t.shards, t.children)
      |> Aba.fold_left(
           shard => (m.origin.col + Unicode.length(token(shard)), []),
           (
             (origin, children: list(Measured.measurement_lin)),
             child,
             shard,
           ) => {
             let length = Measured.length(child, M.map);
             (
               origin + length + Unicode.length(token(shard)),
               children @ [{origin, length}],
             );
           },
         )
      |> snd;
    };

  let piece_profile =
      (p: Piece.t, nib_shape: Nib.Shape.t, style: SelemStyle.t)
      : SelemDec.Profile.t => {
    // TODO(d) fix sorts
    let (sort: Sort.t, nibs) =
      switch (p) {
      | Whitespace(_) => (
          Exp,
          Mold.of_whitespace({sort: Exp, shape: nib_shape}).nibs,
        )
      | Grout(g) => (Exp, Mold.of_grout(g, Exp).nibs)
      | Tile(t) => (t.mold.out, Tile.nibs(t))
      };
    let m = Measured.find_p(p, M.map);
    SelemDec.Profile.{
      color: Color.of_sort(sort),
      shape: SelemDec.piece_shape_of_nibs(nibs),
      measurement: m,
      style,
      closed_children: [],
      open_children:
        p |> children |> Measured.relativize_measurements(m.origin.col),
    };
  };

  let selected_pieces = (z: Zipper.t): list(Node.t) =>
    // TODO(d) mold/nibs/selemdec clean up pass
    z.selection.content
    |> ListUtil.fold_left_map(
         (l: Nib.Shape.t, p: Piece.t) => {
           let profile = piece_profile(p, l, Selected);
           (
             fst(snd(profile.shape)).shape,
             SelemDec.view(~font_metrics, profile),
           );
         },
         fst(Siblings.shapes(z.relatives.siblings)),
       )
    |> snd;

  let indicated_piece_deco = (z: Zipper.t): list(Node.t) => {
    switch (Zipper.indicated_piece(z)) {
    | None => []
    | Some((p, side)) =>
      let nib_shape =
        switch (Zipper.caret_direction(z)) {
        | None => Nib.Shape.Convex
        | Some(nib) => Nib.Shape.relative(nib, side)
        };
      [SelemDec.view(~font_metrics, piece_profile(p, nib_shape, Root))];
    };
  };

  let rec targets = (bp: Backpack.t, seg: Segment.t) => {
    let root_targets =
      ListUtil.splits(seg)
      |> List.map(((l, r)) => {
           let sibs = Segment.(incomplete_tiles(l), incomplete_tiles(r));
           switch (Backpack.pop(sibs, bp)) {
           | None
           | Some((true, _, _)) => []
           | Some(_) =>
             let measurement =
               switch (Siblings.neighbors((l, r))) {
               | (None, None) => failwith("impossible")
               | (_, Some(p))
               | (Some(p), _) => Measured.find_p(p, M.map)
               };
             let profile =
               CaretPosDec.Profile.{
                 style: `Sibling,
                 measurement,
                 color: Color.Exp,
                 just_failed: None,
               };
             [CaretPosDec.view(~font_metrics, profile)];
           };
         })
      |> List.concat;
    switch (root_targets) {
    | [_, ..._] => root_targets
    | [] =>
      seg
      |> List.filter_map(
           fun
           | Piece.Tile(t) => Some(t)
           | _ => None,
         )
      |> List.map((t: Tile.t) =>
           List.concat(List.map(targets(bp), t.children))
         )
      |> List.concat
    };
  };

  let all = (z: Zipper.t) => {
    let seg = Zipper.zip(z);
    List.concat([
      holes(seg),
      caret(z),
      targets(z.backpack, seg),
      selected_pieces(z),
      indicated_piece_deco(z),
    ]);
  };
};

let view =
    (
      ~font_metrics,
      ~just_failed as _: option(FailedInput.t)=None,
      ~show_neighbor_tiles as _: bool=false,
      ~zipper: Zipper.t,
    )
    : Node.t => {
  let seg = Zipper.zip(zipper);

  module Deco =
    Deco({
      let font_metrics = font_metrics;
      let map = snd(Measured.of_segment(seg));
    });
  div(
    [Attr.class_("code"), Attr.id("under-the-rail")],
    [span_c("code-text", Text.of_segment(seg))] @ Deco.all(zipper),
  );
};
