open Virtual_dom.Vdom;
open Node;
open Core;
open Util;

let span_c = cls => span([Attr.class_(cls)]);

let repeat_string = (n, s) => String.concat("", List.init(n, _ => s));

module CodeString = {
  let rec of_segment = (seg: Segment.t): string =>
    seg |> List.map(of_piece) |> String.concat("")
  and of_piece: Piece.t => string =
    fun
    | Tile(t) => of_tile(t)
    | Grout(_) => " "
    | Whitespace(w) => w.content == Whitespace.linebreak ? "\n" : w.content
  and of_tile = (t: Tile.t): string =>
    Aba.mk(t.shards, t.children)
    |> Aba.join(of_delim(t), of_segment)
    |> String.concat("")
  and of_delim = (t: Piece.tile, i: int): string => List.nth(t.label, i);
};

let expected_sorts = (sort: Sort.t, seg: Segment.t): list((int, Sort.t)) => {
  let t = List.nth(seg);
  let rec go = (in_sort: Sort.t, skel: Skel.t) => {
    // NOTE(andrew): disable pass_sort to highlight entire term
    /* NOTE(andrew): The Sort.Any part is a hack to prevent holes
       from letting their kids be anything e.g. 1!><2 would
       highlight the 1 but not the ! */
    let pass_sort = (n, cur_sort) =>
      cur_sort != Sort.Any
      && Sort.consistent(fst(Piece.sort(t(n))), in_sort)
        ? cur_sort : in_sort;
    let side_sorts = (n: int) => {
      let (l_sort, r_sort) = Piece.nib_sorts(t(n));
      (pass_sort(n, l_sort), pass_sort(n, r_sort));
    };
    switch (skel) {
    | Op(n) => [(n, in_sort)]
    | Pre(n, sk_r) =>
      let (_, r_sort) = side_sorts(n);
      [(n, in_sort)] @ go(r_sort, sk_r);
    | Post(sk_l, n) =>
      let (l_sort, _) = side_sorts(n);
      go(l_sort, sk_l) @ [(n, in_sort)];
    | Bin(sk_l, n, sk_r) =>
      let (l_sort, r_sort) = side_sorts(n);
      go(l_sort, sk_l) @ [(n, in_sort)] @ go(r_sort, sk_r);
    };
  };
  seg |> Segment.skel |> go(sort);
};

module Text = (M: {
                 let map: Measured.t;
                 let settings: Model.settings;
               }) => {
  let m = p => Measured.find_p(p, M.map);
  let rec of_segment =
          (~no_sorts=false, ~sort=Sort.Exp, seg: Segment.t): list(Node.t) => {
    //note: no_sorts flag is used for backback
    let expected_sorts =
      no_sorts
        ? List.init(List.length(seg), i => (i, Sort.Any))
        : expected_sorts(sort, seg);
    let sort_of_p_idx = idx =>
      switch (List.assoc_opt(idx, expected_sorts)) {
      | None => Sort.Any
      | Some(sort) => sort
      };
    seg |> List.mapi((i, p) => of_piece(sort_of_p_idx(i), p)) |> List.concat;
  }
  and of_piece = (expected_sort: Sort.t, p: Piece.t): list(Node.t) => {
    switch (p) {
    | Tile(t) => of_tile(expected_sort, t)
    | Grout(_) => [Node.text(Unicode.nbsp)]
    | Whitespace({content, _}) =>
      if (content == Whitespace.linebreak) {
        let str = M.settings.whitespace_icons ? Whitespace.linebreak : "";
        [
          span_c("whitespace", [text(str)]),
          Node.br([]),
          Node.text(repeat_string(m(p).last.col, Unicode.nbsp)),
        ];
      } else if (content == Whitespace.space) {
        let str = M.settings.whitespace_icons ? "·" : Unicode.nbsp;
        [span_c("whitespace", [text(str)])];
      } else {
        [Node.text(content)];
      }
    };
  }
  and of_tile = (expected_sort: Sort.t, t: Tile.t): list(Node.t) => {
    let children_and_sorts =
      List.mapi(
        (i, (l, child, r)) =>
          //TODO(andrew): more subtle logic about sort acceptability
          (child, l + 1 == r ? List.nth(t.mold.in_, i) : Sort.Any),
        Aba.aba_triples(Aba.mk(t.shards, t.children)),
      );
    let is_consistent = Sort.consistent(t.mold.out, expected_sort);
    Aba.mk(t.shards, children_and_sorts)
    |> Aba.join(of_delim(is_consistent, t), ((seg, sort)) =>
         of_segment(~sort, seg)
       )
    |> List.concat;
  }
  and of_delim = (is_consistent, t: Piece.tile, i: int): list(Node.t) => {
    let cls =
      List.length(t.label) == 1
        ? is_consistent ? "single" : "mono-sort-inconsistent"
        : is_consistent
            ? Tile.is_complete(t) ? "delim" : "delim-incomplete"
            : "delim-sort-inconsistent";
    [span_c(cls, [Node.text(List.nth(t.label, i))])];
  };
};

module Deco =
       (
         M: {
           let font_metrics: FontMetrics.t;
           let map: Measured.t;
           let show_backpack_targets: bool;
         },
       ) => {
  let font_metrics = M.font_metrics;

  let backpack_sel_view = ({focus: _, content}: Selection.t): t => {
    // TODO(andrew): Maybe use sort at caret instead of root
    let (_, map) = Measured.of_segment(content);
    module Text =
      Text({
        let map = map;
        let settings = Model.settings_init;
      });
    let text_view = Text.of_segment(~no_sorts=true, content);
    div(
      Attr.[
        create(
          "style",
          Printf.sprintf("padding: 0 %fpx;", font_metrics.col_width),
        ),
        classes(["code-text", "backpack-selection"]),
      ],
      // zwsp necessary so that div includes final newline
      // when it is the last character
      text_view @ [text(Unicode.zwsp)],
    );
  };

  let backpack_view =
      (~origin: Measured.point, {backpack, _} as z: Zipper.t): Node.t => {
    let length =
      switch (backpack) {
      | [] => 0
      | [hd, ..._] => Measured.segment_width(hd.content) + 2 //space-padding
      };
    let height =
      List.fold_left(
        (acc, sel: Selection.t) =>
          acc + Measured.segment_height(sel.content),
        0,
        backpack,
      );
    //TODO(andrew): truncate backpack when height is too high?
    let can_put_down =
      switch (Zipper.put_down(z)) {
      | Some(_) => true
      | None => false
      };
    let style =
      Printf.sprintf(
        "position: absolute; left: %fpx; top: %fpx;",
        Float.of_int(origin.col) *. font_metrics.col_width,
        Float.of_int(/* origin.row */ - height - 1)
        *. font_metrics.row_height
        +. CaretDec.top_text_fudge,
      );
    let selections_view =
      div(
        [Attr.create("style", style), Attr.classes(["backpack"])],
        List.map(backpack_sel_view, List.rev(backpack)),
      );
    let genie_profile =
      RestructuringGenieDec.Profile.{length, height, origin};
    let genie_view = RestructuringGenieDec.view(~font_metrics, genie_profile);
    let joiner_style =
      Printf.sprintf(
        "position: absolute; left: %fpx; top: %fpx; height: %fpx;",
        Float.of_int(origin.col) *. font_metrics.col_width,
        CaretDec.top_text_fudge -. 3.,
        Float.of_int(origin.row) *. font_metrics.row_height +. 3.,
      );
    let joiner =
      div(
        [
          Attr.create("style", joiner_style),
          Attr.classes(["backpack-joiner"]),
        ],
        [],
      );
    div(
      [
        Attr.classes(["backpack"] @ (can_put_down ? [] : ["cant-put-down"])),
      ],
      [selections_view, genie_view] @ (backpack != [] ? [joiner] : []),
    );
  };

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
      backpack_view(~origin, z),
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

  let selected_piece_profile =
      (p: Piece.t, nib_shape: Nib.Shape.t): PieceDec.Profile.t => {
    // TODO(d) fix sorts
    let mold =
      switch (p) {
      | Whitespace(_) => Mold.of_whitespace({sort: Exp, shape: nib_shape})
      | Grout(g) => Mold.of_grout(g, Exp)
      | Tile(t) => t.mold
      };
    // TODO(d) awkward
    let shards =
      switch (p) {
      | Whitespace(w) => [(0, Measured.find_w(w, M.map))]
      | Grout(g) => [(0, Measured.find_g(g, M.map))]
      | Tile(t) =>
        Measured.find_shards(t, M.map)
        |> List.filter(((i, _)) => List.mem(i, t.shards))
      };
    let l = fst(List.hd(shards));
    let r = fst(ListUtil.last(shards));
    PieceDec.Profile.{shards, mold, style: Selected(l, r)};
  };

  let root_piece_profile =
      (p: Piece.t, nib_shape: Nib.Shape.t, (l, r)): PieceDec.Profile.t => {
    // TODO(d) fix sorts
    let mold =
      switch (p) {
      | Whitespace(_) => Mold.of_whitespace({sort: Exp, shape: nib_shape})
      | Grout(g) => Mold.of_grout(g, Exp)
      | Tile(t) => t.mold
      };
    // TODO(d) awkward
    let shards =
      switch (p) {
      | Whitespace(w) => [(0, Measured.find_w(w, M.map))]
      | Grout(g) => [(0, Measured.find_g(g, M.map))]
      | Tile(t) => Measured.find_shards(t, M.map)
      };
    PieceDec.Profile.{shards, mold, style: Root(l, r)};
  };

  let selected_pieces = (z: Zipper.t): list(Node.t) =>
    // TODO(d) mold/nibs/selemdec clean up pass
    z.selection.content
    |> List.filter(
         fun
         | Piece.Whitespace(w) when w.content == Whitespace.linebreak => false
         | _ => true,
       )
    |> ListUtil.fold_left_map(
         (l: Nib.Shape.t, p: Piece.t) => {
           let profile = selected_piece_profile(p, l);
           (
             snd(Mold.nibs(profile.mold)).shape,
             PieceDec.view(~font_metrics, ~rows=M.map.rows, profile),
           );
         },
         fst(Siblings.shapes(z.relatives.siblings)),
       )
    |> snd;

  let indicated_piece_deco = (z: Zipper.t): list(Node.t) => {
    switch (Zipper.indicated_piece(z)) {
    | None => []
    | Some((p, side)) =>
      let ranges = TermRanges.mk(Zipper.zip(z));
      switch (TermRanges.find_opt(Piece.id(p), ranges)) {
      | None => []
      | Some((p_l, p_r)) =>
        let l = Measured.find_p(p_l, M.map).origin;
        let r = Measured.find_p(p_r, M.map).last;
        let nib_shape =
          switch (Zipper.caret_direction(z)) {
          | None => Nib.Shape.Convex
          | Some(nib) => Nib.Shape.relative(nib, side)
          };
        [
          PieceDec.view(
            ~font_metrics,
            ~rows=M.map.rows,
            root_piece_profile(p, nib_shape, (l, r)),
          ),
        ];
      };
    };
  };

  let rec targets = (bp: Backpack.t, seg: Segment.t) => {
    let root_targets =
      // TODO(d): review correctness wrt splits reversing prefix
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
      M.show_backpack_targets ? targets(z.backpack, seg) : [],
      selected_pieces(z),
      indicated_piece_deco(z),
    ]);
  };
};

let view =
    (
      ~font_metrics,
      ~show_backpack_targets,
      ~just_failed as _: option(FailedInput.t)=None,
      ~zipper: Zipper.t,
      ~settings: Model.settings,
    )
    : Node.t => {
  let unsel_seg = Zipper.unselect_and_zip(zipper);
  let map = snd(Measured.of_segment(unsel_seg));
  module Text =
    Text({
      let map = map;
      let settings = settings;
    });
  module Deco =
    Deco({
      let font_metrics = font_metrics;
      let map = map;
      let show_backpack_targets = show_backpack_targets;
    });
  div(
    [Attr.class_("code"), Attr.id("under-the-rail")],
    [span_c("code-text", Text.of_segment(unsel_seg))] @ Deco.all(zipper),
  );
};
