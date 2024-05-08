open Virtual_dom.Vdom;
open Node;
open Tylr_core;
open Util;

// let span_c = cls => span([Attr.class_(cls)]);

// let expected_sorts = (sort: Sort.t, seg: Segment.t): list((int, Sort.t)) => {
//   let t = List.nth(seg);
//   let rec go = (in_sort: Sort.t, skel: Skel.t) => {
//     // NOTE(andrew): disable pass_sort to highlight entire term
//     /* NOTE(andrew): The Sort.Any part is a hack to prevent holes
//        from letting their kids be anything e.g. 1!><2 would
//        highlight the 1 but not the ! */
//     let pass_sort = (n, cur_sort) =>
//       cur_sort != Sort.Any
//       && Sort.consistent(fst(Piece.sort(t(n))), in_sort)
//         ? cur_sort : in_sort;
//     let side_sorts = (n: int) => {
//       let (l_sort, r_sort) = Piece.nib_sorts(t(n));
//       (pass_sort(n, l_sort), pass_sort(n, r_sort));
//     };
//     switch (skel) {
//     | Op(n) => [(n, in_sort)]
//     | Pre(n, sk_r) =>
//       let (_, r_sort) = side_sorts(n);
//       [(n, in_sort)] @ go(r_sort, sk_r);
//     | Post(sk_l, n) =>
//       let (l_sort, _) = side_sorts(n);
//       go(l_sort, sk_l) @ [(n, in_sort)];
//     | Bin(sk_l, n, sk_r) =>
//       let (l_sort, r_sort) = side_sorts(n);
//       go(l_sort, sk_l) @ [(n, in_sort)] @ go(r_sort, sk_r);
//     };
//   };
//   seg |> Segment.skel |> go(sort);
// };

// module Text = (M: {
//                  let map: Measured.t;
//                  let settings: Model.settings;
//                }) => {
//   let m = p => Measured.find_p(p, M.map);
//   let rec of_segment =
//           (~no_sorts=false, ~sort=Sort.root, seg: Segment.t): list(Node.t) => {
//     //note: no_sorts flag is used for backback
//     let expected_sorts =
//       no_sorts
//         ? List.init(List.length(seg), i => (i, Sort.Any))
//         : expected_sorts(sort, seg);
//     let sort_of_p_idx = idx =>
//       switch (List.assoc_opt(idx, expected_sorts)) {
//       | None => Sort.Any
//       | Some(sort) => sort
//       };
//     seg |> List.mapi((i, p) => of_piece(sort_of_p_idx(i), p)) |> List.concat;
//   }
//   and of_piece = (expected_sort: Sort.t, p: Piece.t): list(Node.t) => {
//     switch (p) {
//     | Tile(t) => of_tile(expected_sort, t)
//     | Grout(_) => [Node.text(Unicode.nbsp)]
//     | Whitespace({content, _}) =>
//       if (content == Whitespace.linebreak) {
//         let str = M.settings.whitespace_icons ? Whitespace.linebreak : "";
//         [
//           span_c("linebreak", [text(str)]),
//           Node.br([]),
//           Node.text(StringUtil.repeat(m(p).last.col, Unicode.nbsp)),
//         ];
//       } else if (content == Whitespace.space) {
//         let str = M.settings.whitespace_icons ? "·" : Unicode.nbsp;
//         [span_c("whitespace", [text(str)])];
//       } else {
//         [Node.text(content)];
//       }
//     };
//   }
//   and of_tile = (expected_sort: Sort.t, t: Tile.t): list(Node.t) => {
//     let children_and_sorts =
//       List.mapi(
//         (i, (l, child, r)) =>
//           //TODO(andrew): more subtle logic about sort acceptability
//           (child, l + 1 == r ? List.nth(t.mold.in_, i) : Sort.Any),
//         Aba.aba_triples(Aba.mk(t.shards, t.children)),
//       );
//     let is_consistent = Sort.consistent(t.mold.out, expected_sort);
//     Aba.mk(t.shards, children_and_sorts)
//     |> Aba.join(of_delim(t.mold.out, is_consistent, t), ((seg, sort)) =>
//          of_segment(~sort, seg)
//        )
//     |> List.concat;
//   }
//   and of_delim =
//       (sort: Sort.t, is_consistent, t: Piece.tile, i: int): list(Node.t) => {
//     let cls =
//       List.length(t.label) == 1
//         ? is_consistent ? "single" : "mono-sort-inconsistent"
//         : is_consistent
//             ? Tile.is_complete(t) ? "delim" : "delim-incomplete"
//             : "delim-sort-inconsistent";
//     [
//       span(
//         [Attr.classes([cls, "text-" ++ Sort.to_string(sort)])],
//         [Node.text(List.nth(t.label, i))],
//       ),
//     ];
//   };
// };

// module Deco =
//        (
//          M: {
//            let font_metrics: FontMetrics.t;
//            let map: Measured.t;
//            let show_backpack_targets: bool;
//          },
//        ) => {
//   let font_metrics = M.font_metrics;

//   let backpack_sel_view =
//       (
//         x_off: float,
//         y_off: float,
//         scale: float,
//         opacity: float,
//         {focus: _, content}: Selection.t,
//       ) => {
//     module Text =
//       Text({
//         let map = Measured.of_segment(content);
//         let settings = Model.settings_init;
//       });
//     // TODO(andrew): Maybe use init sort at caret to prime this
//     div(
//       [
//         Attr.classes(["code-text", "backpack-selection"]),
//         Attr.create(
//           "style",
//           Printf.sprintf(
//             "position: absolute; transform-origin: bottom left; transform: translate(%fpx, %fpx) scale(%f); opacity: %f%%;",
//             x_off,
//             y_off,
//             scale,
//             opacity,
//           ),
//         ),
//       ],
//       // zwsp necessary for containing box to stretch to contain trailing newline
//       Text.of_segment(~no_sorts=true, content) @ [text(Unicode.zwsp)],
//     );
//   };

//   let backpack_view =
//       (~origin: Measured.point, {backpack, _} as z: Zipper.t): Node.t => {
//     //TODO(andrew): clean up this dumpster fire of a function
//     let height_head =
//       switch (backpack) {
//       | [] => 0
//       | [hd, ..._] => Measured.segment_height(hd.content)
//       };
//     let can_put_down =
//       switch (Zipper.pop_backpack(z)) {
//       // caret thing is hack; i don't know why pop_backpack
//       // gives us what we want here
//       | Some(_) => z.caret == Outer
//       | None => false
//       };
//     let caret_adj = {
//       let shape = Zipper.caret_direction(z);
//       let side =
//         switch (Zipper.indicated_piece(z)) {
//         | Some((_, side, _)) => side
//         | _ => Right
//         };
//       DecUtil.caret_adjust(side, shape);
//     };
//     let caret_adj_px =
//       //TODO(andrew): figure out why we need this mystery pixel below
//       (-1.) +. caret_adj *. font_metrics.col_width;
//     let style =
//       Printf.sprintf(
//         "position: absolute; left: %fpx; top: %fpx;",
//         Float.of_int(origin.col) *. font_metrics.col_width +. caret_adj_px,
//         Float.of_int(/* origin.row */ - height_head - 1)
//         *. font_metrics.row_height,
//       );
//     let scale_fn = idx => float_of_int(100 - 12 * idx) /. 100.;
//     let x_fn = idx => float_of_int(12 * idx);
//     let init_opacity = 100.;
//     let opacity_reduction = 20.; // reduction per line
//     let init_idx = 0;
//     let dy_fn = (idx, base_height) =>
//       font_metrics.row_height
//       *. float_of_int(base_height)
//       *. scale_fn(idx)
//       -. 4.;
//     let init_y_offset = dy_fn(init_idx, height_head);
//     let (_, _, _, selections) =
//       List.fold_left(
//         ((idx, y_offset, opacity, vs), s: Selection.t) => {
//           let base_height = Measured.segment_height(s.content);
//           let scale = scale_fn(idx);
//           let x_offset = x_fn(idx);
//           let new_y_offset = y_offset -. dy_fn(idx, base_height);
//           let v =
//             backpack_sel_view(x_offset, new_y_offset, scale, opacity, s);
//           let new_idx = idx + 1;
//           let new_opacity = opacity -. opacity_reduction;
//           //TODO(andrew): am i making this difficult by going backwards?
//           (new_idx, new_y_offset, new_opacity, List.cons(v, vs));
//         },
//         (init_idx, init_y_offset, init_opacity, []),
//         backpack,
//       );
//     let selections_view =
//       div(
//         [Attr.create("style", style), Attr.classes(["backpack"])],
//         selections,
//       );
//     let length =
//       switch (backpack) {
//       | [] => 0
//       | [hd, ..._] => Measured.segment_width(hd.content)
//       };
//     let joiner_style =
//       Printf.sprintf(
//         "position: absolute; left: %fpx; top: %fpx; height: %fpx;",
//         Float.of_int(origin.col) *. font_metrics.col_width +. caret_adj_px,
//         -3.,
//         Float.of_int(origin.row) *. font_metrics.row_height +. 3.,
//       );
//     let joiner =
//       div(
//         [
//           Attr.create("style", joiner_style),
//           Attr.classes(["backpack-joiner"]),
//         ],
//         [],
//       );
//     //TODO(andrew): break out backpack decoration into its own module
//     let genie_view =
//       DecUtil.code_svg(
//         ~font_metrics,
//         ~origin={row: 0, col: 0},
//         ~base_cls=["restructuring-genie"],
//         ~path_cls=["restructuring-genie-path"],
//         SvgUtil.Path.[
//           M({x: 0., y: 0.}),
//           V({y: (-1.0)}),
//           H_({dx: Float.of_int(length)}),
//           V_({dy: 0.0}),
//           Z,
//         ],
//       );
//     let genie_style =
//       Printf.sprintf(
//         "position: absolute; left: %fpx;",
//         Float.of_int(origin.col) *. font_metrics.col_width +. caret_adj_px,
//       );
//     div(
//       [
//         Attr.classes(["backpack"] @ (can_put_down ? [] : ["cant-put-down"])),
//       ],
//       [
//         selections_view,
//         div([Attr.create("style", genie_style)], [genie_view]),
//       ]
//       @ (backpack != [] ? [joiner] : []),
//     );
//   };

//   let rec holes = (seg: Segment.t): list(Node.t) =>
//     seg
//     |> List.map(
//          fun
//          | Piece.Whitespace(_) => []
//          | Tile(t) => t.children |> List.map(holes) |> List.concat
//          | Grout(g) => [
//              EmptyHoleDec.view(
//                ~font_metrics, // TODO(d) fix sort
//                {
//                  measurement: Measured.find_g(g, M.map),
//                  mold: Mold.of_grout(g, Any),
//                },
//              ),
//            ],
//        )
//     |> List.concat;

//   let caret = (z: Zipper.t): list(Node.t) => {
//     let origin = Zipper.caret_point(M.map, z);
//     let shape = Zipper.caret_direction(z);
//     let side =
//       switch (Zipper.indicated_piece(z)) {
//       | Some((_, side, _)) => side
//       | _ => Right
//       };
//     [
//       CaretDec.view(~font_metrics, ~profile={side, origin, shape}),
//       backpack_view(~origin, z),
//     ];
//   };

//   let children = (p: Piece.t): list(Measured.measurement_lin) =>
//     switch (p) {
//     | Whitespace(_)
//     | Grout(_) => []
//     | Tile(t) =>
//       let m = Measured.find_t(t, M.map);
//       let token = List.nth(t.label);
//       Aba.mk(t.shards, t.children)
//       |> Aba.fold_left(
//            shard => (m.origin.col + Unicode.length(token(shard)), []),
//            (
//              (origin, children: list(Measured.measurement_lin)),
//              child,
//              shard,
//            ) => {
//              let length = Measured.length(child, M.map);
//              (
//                origin + length + Unicode.length(token(shard)),
//                children @ [{origin, length}],
//              );
//            },
//          )
//       |> snd;
//     };

//   let selected_piece_profile =
//       (p: Piece.t, nib_shape: Nib.Shape.t): PieceDec.Profile.t => {
//     // TODO(d) fix sorts
//     let mold =
//       switch (p) {
//       | Whitespace(_) => Mold.of_whitespace({sort: Any, shape: nib_shape})
//       | Grout(g) => Mold.of_grout(g, Any)
//       | Tile(t) => t.mold
//       };
//     // TODO(d) awkward
//     let shards =
//       switch (p) {
//       | Whitespace(w) => [(0, Measured.find_w(w, M.map))]
//       | Grout(g) => [(0, Measured.find_g(g, M.map))]
//       | Tile(t) =>
//         Measured.find_shards(t, M.map)
//         |> List.filter(((i, _)) => List.mem(i, t.shards))
//       };
//     let l = fst(List.hd(shards));
//     let r = fst(ListUtil.ft_exn(shards));
//     PieceDec.Profile.{shards, mold, style: Selected(l, r), index: 0};
//   };

//   let root_piece_profile =
//       (index: int, p: Piece.t, nib_shape: Nib.Shape.t, (l, r))
//       : PieceDec.Profile.t => {
//     // TODO(d) fix sorts
//     let mold =
//       switch (p) {
//       | Whitespace(_) => Mold.of_whitespace({sort: Any, shape: nib_shape})
//       | Grout(g) => Mold.of_grout(g, Any)
//       | Tile(t) => t.mold
//       };
//     // TODO(d) awkward
//     let shards =
//       switch (p) {
//       | Whitespace(w) => [(0, Measured.find_w(w, M.map))]
//       | Grout(g) => [(0, Measured.find_g(g, M.map))]
//       | Tile(t) => Measured.find_shards(t, M.map)
//       };
//     PieceDec.Profile.{shards, mold, style: Root(l, r), index};
//   };

//   let selected_pieces = (z: Zipper.t): list(Node.t) =>
//     // TODO(d) mold/nibs/selemdec clean up pass
//     z.selection.content
//     |> List.filter(
//          fun
//          | Piece.Whitespace(w) when w.content == Whitespace.linebreak => false
//          | _ => true,
//        )
//     |> ListUtil.fold_left_map(
//          (l: Nib.Shape.t, p: Piece.t) => {
//            let profile = selected_piece_profile(p, l);
//            // TODO(andrew): do something different for the caret
//            // adjacent piece so it lines up nice
//            (
//              snd(Mold.nibs(profile.mold)).shape,
//              PieceDec.view(~font_metrics, ~rows=M.map.rows, profile),
//            );
//          },
//          fst(Siblings.shapes(z.relatives.siblings)),
//        )
//     |> snd
//     |> List.flatten;

//   let indicated_piece_deco = (z: Zipper.t): list(Node.t) => {
//     switch (Zipper.indicated_piece(z)) {
//     | _ when z.selection.content != [] => []
//     | None => []
//     | Some((Grout(_), _, _)) => []
//     | Some((p, side, _)) =>
//       let nib_shape =
//         switch (Zipper.caret_direction(z)) {
//         | None => Nib.Shape.Convex
//         | Some(nib) => Nib.Shape.relative(nib, side)
//         };
//       let range: option((Measured.point, Measured.point)) =
//         if (Piece.has_ends(p)) {
//           let ranges = TermRanges.mk(Zipper.zip(z));
//           switch (TermRanges.find_opt(Piece.id(p), ranges)) {
//           | None => None
//           | Some((p_l, p_r)) =>
//             let l = Measured.find_p(p_l, M.map).origin;
//             let r = Measured.find_p(p_r, M.map).last;
//             Some((l, r));
//           };
//         } else {
//           // using range of piece itself hides unidelimited child borders
//           let m = Measured.find_p(p, M.map);
//           Some((m.origin, m.last));
//         };
//       let index =
//         switch (Zipper.indicated_shard_index(z)) {
//         | None => (-1)
//         | Some(i) => i
//         };
//       //TODO(andrew): get this working
//       let _segs =
//         switch (p) {
//         | Tile({children, mold, _}) =>
//           children
//           |> List.flatten
//           |> List.filter(
//                fun
//                | Piece.Whitespace(w) when w.content == Whitespace.linebreak =>
//                  false
//                | _ => true,
//              )
//           |> List.map(p => (mold, Measured.find_p(p, M.map)))
//         | _ => []
//         };
//       switch (range) {
//       | None => []
//       | Some(range) =>
//         PieceDec.view(
//           ~font_metrics,
//           ~rows=M.map.rows,
//           ~segs=[],
//           root_piece_profile(index, p, nib_shape, range),
//         )
//       };
//     };
//   };

//   let rec targets = (~container_shards=?, bp: Backpack.t, seg: Segment.t) => {
//     let with_container_shards = ((pre, suf) as sibs) =>
//       switch (container_shards) {
//       | None => sibs
//       | Some((l, r)) => ([l, ...pre], suf @ [r])
//       };
//     let root_targets =
//       ListUtil.splits(seg)
//       |> List.map(((l, r)) => {
//            let sibs =
//              Segment.(incomplete_tiles(l), incomplete_tiles(r))
//              |> with_container_shards;
//            switch (Backpack.pop(sibs, bp)) {
//            | None
//            | Some((true, _, _)) => []
//            | Some(_) =>
//              let measurement =
//                switch (Siblings.neighbors((l, r))) {
//                | (None, None) => failwith("impossible")
//                | (_, Some(p)) =>
//                  let m = Measured.find_p(p, M.map);
//                  Measured.{origin: m.origin, last: m.origin};
//                | (Some(p), _) =>
//                  let m = Measured.find_p(p, M.map);
//                  Measured.{origin: m.last, last: m.last};
//                };
//              let profile =
//                CaretPosDec.Profile.{style: `Sibling, measurement, sort: Exp};
//              [CaretPosDec.view(~font_metrics, profile)];
//            };
//          })
//       |> List.concat;
//     switch (root_targets) {
//     | [_, ..._] => root_targets
//     | [] =>
//       seg
//       |> List.filter_map(
//            fun
//            | Piece.Tile(t) => Some(t)
//            | _ => None,
//          )
//       |> List.map((t: Tile.t) => {
//            // TODO(d): unify with Relatives.local_incomplete_tiles
//            Tile.contained_children(t)
//            |> List.map(((l, seg, r)) =>
//                 targets(~container_shards=(l, r), bp, seg)
//               )
//            |> List.concat
//          })
//       |> List.concat
//     };
//   };

//   let all = (z: Zipper.t) => {
//     let seg = Zipper.zip(z);
//     List.concat([
//       holes(seg),
//       caret(z),
//       selected_pieces(z),
//       indicated_piece_deco(z),
//       M.show_backpack_targets && Backpack.restricted(z.backpack)
//         ? targets(z.backpack, seg) : [],
//     ]);
//   };
// };

let view_str =
  Core.Memo.general(
    fun
    | "" => []
    | str => Node.[span([text(str)])],
  );

let view_indent =
  Core.Memo.general(n => view_str(StringUtil.repeat(n, Unicode.nbsp)));

let view_text =
  Layout.fold(
    [],
    (ctx, _pos, tok: Token.t) =>
      switch (tok.mtrl) {
      | Space =>
        let (hd, tl) =
          ListUtil.Framed.hd_exn(StringUtil.split(~on='\n', tok.text));
        tl
        |> List.mapi((i, line) => (i, line))
        |> List.concat_map(((i, line)) =>
             view_indent(
               i < List.length(tl) - 1 ? Layout.Ictx.middle(ctx) : ctx.right,
             )
             @ view_str(line)
           )
        |> (@)(view_str(hd));
      | Grout => view_str("•")
      | Tile(_) => view_str(tok.text)
      },
    txts => Node.[span(List.concat(Chain.to_list(Fun.id, Fun.id, txts)))],
  );

let view = (~font_metrics as _, ~zipper: Zipper.t): Node.t => {
  let c = Zipper.zip(zipper);
  // module Text =
  //   Text({
  //     let map = map;
  //     let settings = settings;
  //   });
  // module Deco =
  //   Deco({
  //     let font_metrics = font_metrics;
  //     let map = map;
  //     let show_backpack_targets = show_backpack_targets;
  //   });
  div(
    ~attrs=[Attr.class_("code"), Attr.id("under-the-rail")],
    Node.[span(~attrs=[Attr.class_("code-text")], view_text(c))],
    // @ Deco.all(zipper),
  );
};
