open Core;

type t = {
  edit_state: EditState.t,
  history: ActionHistory.t,
  font_metrics: FontMetrics.t,
  logo_font_metrics: FontMetrics.t,
  show_neighbor_tiles: bool,
};

let cutoff = (===);

let one =
  Tile.{
    id: 0,
    mold: {
      shape: Op,
      sorts: {
        out: Exp,
        in_: [],
      },
    },
    tokens: ("1", []),
  };
let plus_12 =
  Tile.{
    id: 1,
    mold: {
      shape: Bin(3),
      sorts: {
        out: Exp,
        in_: [],
      },
    },
    tokens: ("+", []),
  };
let two =
  Tile.{
    id: 2,
    mold: {
      shape: Op,
      sorts: {
        out: Exp,
        in_: [],
      },
    },
    tokens: ("2", []),
  };
// let one_plus_two: Tiles.t = Tiles.mk([one, plus_12, two]);

// let paren =
//   Tile.{
//     id: 3,
//     mold: {
//       shape: Op,
//       sorts: {
//         out: Exp,
//         in_: [Exp],
//       },
//     },
//     tokens: ("(", [(one_plus_two, ")")]),
//   };

let init = () => {
  edit_state: {
    zipper: {
      subject: {
        focus: Left,
        selection: Segment.empty,
        affixes: (Segment.empty, Segment.empty),
      },
      frame: [],
    },
    id_gen: IdGen.init,
    backpack: [],
  },
  history: ActionHistory.empty,
  font_metrics: FontMetrics.init,
  logo_font_metrics: FontMetrics.init,
  show_neighbor_tiles: false,
};

// let filler = (model: t) => {
//   switch (model.zipper) {
//   | (Pointing(_) | Selecting(_), _) => 0
//   | (Restructuring(_), _) =>
//     switch (ActionHistory.zipper_before_restructuring(model.history)) {
//     | None => 0
//     | Some(zipper) =>
//       let len_before = Layout.length(Layout.mk_zipper(zipper));
//       let len_now = Layout.length(Layout.mk_zipper(model.zipper));
//       len_now < len_before ? len_before - len_now : 0;
//     }
//   };
// };
let filler = _ => 0;
