open Tylr_core;

// type editor_model =
//   | Simple(Zipper.t)
//   | Study(int, list(Zipper.t));

// [@deriving (show({with_path: false}), yojson)]
// type timestamp = float;

// [@deriving sexp]
// type settings = {
//   captions: bool,
//   whitespace_icons: bool,
// };

// let settings_init = {captions: false, whitespace_icons: false};

module State = State;
module Font = Font;
module History = History;

type t = {
  zipper: Zipper.t,
  history: History.t,
  font: Font.t,
  // logo_font_metrics: Font.t,
  // settings,
};

let cutoff = (==);

// let empty_zipper: Zipper.t = {
//   selection: {
//     focus: Left,
//     content: [],
//   },
//   backpack: [],
//   relatives: {
//     siblings: ([], [Grout({id: 0, shape: Convex})]),
//     ancestors: [],
//   },
//   caret: Outer,
//   caret_col_target: 0,
// };

let init_zipper =
  Zipper.{
    cur: Point(Caret.focus()),
    ctx: Ctx.unit(([], [Terr.of_tok(Token.Grout.op_(Sort.root))])),
  };

let init = {zipper: init_zipper, history: History.empty, font: Font.init};

// let get_zipper = (model: t): Zipper.t =>
//   switch (model.editor_model) {
//   | Simple(zipper) => zipper
//   | Study(n, zs) =>
//     assert(n < List.length(zs));
//     List.nth(zs, n);
//   };

// let put_zipper = (model: t, z: Zipper.t): editor_model =>
//   switch (model.editor_model) {
//   | Simple(_) => Simple(z)
//   | Study(n, zs) =>
//     assert(n < List.length(zs));
//     Study(n, Stds.Lists.put_nth(n, z, zs));
//   };

// let update_zipper = (f: Zipper.state => Zipper.state, model: t): t => {
//   let (z, id_gen) = f((get_zipper(model), model.id_gen));
//   {...model, id_gen, editor_model: put_zipper(model, z)};
// };

// let current_editor = (model: t): int =>
//   switch (model.editor_model) {
//   | Simple(_) => 0
//   | Study(n, zs) =>
//     assert(n < List.length(zs));
//     n;
//   };
