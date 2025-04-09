open Tylr_core;

module State = State;
module Font = Font;
module History = History;

type hist = list((string, string));

type t = {
  editor: int,
  zipper: Zipper.t,
  history: History.t,
  font: Font.t,
  hist,
};

let cutoff = (==);

let init = {
  editor: 0,
  zipper: Zipper.empty,
  history: History.empty,
  font: Font.init,
  hist: [],
};

let init_from_store = _ => {
  {...init, zipper: Store.load_syntax(0)};
};
