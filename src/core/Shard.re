type t = {
  id: (Id.t, int),
  nibs: Nibs.t,
  tokens: list(Token.t),
};

let nibs: (~matching: IntMap.t(Nibs.t), t) => list(Nibs.t);