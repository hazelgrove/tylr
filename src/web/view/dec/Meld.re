module Layout = Tylr_core.Layout;

module Profile = {
  type t = {
    indent: Layout.Col.t,
    tokens: list(Token.Profile.t),
    range: (Layout.Pos.t, Layout.Pos.t),
    sort: Tylr_core.Mtrl.Sorted.t,
  };
};
