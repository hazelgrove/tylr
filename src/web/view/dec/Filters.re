open Virtual_dom.Vdom;

let all =
  Util.Nodes.svg(
    ~attrs=[Attr.id("filters")],
    [
      Silhouette.Outer.blur,
      // Silhouette.Inner.blur,
      ...List.map(Token.drop_shadow, Tylr_core.Sort.all),
    ],
  );
