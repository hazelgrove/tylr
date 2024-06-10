open Virtual_dom.Vdom.Attr;

let style = (kvs: list((string, string))) =>
  create(
    "style",
    kvs |> List.map(((k, v)) => k ++ ":" ++ v ++ ";") |> String.concat(""),
  );
