[@deriving (show({with_path: false}), sexp, yojson, hash)]
type t =
  | Focus
  | Anchor;
