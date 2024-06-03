module Hand = {
  [@deriving (show({with_path: false}), sexp, yojson, hash)]
  type t =
    | Focus
    | Anchor;
};

[@deriving (show({with_path: false}), sexp, yojson, hash)]
type t('path) = {
  hand: Hand.t,
  path: 'path,
};

let mk = (hand: Hand.t, path) => {hand, path};
let focus = path => mk(Focus, path);
let anchor = path => mk(Anchor, path);

let get = (f, p) => f(p.path);
let map = (f, p) => {...p, path: f(p.path)};
