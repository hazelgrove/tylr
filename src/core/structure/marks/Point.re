[@deriving (show({with_path: false}), sexp, yojson, hash)]
type t('path) = {
  caret: Caret.t,
  path: 'path,
};

let mk = (caret, path) => {caret, path};
let focus = path => mk(Focus, path);
let anchor = path => mk(Anchor, path);

let get = (f, p) => f(p.path);
let map = (f, p) => {...p, path: f(p.path)};
