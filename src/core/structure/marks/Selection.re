open Util;

[@deriving (show({with_path: false}), sexp, yojson, hash)]
type t('range) = {
  focus: Dir.t,
  range: 'range,
};

let get = (f, sel: t(_)) => f(sel.range);
let map = (f, sel: t(_)) => {...sel, range: f(sel.range)};
