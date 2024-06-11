[@deriving (show({with_path: false}), sexp, yojson, hash)]
type t('range) = {
  focus: Dir.t,
  range: 'range,
};
let mk = (~focus, range) => {focus, range};
let get = (f, sel: t(_)) => f(sel.range);
let map = (f, sel: t(_)) => {...sel, range: f(sel.range)};
let put = range => map(Fun.const(range));
let carets =
    (~split_range: 'range => ('path, 'path), sel: t('range))
    : (Caret.t('path) as 'car, 'car) => {
  let (foc, anc) = Dir.order(sel.focus, split_range(sel.range));
  Caret.(focus(foc), anchor(anc)) |> Dir.order(sel.focus);
};
let map_focus = (~split_range, f, sel: t('range)) => {
  let (foc, anc) = Dir.order(sel.focus, split_range(sel.range));
  {...sel, range: Dir.order(sel.focus, (f(foc), anc))};
};
let get_focus = (~split_range, sel: t(_)) =>
  fst(Dir.order(sel.focus, split_range(sel.range)));
