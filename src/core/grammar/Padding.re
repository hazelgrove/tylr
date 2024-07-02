open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;

// specifies preferred whitespace padding around a token
[@deriving (show({with_path: false}), sexp, yojson, ord)]
type t = {
  // whether to pad token with spaces in horizontal layout
  h: (bool, bool),
  // whether to pad token with newlines in vertical layout
  // (generally (true, true) but eg (false, true) for commas)
  v: (bool, bool),
  // whether to indent contents of the following cell
  indent: bool,
};

let none = {h: (false, false), v: (false, false), indent: false};

let kw = (~l=true, ~r=true, ~indent=true, ()) => {
  h: (l, r),
  v: (l, r),
  indent,
};
let op = (~l=true, ~r=true, ~indent=true, ()) => {
  h: (l, r),
  v: (l, r),
  indent,
};
let brc = (side: Dir.t) => {
  h: (false, false),
  v: Dir.pick(side, ((false, true), (true, false))),
  indent: Dir.pick(side, (true, false)),
};
