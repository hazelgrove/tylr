open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;

// specifies preferred whitespace padding around a token
[@deriving (show({with_path: false}), sexp, yojson, ord)]
type t = {
  // whether to pad token with space in horizontal layout
  // (generally (true, true) but eg (false, true) for commas)
  space: (bool, bool),
  // whether vertical layout should be preferred on either side
  break: (bool, bool),
  // whether to indent contents of the following cell in vertical layout
  indent: bool,
};

let none = {space: (false, false), break: (false, false), indent: false};

let kw = (~space=(true, true), ~break=(false, false), ~indent=true, ()) => {
  space,
  break,
  indent,
};
let op = (~space=(true, true), ~break=(false, false), ~indent=true, ()) => {
  space,
  break,
  indent,
};
let brc = (side: Dir.t) => {
  space: (false, false),
  break: (false, false),
  indent: Dir.pick(side, (true, false)),
};
