include Frame.Open.Base;
[@deriving (show({with_path: false}), sexp, yojson)]
type t = Frame.Open.Base.t(Block.t);
