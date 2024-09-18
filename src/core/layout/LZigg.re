include Zigg.Base;
[@deriving (show({with_path: false}), sexp, yojson)]
type t = Zigg.Base.t(Block.t);
