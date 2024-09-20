module Open = {
  include Frame.Open.Base;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Frame.Open.Base.t(Block.t);
};

module Closed = {
  include Frame.Closed.Base;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Frame.Closed.Base.t(Block.t);
};
