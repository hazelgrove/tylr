module Open = {
  include Frame.Open.Base;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Frame.Open.Base.t(Block.t);
  let snoc = (~side: Dir.t, terr: LTerr.t, (dn, up): t) =>
    switch (side) {
    | L => (dn @ [terr], up)
    | R => (dn, up @ [terr])
    };
};

module Closed = {
  include Frame.Closed.Base;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Frame.Closed.Base.t(Block.t);
};
