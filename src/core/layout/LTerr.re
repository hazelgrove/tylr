include Terr.Base;
[@deriving (show({with_path: false}), sexp, yojson)]
type t = Terr.Base.t(Block.t);

let sort = (terr: t) => LWald.sort(terr.wald);
