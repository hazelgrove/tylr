include Terr.Base;
[@deriving (show({with_path: false}), sexp, yojson)]
type t = Terr.Base.t(Block.t);

let sort = (terr: t) => LWald.sort(terr.wald);

let flatten = (terr: t): Block.t =>
  Block.hcat(
    LWald.flatten(~flatten=LCell.flatten, terr.wald),
    LCell.flatten(terr.cell),
  );
