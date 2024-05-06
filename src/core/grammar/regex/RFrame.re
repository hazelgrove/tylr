open Util;

[@deriving (sexp, yojson, ord)]
type t('a) =
  | Star_
  | Seq_(Regex.s('a), Regex.s('a))
  | Alt_(Regex.s('a), Regex.s('a));

let pp = (pp_a, out, f) =>
  switch (f) {
  | Star_ => Fmt.pf(out, "(•)*")
  | Seq_(ls, rs) =>
    let pp_s = Fmt.(list(~sep=sp, Regex.pp(pp_a)));
    switch (ls, rs) {
    | ([], []) => Fmt.pf(out, "(•)")
    | ([], [_, ..._]) => Fmt.pf(out, "(•@ %a)", pp_s, rs)
    | ([_, ..._], []) => Fmt.pf(out, "(%a@ •)", pp_s, ls)
    | ([_, ..._], [_, ..._]) =>
      Fmt.pf(out, "%a@ •@ %a", pp_s, ls, pp_s, rs)
    };
  | Alt_(ls, rs) =>
    let pp_s = Fmt.(list(~sep=any("@ |@ "), Regex.pp(pp_a)));
    switch (ls, rs) {
    | ([], []) => Fmt.pf(out, "(•)")
    | ([], [_, ..._]) => Fmt.pf(out, "(•@ |@ %a)", pp_s, rs)
    | ([_, ..._], []) => Fmt.pf(out, "(%a@ |@ •)", pp_s, ls)
    | ([_, ..._], [_, ..._]) =>
      Fmt.pf(out, "%a@ |@ •@ |@ %a", pp_s, ls, pp_s, rs)
    };
  };
let show = pp_a => Fmt.to_to_string(pp_a);

let star_ = Star_;
let seq_ = (ls, rs) => Seq_(ls, rs);
let alt_ = (ls, rs) => Alt_(ls, rs);

let opt_ = Alt_([Regex.eps], []);

let aseq_ = (ls, rs) =>
  seq_(List.map(Regex.atom, ls), List.map(Regex.atom, rs));

let zip = (f: t(_), r: Regex.t(_)) =>
  switch (f) {
  | Star_ => Regex.Star(r)
  | Alt_(ls, rs) => Alt(List.rev(ls) @ [r, ...rs])
  | Seq_(ls, rs) => Seq(List.rev(ls) @ [r, ...rs])
  };

let is_null = (~atom, ~side: Dir.t, f: t(_)) =>
  switch (side, f) {
  | (_, Star_ | Alt_(_)) => true
  | (L, Seq_(ls, _)) => Regex.is_null(~atom, Seq(ls))
  | (R, Seq_(_, rs)) => Regex.is_null(~atom, Seq(rs))
  };
let nullable = (~atom, ~side: Dir.t, f: t(_)) =>
  switch (side, f) {
  | (_, Star_ | Alt_(_)) => true
  | (L, Seq_(ls, _)) => Regex.nullable(~atom, Seq(ls))
  | (R, Seq_(_, rs)) => Regex.nullable(~atom, Seq(rs))
  };
