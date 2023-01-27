open Util;

module Molds = {
  include Token.Map;
  type t = Token.Map.t(list(Mold.t));

  let union2 = union((_, ms_l, ms_r) => Some(ms_l @ ms_r));
  let union = List.fold_left(union2, empty);

  let molds_of_gram = (s: Sort.t, p: Prec.t, g: Gram.t(Sort.t)): t => {
    let rec go = (m: Mold.t, g: Gram.t(Sort.o)) =>
      switch (g) {
      | Atom(Kid(_)) => empty
      | Atom(Tok(t)) => singleton(t, [m])
      | Star(g) => go(Mold.push(Star_, m), g)
      | Alt(gs) =>
        ListUtil.elem_splits(gs)
        |> List.map(((pre, g, suf)) =>
             go(Mold.push(Alt_(pre, suf), m), g)
           )
        |> union
      | Seq(gs) =>
        ListUtil.elem_splits(gs)
        |> List.map(((pre, g, suf)) =>
             go(Mold.push(Seq_(pre, suf), m), g)
           )
        |> union
      };
    g
    |> Gram.map_atoms(
         fun
         | Tok(_) as a => a
         | Kid(s) => Kid(Some(s)),
       )
    |> go(Mold.init(Some(s), p));
  };
  let t: t =
    List.to_seq(Lang.t)
    |> Seq.concat_map(((s, prec_lvls)) =>
         List.to_seq(prec_lvls)
         |> Seq.mapi((p, lvl) => (p, lvl))
         |> Seq.concat_map(((p, (g, _))) =>
              to_seq(molds_of_gram(s, p, g))
            )
       )
    |> Token.Map.of_seq;

  let of_token = token =>
    switch (find_opt(token, t)) {
    | None => []
    | Some(ms) => ms
    };
};

module Tokens = {
  include Mold.Map;
  type t = Mold.Map.t(list(Token.Shape.t));

  let union2 = union((_, ts_l, ts_r) => Some(ts_l @ ts_r));
  let union = List.fold_left(union2, empty);

  let t =
    Molds.bindings(Molds.t)
    |> List.concat_map(((t, ms)) =>
         ms |> List.map(m => singleton(m, [t]))
       )
    |> union;

  let of_mold = m =>
    switch (find_opt(m, t)) {
    | None => []
    | Some(ts) => ts
    };
};

let keywords: list(Token.t) =
  Molds.bindings(Molds.t)
  |> List.map(fst)
  |> List.filter_map(
       fun
       | Token.Shape.Const(t) when CharUtil.is_alpha(t.[0]) => Some(t)
       | _ => None,
     );

let molds_of_token = t => {
  let shape = List.mem(t, keywords) ? Token.Shape.Const(t) : Token.shape(t);
  Molds.of_token(shape);
};

let tokens_of_mold = Tokens.of_mold;

let assoc = (s, p) =>
  switch (s) {
  | None => None
  | Some(s) => snd(List.nth(List.assoc(s, Lang.t), p))
  };
