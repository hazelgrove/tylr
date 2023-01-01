module type S = {
  module Sort: Sort.S;
  type mold = Mold.t(Sort.t);
  type grammar = Grammar.t(Sort.t);

  let grammar: grammar;
  let molds: Token.t => list(mold);
  // raises Not_found if no such sort-precedence pair
  let assoc: (Sort.t, Prec.t) => Assoc.t;
};

module Make = (Sort: Sort.S, G: {let grammar: Grammar.t(Sort.t);}) => {
  module Sort = Sort;
  type mold = Mold.t(Sort.t);
  type grammar = Grammar.t(Sort.t);

  let grammar = G.grammar;

  let assoc = (s, p) => snd(List.nth(List.assoc(s, grammar), p));

  module Molds = {
    type t = Token.Map.t(list(mold));

    let union2 = Token.Map.union((t, ms_l, ms_r) => Some(ms_l @ ms_r));
    let union = List.fold_left(union2, Token.Map.empty);

    let molds_of_grex = (s: Sort.t, p: Prec.t, g: Grex.t(_)): t => {
      let rec go = (m: Mold.t(_), g: Grex.t(_)) =>
        switch (g) {
        | Atom(Kid(_)) => Token.Map.empty
        | Atom(Tok(t)) => Token.Map.singleton(t, m)
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
      go(Mold.init(s, p), g);
    };
    let molds: t =
      SMap.to_seq(g)
      |> Seq.concat_map(((s, prec_lvls)) =>
           Prec.Map.to_seq(prec_lvls)
           |> Seq.concat_map((p, (g, _)) =>
                Token.Map.to_seq(molds_of_grex(s, p, g))
              )
         )
      |> Token.Map.of_seq;

    let find = token =>
      switch (Token.Map.find_opt(token, Molds.molds)) {
      | None => []
      | Some(ms) => ms
      };
  };
  let molds = Molds.find;
};
