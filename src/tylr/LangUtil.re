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
let shape_of_token = t =>
  List.mem(t, keywords) ? Token.Shape.Const(t) : Token.shape(t);
let molds_of_token = t => Molds.of_token(shape_of_token(t));
// todo: reimplement in terms of precedence bounds
let mold_of_token = (in_l: option(Sort.o), out: Sort.o, t: Token.t) => {
  let out_consistent =
    molds_of_token(t)
    |> List.filter((m: Mold.t) => Sort.compare_o(m.sort, out) <= 0);
  switch (out_consistent) {
  | [] => None
  | [m] => Some(m)
  | [_, _, ..._] =>
    let in_l_consistent =
      out_consistent
      |> List.filter(m =>
           switch (in_l, Mold.tip(L, m)) {
           | (None, Concave(_))
           | (Some(_), Convex) => false
           | (None, Convex) => true
           | (Some(actual), Concave(expected, _)) =>
             Sort.compare_o(actual, expected) <= 0
           }
         );
    switch (in_l_consistent) {
    | [] => None
    | [m, ..._] => Some(m) // unspecified choice
    };
  };
};

let tokens_of_mold = Tokens.of_mold;

let assoc = (s, p) => {
  open OptUtil.Syntax;
  let* s = s;
  let* (_, a) = p < 0 ? None : List.nth_opt(List.assoc(s, Lang.t), p);
  a;
};

module Edge = {
  type t = {
    l: bool,
    r: bool,
  };

  let mk = (~l=false, ~r=false, ()) => {l, r};

  let merge = (l, r) => {l: l.l || r.l, r: l.r || r.r};
};

module Edges = {
  type t = Sort.Map.t(Sort.Map.t(Edge.t));

  let of_gram = (g: Gram.t(Sort.t)): Sort.Map.t(Edge.t) => {
    let set_l = s =>
      Sort.Map.update(
        s,
        fun
        | None => Some(Edge.mk(~l=true, ()))
        | Some(Edge.{l: _, r}) => Some({l: true, r}),
      );
    let set_r = s =>
      Sort.Map.update(
        s,
        fun
        | None => Some(Edge.mk(~r=true, ()))
        | Some(Edge.{l, r: _}) => Some({l, r: true}),
      );
    Gram.kids(g)
    |> List.to_seq
    |> Seq.map(s => (s, Edge.mk()))
    |> Sort.Map.of_seq
    |> List.fold_right(
         fun
         | None
         | Some(Gram.Atom.Tok(_)) => Fun.id
         | Some(Kid(s)) => set_l(s),
         Gram.exterior(L, g),
       )
    |> List.fold_right(
         fun
         | None
         | Some(Gram.Atom.Tok(_)) => Fun.id
         | Some(Kid(s)) => set_r(s),
         Gram.exterior(R, g),
       );
  };

  let of_sort = (gs: list(Gram.t(Sort.t))): Sort.Map.t(Edge.t) =>
    gs
    |> List.map(of_gram)
    |> List.fold_left(Sort.Map.union(_ => Edge.merge), Sort.Map.empty);

  let mk = (lang: Lang.t): t =>
    lang
    |> List.to_seq
    |> Seq.map(((s, gs)) => (s, of_sort(List.map(fst, gs))))
    |> Sort.Map.of_seq;
};

let takes = (_, _) => failwith("todo LangUtil.takes");
