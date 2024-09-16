module Dep = {
  type t = {
    depth: int,
    nullable: (bool, bool),
  };
  let mk = (~depth=0, mold: Bound.t(Mold.t)) => {
    depth,
    nullable:
      switch (mold) {
      | Root => (true, true)
      | Node(m) => Mold.(nullable(~side=L, m), nullable(m, ~side=R))
      },
  };
  let min = (l: t, r: t) => {
    depth: min(l.depth, r.depth),
    nullable: (
      fst(l.nullable) || fst(r.nullable),
      snd(l.nullable) || snd(r.nullable),
    ),
  };
  let compare = (l: t, r: t) => {
    open Stds.Compare.Syntax;
    let/ () = Int.compare(l.depth, r.depth);
    let/ () = Bool.compare(!fst(l.nullable), !fst(r.nullable));
    Bool.compare(!snd(l.nullable), !snd(r.nullable));
  };
};
module Deps = {
  include Sort.Map;
  type t = Sort.Map.t(Dep.t);
  let add = (s: Sort.t, dep: Dep.t) =>
    update(
      s,
      fun
      | None => Some(dep)
      | Some(d) => Some(Dep.min(d, dep)),
    );
  let union = Sort.Map.union((_, l, r) => Some(Dep.min(l, r)));
  let ordered = (deps: t): list(Sort.t) =>
    bindings(deps)
    |> List.sort(((_, l), (_, r)) => Dep.compare(l, r))
    |> List.map(fst);
};

let kids = (s: Sort.t): Deps.t =>
  Tile.Sym.all(s)
  |> List.filter_map(Sym.get_nt)
  // |> List.map(snd)
  |> List.fold_left(
       (deps, ((_, s), bound)) => Deps.add(s, Dep.mk(bound), deps),
       Deps.empty,
     );
let kids = Stds.Memo.general(kids);

// returns sort dependencies of s ordered by depth of dependency chain
// and nullability of the sort dependency's mold. ordered this way to prioritize
// "closer" sorts over others when molding the child of a grout token, eg
// we want the 2 in 1 >< 2 to be molded as same sort (pat vs exp) as 1
let deps = (s: Sort.t): list(Sort.t) => {
  let rec go = (~depth=0, ~deps=Deps.empty, s) => {
    let kids = kids(s);
    let deps' = Deps.union(deps, kids);
    Deps.bindings(kids)
    |> List.filter_map(((s, _)) => Deps.mem(s, deps) ? None : Some(s))
    |> List.map(go(~depth=depth + 1, ~deps=deps'))
    |> List.fold_left(Deps.union, deps');
  };
  Deps.ordered(go(s));
};
let deps = Stds.Memo.general(deps);
