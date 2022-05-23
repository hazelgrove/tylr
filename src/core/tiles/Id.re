module T = {
  [@deriving show]
  type t = int;
  let compare = Int.compare;
};
include T;

module Map = Map.Make(T);

module Uf: {
  type store('a);
  let init: unit => store(_);
  let add: (t, 'a, store('a)) => unit;
  let get: (t, store('a)) => 'a;
  let merge: (('a, 'a) => 'a, t, t, store('a)) => unit;
} = {
  module M = UnionFind.Make(UnionFind.StoreMap);
  type store('a) = {
    refs: ref(Map.t(M.rref('a))),
    store: M.store('a),
  };
  let init = () => {refs: ref(Map.empty), store: M.new_store()};
  let rref = (id, s) => Map.find(id, s.refs^);
  let add = (id, a, s) =>
    switch (Map.find_opt(id, s.refs^)) {
    | None =>
      let r = M.make(s.store, a);
      s.refs := Map.add(id, r, s.refs^);
    | Some(_) => ()
    };
  let get = (id, s) => M.get(s.store, Map.find(id, s.refs^));
  let merge = (f, id, id', s) =>
    ignore(M.merge(s.store, f, rref(id, s), rref(id', s)));
};
