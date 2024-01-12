module Step = Molded.Label;
module Stride = Molded.Bounds;
module Walk = {
  type t = Chain.t(Stride.t, Step.t);
};
module End = {
  include Bound;
  type t = Bound.t(Step.t);
  module Map =
    Map.Make({
      type nonrec t = t;
      let compare = compare;
    });
};

module Index = {
  include End.Map;
  type t = End.Map.t(list(Walk.t));
};

let expect_lbl =
  fun
  | Sym.NT(_) => raise(MGrammar.Non_alternating_form)
  | T(mlbl) => mlbl;
let expect_srt =
  fun
  | Sym.T(_) => raise(MGrammar.Non_alternating_form)
  | NT(msrt) => msrt;

let enter =
    (~from: Dir.t, ~l=Bound.Root, ~r=Bound.Root, s: Mtrl.Sort.t)
    : list(Molded.Sort.t) => {
  MGrammar.v
  |> Mtrl.Sort.Map.find(s)
  |> Prec.Table.mapi(((p, a), rgx) => {
       // need to check for legal bounded entry from both sides
       let go = (from: Dir.t, bounded): list(Molded.Sort.t) =>
         // currently filtering without assuming single operator form
         // for each prec level. this may need to change.
         RZipper.enter(~from, rgx)
         |> List.filter_map(((msym, rctx)) => {
              let msrt = expect_sort(msym);
              let mold = Mold.{sort: s, prec: p, rctx};
              bounded || Mtrl.is_space(msrt)
                ? Some(Molded.{mtrl: msrt, mold}) : None;
            });
       switch (go(L, Prec.lt(~a, l, p)), go(R, Prec.gt(~a, p, r))) {
       | ([], _)
       | (_, []) => []
       | ([_, ..._] as ent_l, [_, ..._] as ent_r) =>
         Dir.choose(from, ent_l, ent_r)
       };
     })
  |> List.concat;
};

let stride_over = (~from: Dir.t, sort: Molded.Sort.t): Index.t => {
  let eq = Walk.unit(Stride.Eq(sort));
  (Sym.NT(sort.mtrl), sort.mold.rctx)
  |> RZipper.step(Dir.toggle(from))
  |> List.map(
       Bound.map(((msym, rctx)) => {
         let mlbl = expect_lbl(msym);
         Molded.{
           mtrl: mlbl,
           mold: {
             ...mold,
             rctx,
           },
         };
       }),
     )
  |> List.map(lbl => (lbl, [eq]))
  |> Index.of_list;
};

let stride_into = (~from: Dir.t, sort: Bound.t(Molded.Sort.t)): Index.t => {
  let bounds = (s: Molded.Sort.t) => {
    let (l_sort, r_sort) = Molded.Sort.bounds(sort);
    let (l_s, r_s) = Molded.Sort.bounds(Node(s));
    Dir.choose(from, (l_sort, r_s), (l_s, r_sort));
  };
  let seen = Hashtbl.create(10);
  let rec go = (s: Molded.Sort.t) =>
    switch (Hashtbl.find_opt(seen, s.mtrl)) {
    | Some () => Index.empty
    | None =>
      Hashtbl.add(seen, s.mtrl, ());
      let (l, r) = bounds(s);
      enter(~from, ~l, ~r, s.mtrl)
      |> List.map(s => Index.union(stride_over(~from, s), go(s)))
      |> Index.union_all;
    };
  let stepped =
    switch (sort) {
    | Node(s) => go(s)
    | Root =>
      Hashtbl.add(seen, Mtrl.Sort.root, ());
      enter(~from, Mtrl.Sort.root)
      |> List.map(s => Index.union(stride_over(~from, s), go(s)))
      |> Index.union_all;
    };
  Index.map(Walk.bound(~from, sort), stepped);
};

let step = (~from: Dir.t, src: End.t) =>
  switch (src) {
  | Root => stride_into(~from, Root)
  | Node(lbl) =>
    (Sym.T(lbl.mtrl), lbl.mold.rctx)
    |> RZipper.step(Dir.toggle(from))
    |> List.map(
         Bound.map(((msym, rctx)) => {
           let msrt = expect_srt(msym);
           Molded.{
             mtrl: msrt,
             mold: {
               ...mold,
               rctx,
             },
           };
         }),
       )
    |> List.concat_map(sort => {
         let into = stride_into(~from, sort);
         let over =
           Bound.is_node(sort)
           |> Option.map(stride_over(~from))
           |> Option.value(~default=Index.empty);
         [over, into];
       })
    |> Index.union_all
  };

let walk = (~from: Dir.t, src: End.t) => {
  let seen = Hashtbl.create(100);
  let rec go = (src: Bound.t(Molded.Label.t)) =>
    switch (Hashtbl.find_opt(seen, src)) {
    | Some () => Index.empty
    | None =>
      Hashtbl.add(seen, src, ());
      let stepped = step(~from, src);
      let walked = {
        open Index.Syntax;
        let* (src_mid, mid) = stepped;
        switch (mid) {
        | Root => Index.empty
        | Node(mid) =>
          let* (mid_dst, dst) = go(Node(mid));
          return(Walk.append(src_mid, mid, mid_dst), dst);
        };
      };
      Index.union(stepped, walked);
    };
  go(src);
};
