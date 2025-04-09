module B = Bound;
module Bound = {
  include Bound;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Bound.t(Terr.t);
};

module Base = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    slope: Slope.t,
    // may want to limit this to tok or make polymorphic
    bound: Bound.t,
  };
};
include Base;

let mk = (~slope=Slope.empty, bound) => {slope, bound};
let empty = {slope: Slope.empty, bound: Bound.root};

let cat = (slope: Slope.t, stack: t) => {
  ...stack,
  slope: Slope.cat(slope, stack.slope),
};

let face = (~from: Dir.t, stack: t) => {
  let (face, _) = Slope.pull(~from, stack.slope);
  switch (face) {
  | Node(tok) => B.Node(tok)
  | Root =>
    stack.bound |> Bound.map(terr => fst(Slope.pull_terr(~from, terr)))
  };
};

let merge_hd = (~onto: Dir.t, t: Token.t, stack: t) =>
  switch (stack) {
  | {slope: [], bound: Root} => None
  | {slope: [], bound: Node(terr)} =>
    Terr.merge_hd(~onto, t, terr)
    |> Option.map(terr => {...stack, bound: Node(terr)})
  | {slope: [_, ..._], _} =>
    Slope.merge_hd(~onto, t, stack.slope)
    |> Option.map(slope => {...stack, slope})
  };

let to_slope = (stack: t) =>
  stack.slope @ Option.to_list(Bound.to_opt(stack.bound));

let extend = (tl, stack) =>
  switch (stack.slope) {
  | [] => {...stack, bound: Bound.map(Terr.extend(tl), stack.bound)}
  | [_, ..._] => {...stack, slope: Slope.extend(tl, stack.slope)}
  };

let link = (t: Token.t, (sw: Walk.Swing.t, c: Cell.t), stack: t) =>
  switch (stack.slope) {
  | [] =>
    switch (stack.bound) {
    | Node(terr) when Walk.Swing.is_eq(sw) => {
        ...stack,
        bound: Node(Terr.link(t, c, terr)),
      }
    | _ =>
      if (!Walk.Swing.is_neq(sw)) {
        open Stds;
        P.log("--- Stack.link/failed neq assert ---");
        P.show("t", Token.show(t));
        P.show("sw", Walk.Swing.show(sw));
        P.show("c", Cell.show(c));
        P.show("stack", show(stack));
        failwith("expected neq swing to link to root bound");
      };
      let terr = Terr.Base.{wald: Wald.of_tok(t), cell: c};
      {...stack, slope: [terr]};
    }
  | [hd, ...tl] =>
    if (Walk.Swing.is_eq(sw)) {
      let hd = Terr.link(t, c, hd);
      {...stack, slope: [hd, ...tl]};
    } else {
      let terr = Terr.Base.{wald: Wald.of_tok(t), cell: c};
      {...stack, slope: [terr, hd, ...tl]};
    }
  };

let connect_affix =
    (affix: Chain.Affix.t(Token.t, (Walk.Swing.t, Cell.t)), stack: t) =>
  affix
  |> Chain.Affix.fold_out(~init=stack, ~f=(tok, (swing, cell)) =>
       link(tok, (swing, cell))
     );

let connect = (t: Token.t, grouted: Grouted.t, stack: t) =>
  connect_affix(Chain.Affix.cons(t, grouted), stack);

let connect_ = (grouted: Grouted.t, stack: t): ((Walk.Swing.t, Cell.t), t) =>
  grouted
  |> Chain.fold_right(
       (sw_c, t, (sw_c_acc, stack_acc)) =>
         (sw_c, link(t, sw_c_acc, stack_acc)),
       sw_c => (sw_c, stack),
     );

module Frame = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = (Base.t, Base.t);
  let empty = (empty, empty);
  let cat = ((slope_l, slope_r), (stack_l, stack_r)) => (
    cat(slope_l, stack_l),
    cat(slope_r, stack_r),
  );
};
