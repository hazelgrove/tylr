[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  slope: Slope.t,
  // may want to limit this to tok or make polymorphic
  bound: Bound.t(Terr.t),
};

let empty = {slope: Slope.empty, bound: Bound.Root};

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
        failwith("expected neq swing");
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

let connect = (t: Token.t, grouted: Grouted.t, stack: t) =>
  Chain.Affix.cons(t, grouted)
  |> Chain.Affix.fold_out(~init=stack, ~f=(tok, (swing, cell)) =>
       link(tok, (swing, cell))
     );
