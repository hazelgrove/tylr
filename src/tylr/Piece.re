open Sexplib.Std;
open Util;

module Shape = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | T(Tile.t)
    | G(Grout.t);
};

module Path = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = int;
  let shift = (n, p) => p + n;
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  shape: Shape.t,
  paths: list(Path.t),
};

let mk = (~paths=[], shape) => {paths, shape};
let of_grout = (~paths=[], g) => mk(~paths, G(g));
let of_tile = (~paths=[], t) => mk(~paths, T(t));

let add_paths = (ps, p) => {...p, paths: ps @ p.paths};
let clear_paths = p => {...p, paths: []};

// let pad = (~l=Space.empty, ~r=Space.empty, {shape, space: (l', r')}) => {
//   shape,
//   space: (l @ l', r' @ r),
// };

let is_porous = _ => failwith("todo is_porous");

// let space_l = ({space: (l, _), _}) => l;
// let space_r = ({space: (_, r), _}) => r;
// let space = ({space: (l, r), _}) => l @ r;

let id = p =>
  switch (p.shape) {
  | G(g) => g.id
  | T(t) => t.id
  };

let length = (p: t) => {
  switch (p.shape) {
  | T(t) => Tile.length(t)
  | G(g) => Grout.length(g)
  };
};

let zip = (l: t, r: t): option(t) => {
  open OptUtil.Syntax;
  let paths = l.paths @ List.map(Path.shift(length(l)), r.paths);
  let+ shape =
    switch (l.shape, r.shape) {
    | (G(_), T(_))
    | (T(_), G(_)) => None
    | (G(g_l), G(g_r)) =>
      let+ g = Grout.zip(g_l, g_r);
      Shape.G(g);
    | (T(t_l), T(t_r)) =>
      let+ t = Tile.zip(t_l, t_r);
      Shape.T(t);
    };
  mk(~paths, shape);
};

// let pop_space_l = ({shape, space: (l, r)}: t) => (
//   l,
//   {shape, space: (Space.empty, r)},
// );
// let pop_space_r = ({shape, space: (l, r)}: t) => (
//   {shape, space: (l, Space.empty)},
//   r,
// );

let mold = p =>
  switch (p.shape) {
  | T(t) => t.mold
  | G(g) => g.mold
  };
let sort = p => Mold.sort_(mold(p));
let prec = p => Mold.prec_(mold(p));
let tip = (side, p) => Mold.tip(side, mold(p));

let is_grout = p =>
  switch (p.shape) {
  | G(_) => true
  | T(_) => false
  };

let is_strict = p =>
  switch (p.shape) {
  | T(_) => true
  | G(g) => Grout.has_sugg(g)
  };

let zipper = (p: t): Gram.Zipper.a(_) => {
  let t =
    switch (p.shape) {
    | G(g) => g.sugg
    | T(t) => t.token
    };
  (Tok(LangUtil.shape_of_token(t)), mold(p).frames);
};

let complement = (~side: Dir.t, p: t): list((Token.t, Mold.t)) => {
  let rec go = z =>
    switch (Gram.Zipper.move_to_tok(~skip_nullable=true, side, z)) {
    // default to first alternative
    | [(Tok(Const(t)), frames) as z, ..._] =>
      let m = {...mold(p), frames};
      [(t, m), ...go(z)];
    | _ => []
    };
  go(zipper(p));
};

let eq = (l: t, r: t): option(Sort.Ana.t) => {
  switch (Mold.tip(R, mold(l))) {
  | Convex => None
  | Concave(sort, _) =>
    let (z_l, z_r) = (zipper(l), zipper(r));
    let (moved_l, moved_r) =
      Gram.Zipper.(move_to_tok(R, z_l), move_to_tok(L, z_r));
    let strict = is_strict(l) || is_strict(r);
    List.mem(z_l, moved_r) && List.mem(z_r, moved_l)
      ? Some(Sort.Ana.mk(~strict, ~sort?, ())) : None;
  };
};

let eq_transitive = (l: t, r: t): bool => {
  let rec go = (z_l, z_r) => {
    let moved_r = Gram.Zipper.move_to_tok(L, z_r);
    List.mem(z_l, moved_r) ? true : List.exists(go(z_l), moved_r);
  };
  go(zipper(l), zipper(r));
};

// separate from cmp bc these are only relevant
// based on surrounding meld kids (see Meld.merge/degrout)
type dg =
  | Degrout
  | Fill(Dir.t)
  | Pass(Dir.t);

let degrout = (l: t, r: t): option(dg) =>
  switch (l.shape, r.shape) {
  | (T(_), T(_)) => None
  | (G(_), _) when mold(l) == mold(r) => Some(Fill(L))
  | (_, G(_)) when mold(l) == mold(r) => Some(Fill(R))
  | (G(_), _) when eq_transitive(r, l) => Some(Pass(L))
  | (_, G(_)) when eq_transitive(r, l) => Some(Pass(R))
  | (G(g), _) when Grout.has_sugg(g) => None
  | (_, G(g)) when Grout.has_sugg(g) => None
  // todo: probably need strengthen this check for degrouting
  | (G(_), G(_))
      when Tip.fits(Mold.tip(L, mold(l)), Mold.tip(R, mold(r))) =>
    Some(Degrout)
  | (G(_), _) =>
    Tip.same_shape(Mold.tip(L, mold(l)), Mold.tip(L, mold(r)))
      ? Some(Fill(L)) : None
  | (_, G(_)) =>
    Tip.same_shape(Mold.tip(R, mold(l)), Mold.tip(R, mold(r)))
      ? Some(Fill(R)) : None
  };

let cmp = (l: t, r: t): (Cmp.i_leg((Sort.o, Prec.t), Sort.Ana.t) as 'r) => {
  let (m_l, m_r) = (mold(l), mold(r));
  switch (eq(l, r)) {
  | Some(s) => Eq(s)
  | None =>
    let in_ = {
      let sort = Sort.lca(m_l.sort, m_r.sort);
      let prec = min(m_l.prec, m_r.prec);
      Cmp.In((sort, prec));
    };
    let lt = sort => Cmp.Lt(Sort.Ana.mk(~strict=is_strict(l), ~sort?, ()));
    let gt = sort => Cmp.Gt(Sort.Ana.mk(~strict=is_strict(r), ~sort?, ()));
    let eq = sort => {
      let strict = is_strict(l) || is_strict(r);
      Cmp.Eq(Sort.Ana.mk(~strict, ~sort?, ()));
    };
    let try_lt = (~else_=in_, sort_l): 'r =>
      Sort.compare_o(sort_l, sort(r)) <= 0 ? lt(sort_l) : else_;
    let try_gt = (~else_=in_, sort_r): 'r =>
      Sort.compare_o(sort(l), sort_r) >= 0 ? gt(sort_r) : else_;
    switch (Mold.tip(R, m_l), Mold.tip(L, m_r)) {
    | (Convex, Convex) => in_
    | (Convex, Concave(sort_r, _)) => try_gt(sort_r)
    | (Concave(sort_l, _), Convex) => try_lt(sort_l)
    | (Concave(sort_l, prec_l), Concave(sort_r, prec_r)) =>
      // todo: revise when generalizing to sort partial order
      switch (Sort.compare_o(sort(l), sort(r))) {
      | c when c < 0 => lt(sort_l)
      | c when c > 0 => gt(sort_r)
      | _zero =>
        switch (Prec.compare(prec_l, prec_r)) {
        | c when c < 0 => try_lt(~else_=try_gt(sort_r), sort_l)
        | c when c > 0 => try_gt(~else_=try_lt(sort_l), sort_r)
        | _ =>
          switch (LangUtil.assoc(sort(l), prec_l)) {
          | Some(L) => gt(sort_r)
          | Some(R) => lt(sort_l)
          | None => eq(sort_l)
          }
        }
      }
    };
  };
};

// module Step = {
//   [@deriving (show({with_path: false}), sexp, yojson)]
//   type t = int;
// };

// let unzip = (step: Step.t, p: t): Either.t(Dir.t, (t, t)) => {
//   let (l, r) = p.space;
//   switch (p.shape) {
//   | G(g) =>
//     Grout.unzip(step, g)
//     |> Either.map_r(((g_l, g_r)) => (mk(~l, G(g_l)), mk(~r, G(g_r))))
//   | T(t) =>
//     Tile.unzip(step, t)
//     |> Either.map_r(((t_l, t_r)) => (mk(~l, T(t_l)), mk(~r, T(t_r))))
//   };
// };
