open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  wal: Wald.t,
  mel: Meld.t,
};
[@deriving (show({with_path: false}), sexp, yojson)]
type terr = t;

let of_wald = wal => {wal, mel: Meld.empty()};
let of_piece = p => of_wald(Wald.of_piece(p));

let map_wal = (f, terr) => {...terr, wal: f(terr.wal)};

let split_face = ({wal, mel}: t): (Piece.t, Meld.t) =>
  switch (Chain.unlink(wal)) {
  | None => (Chain.fst(wal), mel)
  | Some((face, kid, rest)) => (face, Wald.unmk(~l=kid, rest, ~r=mel))
  };
let face = terr => fst(split_face(terr));
let sort = (terr: t) => Piece.sort(face(terr));
let prec = (terr: t) => Piece.prec(face(terr));

module L = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = terr; // left-to-right: wal mel

  let mk = (mel: Meld.t): option((Meld.t, t)) =>
    Wald.mk(mel) |> Option.map(((kid, wal, mel)) => (kid, {wal, mel}));
  let unmk = (kid, {wal, mel}: t) => Wald.unmk(~l=kid, wal, ~r=mel);
  let append = (_: Meld.t, _: t) => failwith("todo append");

  let pad = (t: t, s: Space.t) => {...t, mel: Meld.pad(t.mel, ~r=s)};

  let rec mk_s = (mel: Meld.t): (Space.t, list(t)) =>
    switch (mk(mel)) {
    | None =>
      let ((l, r), _empty) = Meld.unpad(mel);
      (Space.cat(l, r), []);
    | Some((kid, l)) =>
      let (s, ls) = mk_s(kid);
      (s, [l, ...ls]);
    };

  let uncons_lexeme = (~char=false, l: t): (Lexeme.t, Space.t, list(t)) => {
    let (face, rest) = split_face(l);
    switch (Piece.unzip(1, face)) {
    | R((c, rest_face)) when char =>
      let (_empty, l) = Option.get(mk(Meld.link(rest_face, rest)));
      (Lexeme.of_piece(c), Space.empty, [l]);
    | _ =>
      let (s, ls) = mk_s(rest);
      (Lexeme.of_piece(face), s, ls);
    };
  };

  let tip = (terr: t) => Piece.tip(L, face(terr));
};

module R = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = terr; // left-to-right: mel wal

  let mk = (mel: Meld.t): option((t, Meld.t)) =>
    Wald.mk(mel) |> Option.map(((mel, wal, kid)) => ({mel, wal}, kid));
  let unmk = ({mel, wal}: t, kid) => Wald.unmk(~l=mel, wal, ~r=kid);

  let pad = (s: Space.t, t: t) => {...t, mel: Meld.pad(~l=s, t.mel)};

  let rec mk_s = (mel: Meld.t): (list(t), Space.t) =>
    switch (mk(mel)) {
    | None =>
      let ((l, r), _empty) = Meld.unpad(mel);
      ([], Space.cat(l, r));
    | Some((r, kid)) =>
      let (rs, s) = mk_s(kid);
      ([r, ...rs], s);
    };

  let prepend = (_: t, _: Meld.t) => failwith("todo prepend");

  let unsnoc_lexeme = (~char=false, r: t): (list(t), Space.t, Lexeme.t) => {
    print_endline("Terrace.unsnoc_lexeme");
    let (face, rest) = split_face(r);
    // left-to-right: rest face
    switch (Piece.unzip(Piece.length(face) - 1, face)) {
    | R((rest_face, c)) when char =>
      let (r, _empty) = Option.get(mk(Meld.knil(rest, rest_face)));
      ([r], Space.empty, Lexeme.of_piece(c));
    | _ =>
      let (rs, s) = mk_s(rest);
      (rs, s, Lexeme.of_piece(face));
    };
  };

  let tip = (terr: t) => Piece.tip(R, face(terr));
  // todo: remove option from return type, added just to get things typechecking
  // todo: review whether match flag is needed here
  let mold = (terr: t, ~kid: option(Sort.o)=?, t: Token.t) =>
    switch (tip(terr)) {
    | Convex => Error(Some(sort(terr)))
    | Concave(s, _) =>
      LangUtil.mold_of_token(kid, s, t)
      |> Result.of_option(~error=Some(sort(terr)))
    };

  let complement = (terr: t) => Piece.complement(~side=R, face(terr));
};

// todo: consider requiring kid already be completed
let lt = (l: R.t, ~kid=Meld.empty(), r: L.t): option(Meld.t) => {
  open OptUtil.Syntax;
  let (p_l, p_r) = (face(l), face(r));
  let+ _ = Piece.lt(p_l, p_r);
  let kid =
    switch (Piece.tip(L, p_r)) {
    | Convex =>
      assert(Option.is_some(Meld.is_empty(kid)));
      kid;
    | Concave(s, _) =>
      // todo review strict flag
      Meld.complete(~expected=Sort.Ana.mk(~sort=s, ()), kid)
    };
  L.unmk(kid, r);
};

let gt = (l: R.t, ~kid=Meld.empty(), r: L.t): option(Meld.t) => {
  open OptUtil.Syntax;
  let (p_l, p_r) = (face(l), face(r));
  let+ _ = Piece.gt(p_l, p_r);
  let kid =
    switch (Piece.tip(R, p_l)) {
    | Convex =>
      assert(Option.is_some(Meld.is_empty(kid)));
      kid;
    | Concave(s, _) =>
      // todo review strict flag
      Meld.complete(~expected=Sort.Ana.mk(~sort=s, ()), kid)
    };
  R.unmk(l, kid);
};

let rec eq = (l: R.t, ~kid=Meld.empty(), r: L.t): option(Meld.t) => {
  open OptUtil.Syntax;
  let ((p_l, tl_l), (p_r, tl_r)) = (split_face(l), split_face(r));
  // left-to-right: tl_l p_l p_r tl_r
  switch (
    // todo: relax to porous
    Meld.is_empty(kid),
    Piece.zip(p_l, p_r),
    Piece.replaces(p_l, p_r),
  ) {
  | (Some(s), Some(p), _) when Space.is_empty(s) =>
    return(Meld.append(tl_l, p, tl_r))
  | (Some(s), _, Some(L)) =>
    let* (l, kid) = R.mk(tl_l);
    eq(l, ~kid=Meld.pad(kid, ~r=s), r);
  | (Some(s), _, Some(R)) =>
    let* (kid, r) = L.mk(r.mel);
    eq(l, ~kid=Meld.pad(~l=s, kid), r);
  | _ =>
    let+ compl = Piece.eq(p_l, p_r);
    // todo: abstract into some join-complement fn
    let r =
      List.fold_right(
        ((sugg, mold), r) =>
          switch (Mold.tip(R, mold)) {
          | Convex => raise(Gram.Ill_typed)
          | Concave(s, _) =>
            let kid = Meld.of_grout(Grout.mk_convex(s));
            let g = Piece.of_grout(Grout.mk(~sugg, mold));
            map_wal(Chain.link(g, kid), r);
          },
        compl,
        r,
      );
    R.prepend(l, L.unmk(kid, r));
  };
};

let in_ = (_, ~s as _: Space.t, _) => failwith("todo Terrace.in_");

type cmp = {
  lt: option(Meld.t),
  eq: option(Meld.t),
  gt: option(Meld.t),
};
let cmp = (l: R.t, ~kid=Meld.empty(), r: L.t) => {
  lt: lt(l, ~kid, r),
  eq: eq(l, ~kid, r),
  gt: gt(l, ~kid, r),
};
