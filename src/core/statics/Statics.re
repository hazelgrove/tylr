type typ =
  | Unknown
  | Int
  | Bool
  | Arrow(typ, typ);

type htyp =
  | InvalidTyp
  | TypeHole
  | Int
  | Bool
  | Arrow(htyp, htyp);

type op_int =
  | Plus;

type op_bool =
  | And;

type term_p =
  | InvalidPat(Piece.t) //TODO(andrew): piecify
  | EmptyHolePat
  | VarPat(Token.t);

type hpat = {
  id: Id.t,
  term_p,
};

type hexp = {
  id: Id.t,
  term,
}
//| FunAnn(Token.t, htyp, hexp)
and term =
  | InvalidPiece(Piece.t) //everything? text? keyword?
  //| InvalidSegment(Segment.t)
  | EmptyHole
  //| Triv
  | Bool(bool)
  | Int(int)
  | Fun(hpat, hexp)
  | Var(Token.t)
  | Let(hpat, hexp, hexp)
  //| LetAnn(Token.t, htyp, hexp, hexp)
  | Ap(hexp, hexp)
  //| ApBuiltin(Token.t, list(hexp))
  // maybe everything with fn semantics should be a builtin e.g. plus??
  | If(hexp, hexp, hexp)
  | OpInt(op_int, hexp, hexp)
  | OpBool(op_bool, hexp, hexp);

type mode =
  | Syn
  | Ana(typ);

type ctx_entry = {
  id: Id.t,
  typ,
};

type ctx = VarMap.t_(ctx_entry);

type co_ctx_item = {
  id: Id.t,
  mode,
};

type co_ctx_entry = list(co_ctx_item);

type co_ctx = VarMap.t_(co_ctx_entry);

type info = {
  syn_typ: typ,
  mode,
  ctx,
  co_ctx,
};

let invalid = (p: Piece.t) => {id: (-1), term: InvalidPiece(p)};

let piece_and_kids = (ps, skel: Skel.t): (Piece.t, list(Skel.t)) => {
  let at = List.nth(ps);
  switch (skel) {
  | Op(idx) => (at(idx), [])
  | Pre(idx, skel') => (at(idx), [skel'])
  | Post(skel', idx) => (at(idx), [skel'])
  | Bin(skel_l, idx, skel_r) => (at(idx), [skel_l, skel_r])
  };
};

let rec of_seg_and_skel = (ps: Segment.t, skel: Skel.t): hexp => {
  let (p, kids) = piece_and_kids(ps, skel);
  of_piece(p, List.map(of_seg_and_skel(ps), kids));
}
and of_piece = (p: Piece.t, children_h: list(hexp)): hexp => {
  switch (p) {
  | Whitespace(_) => invalid(p)
  | Grout({id, shape}) =>
    switch (shape) {
    | Convex => {id, term: EmptyHole}
    | Concave => invalid(p)
    }
  | Tile({id, label, children, mold, shards: _} as t) =>
    // TODO(andrew): do better than switching label
    let id = (term): hexp => {id, term};
    switch (mold.out, label, children_h, children) {
    | _ when !Tile.is_complete(t) => {id: (-1), term: InvalidPiece(p)}
    | (Exp, ["+"], [l, r], []) => id(OpInt(Plus, l, r))
    | (Exp, ["&&"], [l, r], []) => id(OpBool(And, l, r))
    | (Exp, ["fun", "->"], [body], [pat]) =>
      id(Fun(hpat_of_seg(pat), body))
    | (Exp, ["let", "=", "in"], [body], [pat, def]) =>
      id(Let(hpat_of_seg(pat), of_seg(def), body))
    | (Exp, ["if", "then", "else"], [alt], [cond, conseq]) =>
      id(If(of_seg(cond), of_seg(conseq), alt))
    | (Exp, ["(", ")"], [fn], [arg]) => id(Ap(fn, of_seg(arg)))
    //TODO(andrew): more cases
    | _ => id(InvalidPiece(p))
    };
  };
}
and of_seg = (ps: Segment.t): hexp =>
  ps |> Segment.skel |> of_seg_and_skel(ps)
and of_seg_and_skel_pat = (ps: Segment.t, skel: Skel.t): hpat => {
  let (p, kids) = piece_and_kids(ps, skel);
  of_piece_pat(p, List.map(of_seg_and_skel(ps), kids));
}
and hpat_of_seg = (ps: Segment.t): hpat =>
  ps |> Segment.skel |> of_seg_and_skel_pat(ps)
and of_piece_pat = (p: Piece.t, children_h: list(hexp)): hpat => {
  let invalid = {id: (-1), term_p: InvalidPat(p)};
  switch (p) {
  | Whitespace(_) => invalid
  | Grout({id, shape}) =>
    switch (shape) {
    | Convex => {id, term_p: EmptyHolePat}
    | Concave => invalid
    }
  | Tile({id, label, children, mold, shards: _} as t) =>
    // TODO(andrew): do better than switching label
    let id = (term_p): hpat => {id, term_p};
    switch (mold.out, label, children_h, children) {
    | _ when !Tile.is_complete(t) => id(InvalidPat(p))
    | (Exp, [t], [], []) when Form.is_var(t) => id(VarPat(t))
    | _ => id(InvalidPat(p))
    };
  };
};
//TODO(andrew): implement above

let zip_to_hexp = (z: Zipper.t): hexp => z |> Zipper.zip |> of_seg;
