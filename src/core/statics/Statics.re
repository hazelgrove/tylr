type typ =
  | Unknown
  | Int
  | Bool
  | Arrow(typ, typ);

type htyp =
  | InvalidTyp(Piece.t)
  | TypeHole
  | Int
  | Bool
  | Arrow(htyp, htyp);

type op_int =
  | Plus;

type op_bool =
  | And;

type term_p =
  | InvalidPat(Piece.t)
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
  | Tile({id, label, children, mold: _, shards: _} as t) =>
    // TODO(andrew): do better than switching label
    let term =
      switch (/*mold.out,*/ label, children_h, children) {
      | _ when !Tile.is_complete(t) => InvalidPiece(p)
      | (["+"], [l, r], []) => OpInt(Plus, l, r)
      | (["&&"], [l, r], []) => OpBool(And, l, r)
      | (["fun", "->"], [body], [pat]) => Fun(hpat_of_seg(pat), body)
      | (["let", "=", "in"], [body], [pat, def]) =>
        Let(hpat_of_seg(pat), of_seg(def), body)
      | (["if", "then", "else"], [alt], [cond, conseq]) =>
        If(of_seg(cond), of_seg(conseq), alt)
      | (["(", ")"], [fn], [arg]) => Ap(fn, of_seg(arg))
      //TODO(andrew): more cases
      | _ => InvalidPiece(p)
      };
    {id, term};
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

// TYPES:

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

type info_exp = {
  syn_typ: typ,
  mode,
  ctx,
  co_ctx,
};

type info =
  | InfoExp(info_exp)
  | InfoPat(info_exp); //TODO(andrew)

type info_map = Id.Map.t(info);

let hexp_to_info_map =
    (
      ~m=Id.Map.empty,
      ~ctx as _=VarMap.empty,
      ~mode as _=Syn,
      {id: _, term}: hexp,
    )
    : info_map => {
  //TODO(andrew): implement
  switch (term) {
  | InvalidPiece(_p) => m
  | EmptyHole => m
  | Bool(_b) => m
  | Int(_i) => m
  | Fun(_hpat, _hexp) => m
  | Var(_name) => m
  | Let(_hpat, _hexp, _hexp') => m
  | Ap(_hexp, _hexp') => m
  | If(_hexp, _hexp', _hexp'') => m
  | OpInt(_op_int, _hexp, _hexp') => m
  | OpBool(_op_bool, _hexp, _hexp') => m
  };
};

//let hexp_to_info_map = hexp_to_info_map'(Id.Map.empty);
