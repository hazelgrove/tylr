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

type hpat =
  | InvalidPat
  | PatHole
  | Var(Token.t);

type hexp = {
  id: Id.t,
  term,
}
and value =
  | Triv
  | Bool(bool)
  | Int(int)
  | Fun(hpat, hexp)
//| FunAnn(Token.t, htyp, hexp)
and term =
  | InvalidExp //everything? text? keyword?
  | EmptyHole
  | Lit(value)
  | Var(Token.t)
  | Let(hpat, hexp, hexp)
  //| LetAnn(Token.t, htyp, hexp, hexp)
  | Ap(hexp, hexp)
  //| ApBuiltin(Token.t, list(hexp))
  // maybe everything with fn semantics should be a builtin e.g. plus??
  | If(hexp, hexp, hexp)
  | OpInt(op_int, hexp, hexp)
  | OpBool(op_bool, hexp, hexp);

let zip_to_hexp = (_z: Zipper.t): hexp => {id: 666, term: InvalidExp};

type mode =
  | Syn
  | Ana(typ);

type ctx_entry = {
  id: Id.t,
  typ,
};

type ctx = VarMap.t_(ctx_entry);

type co_ctx_entry = list(Id.t);

type co_ctx = VarMap.t_(co_ctx_entry);

type info = {
  syn_typ: typ,
  mode,
  ctx,
  co_ctx,
};
