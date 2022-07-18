type htyp =
  | InvalidType
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
  | Invalid //everything? text? keyword?
  | EmptyHole
  | Lit(value)
  | Var(Token.t)
  | Let(hpat, hexp, hexp)
  //| LetAnn(Token.t, htyp, hexp, hexp)
  | Ap(hexp, hexp)
  //| ApBuiltin(Token.t, list(hexp))
  | If(hexp, hexp, hexp)
  | OpInt(op_int, hexp, hexp)
  | OpBool(op_bool, hexp, hexp);
