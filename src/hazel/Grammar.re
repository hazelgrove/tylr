module Sym = {
  include Sym;
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t = Sym.t(Label.t, Sort.t);
};
module Regex = {
  include Regex;
  type t = Regex.t(Sym.t);
};
open Regex;

let p = (~a: option(Dir.t)=?, r: t) => (a, r);

let t = (lbl: Label.t) => Regex.atom(Sym.t(lbl));
let nt = (srt: Sort.t) => Regex.atom(Sym.nt(srt));

let c = (~p=Padding.none, s) => t(Label.const(~padding=p, s));
let kw = (~space=(true, true), ~break=(false, false), ~indent=true) =>
  c(~p=Padding.kw(~space, ~break, ~indent, ()));
let op = (~space=(true, true), ~break=(false, false), ~indent=true) =>
  c(~p=Padding.op(~space, ~break, ~indent, ()));
let brc = (side: Dir.t) => c(~p=Padding.brc(side));

let comma = op(~space=(false, true), ",");
let comma_sep = atom => seq([atom, Star(seq([comma, atom]))]);

module Typ = {
  let sort = Sort.of_str("Typ");
  let typ = nt(sort);

  let operand =
    alt([
      c("Int"),
      c("Float"),
      c("Bool"),
      c("String"),
      //List type
      seq([c("list"), brc(L, "("), typ, brc(R, ")")]),
      //Tuple type
      seq([brc(L, "("), comma_sep(typ), brc(R, ")")]),
    ]);

  let tbl = [
    //Arrow
    //TODO: should the below be "ch" or c? (padding with horizontal spaces or none?)
    p(seq([typ, op("->"), typ])),
    //Ap
    p(seq([typ, brc(L, "("), typ, brc(R, ")")])),
    p(operand),
  ];
};

module Pat = {
  let sort = Sort.of_str("Pat");
  let pat = nt(sort);

  let bool_lit = alt([c("true"), c("false")]);
  let operand =
    alt([
      t(Int_lit),
      t(Float_lit),
      t(Id_lower),
      bool_lit,
      //Constructor
      t(Id_upper),
      seq([brc(L, "("), comma_sep(pat), brc(R, ")")]),
      seq([brc(L, "["), comma_sep(pat), brc(R, "]")]),
      //Wild
      c("_"),
    ]);

  let tbl = [
    //Typeann
    p(~a=L, seq([pat, c(":"), nt(Typ.sort)])),
    //Cons
    p(~a=R, seq([pat, c("::"), pat])),
    //ap
    p(seq([pat, brc(L, "("), pat, brc(R, ")")])),
    p(operand),
  ];
};

module Exp = {
  let sort = Sort.of_str("Exp");
  let exp = nt(sort);

  let bool_lit = alt([c("true"), c("false")]);

  let rul =
    seq([op(~break=(true, false), "|"), nt(Pat.sort), op("=>"), exp]);
  let case = seq([kw(~space=(false, true), "case"), exp, rul, star(rul)]);

  let let_ =
    seq([
      kw("let", ~space=(false, true)),
      nt(Pat.sort),
      op("="),
      exp,
      kw("in", ~break=(false, true), ~indent=false),
      exp,
    ]);

  let operand =
    alt([
      t(Int_lit),
      t(Float_lit),
      t(Id_lower),
      bool_lit,
      seq([brc(L, "("), comma_sep(exp), brc(R, ")")]),
      seq([brc(L, "["), comma_sep(exp), brc(R, "]")]),
    ]);

  let op_alt = ss => alt(List.map(op, ss));
  let add_op = op_alt(["+", "+.", "-", "-.", "@", "++"]);
  let mult_op = op_alt(["*", "*.", "/", "/."]);
  let neg_op = op_alt(["-", "-."]);

  let tbl = [
    //case
    p(case),
    //let
    p(let_),
    //fun
    p(
      seq([kw(~space=(false, true), "fun"), nt(Pat.sort), op("->"), exp]),
    ),
    //if
    p(
      seq([
        kw(~space=(false, true), "if"),
        exp,
        kw("then"),
        exp,
        kw("else"),
        exp,
      ]),
    ),
    //Math operations
    p(~a=L, seq([exp, add_op, exp])),
    p(~a=L, seq([exp, mult_op, exp])),
    p(seq([neg_op, exp])),
    //ap
    p(seq([exp, brc(L, "("), exp, brc(R, ")")])),
    p(operand),
  ];
};

type t = Sort.Map.t(Prec.Table.t(Regex.t));
let v =
  [Typ.(sort, tbl), Pat.(sort, tbl), Exp.(sort, tbl)]
  |> List.to_seq
  |> Sort.Map.of_seq;
