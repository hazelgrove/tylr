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
let kw = (~l=true, ~r=true, ~indent=true) =>
  c(~p=Padding.kw(~l, ~r, ~indent, ()));
let op = (~l=true, ~r=true, ~indent=true) =>
  c(~p=Padding.op(~l, ~r, ~indent, ()));
let brc = (side: Dir.t) => c(~p=Padding.brc(side));

module Typ = {
  let sort = Sort.of_str("Typ");
  let typ = nt(sort);

  let comma_sep = seq([typ, Star(seq([op(~l=false, ","), typ]))]);

  let operand =
    alt([
      c("Int"),
      c("Float"),
      c("Bool"),
      c("String"),
      //List type
      seq([c("list"), brc(L, "("), typ, brc(R, ")")]),
      //Tuple type
      seq([brc(L, "("), comma_sep, brc(R, ")")]),
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

  let comma_sep = seq([pat, Star(seq([op(~l=false, ","), pat]))]);

  let bool_lit = alt([c("true"), c("false")]);
  let operand =
    alt([
      t(Int_lit),
      t(Float_lit),
      t(Id_lower),
      bool_lit,
      //Constructor
      t(Id_upper),
      seq([brc(L, "("), comma_sep, brc(R, ")")]),
      seq([brc(L, "["), comma_sep, brc(R, "]")]),
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

  [@warning "-32"]
  let comma_sep = seq([exp, Star(seq([op(~l=false, ","), exp]))]);

  let rul = seq([op("|"), nt(Pat.sort), op("=>"), exp]);
  let bool_lit = alt([c("true"), c("false")]);

  let operand =
    alt([
      t(Int_lit),
      t(Float_lit),
      t(Id_lower),
      bool_lit,
      seq([brc(L, "("), comma_sep, brc(R, ")")]),
      seq([brc(L, "["), comma_sep, brc(R, "]")]),
    ]);

  let op_alt = ss => alt(List.map(op, ss));
  let add_op = op_alt(["+", "+.", "-", "-.", "@", "++"]);
  let mult_op = op_alt(["*", "*.", "/", "/."]);
  let neg_op = op_alt(["-", "-."]);

  let tbl = [
    //Math operations
    p(~a=L, seq([exp, add_op, exp])),
    p(~a=L, seq([exp, mult_op, exp])),
    p(seq([neg_op, exp])),
    //case
    p(seq([kw(~l=false, "case"), exp, rul, star(rul)])),
    //let
    p(
      seq([
        kw(~l=false, "let"),
        nt(Pat.sort),
        op("="),
        exp,
        kw("in", ~indent=false),
        exp,
      ]),
    ),
    //fun
    p(seq([kw(~l=false, "fun"), nt(Pat.sort), op("->"), exp])),
    //if
    p(seq([kw(~l=false, "if"), exp, kw("then"), exp, kw("else"), exp])),
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
