open Util;

module Sym = {
  include Sym;
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
let ch =
    (
      ~p=Padding.mk(
           ~h_l=true,
           ~h_r=true,
           ~v_l=false,
           ~v_r=false,
           ~indent=false,
           (),
         ),
      s,
    ) =>
  t(Label.const(~padding=p, s));
let cr =     (
      ~p=Padding.mk(
           ~h_l=false,
           ~h_r=true,
           ~v_l=false,
           ~v_r=false,
           ~indent=false,
           (),
         ),
      s,
    ) =>
  t(Label.const(~padding=p, s));

module Typ = {
  let sort = Sort.of_str("Typ");
  let typ = nt(sort);

  let comma_sep = seq([typ, Star(seq([c(","), typ]))]);

  let operand =
    alt([
      c("Int"),
      c("Float"),
      c("Bool"),
      c("String"),
      //List type
      seq([c("list"), c("("), typ, c(")")]),
      //Tuple type
      seq([c("("), comma_sep, c(")")]),
    ]);

  let tbl = [
    //Arrow
    //TODO: should the below be "ch" or c? (padding with horizontal spaces or none?)
    p(seq([typ, ch("->"), typ])),
    //Ap
    p(seq([typ, c("("), typ, c(")")])),
    p(operand),
  ];
};

module Pat = {
  let sort = Sort.of_str("Pat");
  let pat = nt(sort);

  let comma_sep = seq([pat, Star(seq([c(","), pat]))]);

  let bool_lit = alt([c("true"), c("false")]);
  let operand =
    alt([
      t(Int_lit),
      t(Float_lit),
      t(Id_lower),
      bool_lit,
      //Constructor
      t(Id_upper),
      seq([c("("), comma_sep, c(")")]),
      seq([c("["), comma_sep, c("]")]),
      //Wild
      c("_"),
    ]);

  let tbl = [
    //Typeann
    p(~a=L,seq([pat, c(":"), nt(Typ.sort)])), 
    //Cons
    p(~a=R, seq([pat, c("::"), pat])),
    //ap
    p(seq([pat, c("("), pat, c(")")])),
    p(operand),
  ];
};

module Exp = {
  let sort = Sort.of_str("Exp");
  let exp = nt(sort);

  [@warning "-32"]
  let comma_sep = seq([exp, Star(seq([c(","), exp]))]);

  let rul = seq([c("|"), nt(Pat.sort), c("=>"), exp]);
  let bool_lit = alt([c("true"), c("false")]);

  let operand =
    alt([
      t(Int_lit),
      t(Float_lit),
      t(Id_lower),
      bool_lit,
      seq([c("("), comma_sep, c(")")]),
      seq([c("["), comma_sep, c("]")]),
    ]);

  let tokc_alt = ss => alt(List.map(c, ss));
  let add_op = tokc_alt(["+", "+.", "-", "-.", "@", "++"]);
  let mult_op = tokc_alt(["*", "*.", "/", "/."]);
  let neg_op = tokc_alt(["-", "-."]);

  let tbl = [
    //Math operations
    p(~a=L, seq([exp, add_op, exp])),
    p(~a=L, seq([exp, mult_op, exp])),
    p(seq([neg_op, exp])),
    //case
    p(seq([c("case"), exp, rul, star(rul)])),
    //let
    p(seq([cr("let"), nt(Pat.sort), c("="), exp, c("in"), exp])),
    //fun
    p(seq([c("fun"), nt(Pat.sort), c("->"), exp])),
    //if
    p(seq([c("if"), exp, c("then"), exp, c("else"), exp])),
    //ap
    p(seq([exp, c("("), exp, c(")")])),
    p(operand),
  ];
};

type t = Sort.Map.t(Prec.Table.t(Regex.t));
let v =
  [Typ.(sort, tbl), Pat.(sort, tbl), Exp.(sort, tbl)]
  |> List.to_seq
  |> Sort.Map.of_seq;
