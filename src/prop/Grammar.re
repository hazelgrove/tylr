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

module Stat = {
  let sort = Sort.of_str("Stat");
  let stat = nt(sort);

  [@warning "-32"]
  let comma_sep = seq([stat, Star(seq([c(","), stat]))]);

  let operand = alt([t(Id_lower), t(Id_upper)]);

  let tokc_alt = ss => alt(List.map(c, ss));
  //AND, equals, implies, NAND, nonequals, NOR, NOT, OR, XNOR, XOR
  let prop_ops =
    tokc_alt([
      "AND",
      "==",
      "->",
      "NAND",
      "!=",
      "NOR",
      "NOT",
      "OR",
      "XNOR",
      "XOR",
    ]);

  let tbl = [p(seq([stat, prop_ops, stat])), p(operand)];
};

type t = Sort.Map.t(Prec.Table.t(Regex.t));
let v = [Stat.(sort, tbl)] |> List.to_seq |> Sort.Map.of_seq;
