/*
     Notes about the javascript grammar

     Wherever js defines an opt(automatic_semicolon) we are requiring a semicolon

     "statement_identifier" never seems to be defined in the TreeSitter grammar so we are just ignoring it and assuming it is a normal ident
 */

//TODO: Look into writing an extension to the tylr grammar to do more of a ts precedence with a table of named values and each individual form associated with a particular named value - also with the nested arrays for separate precedence levels by form/sort?

module Sym = {
  include Sym;
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t = Sym.t(Label.t, (Filter.t, Sort.t));
};
module Regex = {
  include Regex;
  type t = Regex.t(Sym.t);
};
open Regex;

let p = (~a: option(Dir.t)=?, r: t) => (a, r);

let t = (lbl: Label.t) => Regex.atom(Sym.t(lbl));
let nt = (filter: Filter.t, srt: Sort.t) =>
  Regex.atom(Sym.nt((filter, srt)));

/*Indent flag is saying "do I indent next token if followed by a new line" ie
      static
          name = 1
      OR
      static
      name = 1
  */

let c = (~p=Padding.none, s) => t(Label.const(~padding=p, s));
let kw = (~space=(true, true), ~break=(false, false), ~indent=true) =>
  c(~p=Padding.kw(~space, ~break, ~indent, ()));
let op = (~space=(true, true), ~break=(false, false), ~indent=true) =>
  c(~p=Padding.op(~space, ~break, ~indent, ()));
let brc = (side: Dir.t) => c(~p=Padding.brc(side));
let tokop_alt = ss => alt(List.map(op, ss));

let comma_sep = (r: Regex.t) => seq([r, star(seq([c(","), r]))]);

// Top level generic forms
let assignment_pat = (exp: unit => Regex.t, pat: unit => Regex.t) =>
  seq([pat(), op("="), exp()]);

let private_property_ident = () => seq([c("#"), t(Id_lower)]);
let import = () => kw("import");
let param = (exp, pat) => alt([pat(), assignment_pat(exp, pat)]);
let params = (exp, pat) =>
  seq([
    brc(L, "("),
    param(exp, pat),
    star(seq([c(","), param(exp, pat)])),
    brc(R, ")"),
  ]);

//TODO
let property_name = () => alt([t(Id_lower)]);
let optional_chain = () => c("?.");

module type SORT = {
  let atom: (~filter: Filter.t=?, unit) => Regex.t;
  let sort: unit => Sort.t;
  let tbl: unit => Prec.Table.t(Regex.t);
};

module rec Pat: SORT = {
  let sort = () => Sort.of_str("Pat");
  let atom = (~filter=[], ()) => nt(filter, sort());

  let rest_pat = () => seq([c("..."), atom()]);

  let array_pat = () =>
    seq([
      brc(L, "["),
      comma_sep(opt(alt([atom(), assignment_pat(atom, Exp.atom)]))),
      brc(R, "]"),
    ]);

  let _pair_pat = () =>
    seq([
      property_name(),
      c(":"),
      alt([atom(), assignment_pat(Exp.atom, atom)]),
    ]);

  let obj_assignment_pat = () =>
    seq([
      alt([t(Id_lower), atom(~filter=["destruct_pat"], ())]),
      c("="),
      Exp.atom(),
    ]);

  let obj_pat = () =>
    seq([
      brc(L, "{"),
      comma_sep(
        alt([
          atom(~filter=["pair_pat", "rest_pat"], ()),
          obj_assignment_pat(),
        ]),
      ),
      brc(R, "}"),
    ]);

  let destruct_pat = () => alt([obj_pat(), array_pat()]);

  let lhs_exp = () =>
    alt([
      t(Id_lower),
      Exp.atom(~filter=["member_exp", "subscript_exp"], ()),
      destruct_pat(),
    ]);

  let tbl = () => [p(alt([lhs_exp(), rest_pat()]))];
}
and Exp: SORT = {
  let sort = () => Sort.of_str("Exp");
  let atom = (~filter=[], ()) => nt(filter, sort());

  let stat_block = () => seq([brc(L, "{"), Prog.atom(), brc(R, "}")]);

  let num = () => alt([t(Int_lit), t(Float_lit)]);

  let subscript_exp = () =>
    seq([
      alt([atom()]),
      opt(optional_chain()),
      brc(L, "["),
      atom(),
      brc(R, "]"),
    ]);

  //NOTE: for now we are making the primary same as exp atom - doing this to test our grammar designs on the assumption that treesitter separates them for precedence but our "global" exp precedence will work
  let primary_exp = () => atom();

  let pair = () => seq([t(Id_lower), c(":"), atom()]);
  let spread_element = () => seq([c("..."), atom()]);

  let arguments = () =>
    seq([
      brc(L, "("),
      alt([atom(), spread_element()]),
      star(seq([c(","), alt([atom(), spread_element()])])),
      brc(R, ")"),
    ]);

  let method_def = () =>
    seq([
      opt(kw(~space=(false, true), ~indent=false, "static")),
      opt(kw(~space=(false, true), ~indent=false, "async")),
      opt(
        alt([
          kw(~space=(false, true), ~indent=false, "get"),
          kw(~space=(false, true), ~indent=false, "set"),
          kw(~space=(false, true), ~indent=false, "*"),
        ]),
      ),
      t(Id_lower),
      params(atom, Pat.atom),
      stat_block(),
    ]);

  let obj_elements = () => alt([pair(), spread_element() /*method_def*/]);

  let obj = () =>
    seq([
      brc(L, "{"),
      obj_elements(),
      star(seq([c(","), obj_elements()])),
      brc(R, "}"),
    ]);

  let arr = () =>
    seq([
      brc(L, "["),
      opt(
        seq([
          alt([spread_element(), atom()]),
          star(seq([c(","), alt([spread_element(), atom()])])),
        ]),
      ),
      brc(R, "]"),
    ]);

  let call_signature = () => params(atom, Pat.atom);
  let func_exp = () =>
    seq([
      opt(kw(~space=(false, true), ~indent=false, "async")),
      kw(~space=(false, true), "function"),
      opt(t(Id_lower)),
      call_signature(),
      stat_block(),
    ]);

  let arrow_function = () =>
    seq([
      opt(kw(~space=(false, true), ~indent=false, "async")),
      alt([t(Id_lower), call_signature()]),
      op("=>"),
      alt([atom(), stat_block()]),
    ]);

  //async function*

  let generator_function = () =>
    seq([
      opt(kw(~space=(false, true), ~indent=false, "async")),
      c("function"),
      kw(~space=(false, true), "*"),
      opt(t(Id_lower)),
      call_signature(),
      stat_block(),
    ]);

  let _initializer = () => seq([op("="), atom()]);

  let field_def = () =>
    seq([
      opt(kw(~space=(false, true), "static")),
      property_name(),
      opt(_initializer()),
    ]);

  let class_static_block = () => seq([kw("static"), c(";"), stat_block()]);

  let class_heritage = () => seq([kw("extends"), atom()]);
  let class_body = () =>
    seq([
      brc(L, "{"),
      star(
        alt([
          seq([method_def(), c(";")]),
          seq([field_def(), c(";")]),
          class_static_block(),
          c(";"),
        ]),
      ),
      brc(R, "}"),
    ]);

  let _class = () =>
    seq([
      kw(~space=(false, true), "class"),
      opt(t(Id_lower)),
      opt(class_heritage()),
      class_body(),
    ]);

  //TODO: template strings
  let template_string = () => alt([]);

  let call_exp = () =>
    alt([
      seq([alt([atom(), import()]), alt([arguments(), template_string()])]),
      seq([primary_exp(), optional_chain(), arguments()]),
    ]);

  let paren_exp = () => seq([brc(L, "("), atom(), brc(R, ")")]);

  let primary_exp_list = () =>
    alt([
      t(Id_lower),
      c("this"),
      c("super"),
      num(),
      //TODO: string lit
      kw("true"),
      kw("false"),
      kw("null"),
      obj(),
      arr(),
      func_exp(),
      //NOTE: removed the arrow exp from the primary exp list bc they are not bi-delimited and seem
      // arrow_function,
      generator_function(),
      _class(),
      call_exp(),
      paren_exp(),
      subscript_exp(),
    ]);

  let member_exp = () =>
    seq([
      alt([atom(), import()]),
      alt([c("."), optional_chain()]),
      alt([private_property_ident(), t(Id_lower)]),
    ]);

  let assignment_exp = () =>
    seq([
      alt([paren_exp(), Pat.atom(~filter=["lhs_exp"], ())]),
      op("="),
      atom(),
    ]);
  let await_exp = () => seq([kw("await"), atom()]);
  let unary_exp = () =>
    seq([
      alt([
        kw("delete"),
        kw("void"),
        kw("typeof"),
        kw("+"),
        kw("-"),
        kw("~"),
        kw("!"),
      ]),
      atom(),
    ]);
  let update_exp = () =>
    alt([
      seq([atom(), alt([c("++"), c("--")])]),
      seq([alt([c("++"), c("--")]), atom()]),
    ]);

  let logical_ops = () => tokop_alt(["||", "&&"]);
  let binary_shift_ops = () => tokop_alt([">>", ">>>", "<<"]);
  let bitwise_ops = () => tokop_alt(["&", "|", "^"]);
  let binary_add_ops = () => tokop_alt(["+", "-"]);
  let binary_times_ops = () => tokop_alt(["*", "/", "%"]);
  let compare_ops = () =>
    tokop_alt(["<", ">", "<=", ">=", "instanceof", "in"]);
  let equality_ops = () => tokop_alt(["==", "!=", "===", "!=="]);

  let operand = () => alt([primary_exp_list()]);

  let binary_exp = () => [
    p(seq([atom(), logical_ops(), atom()])),
    p(seq([atom(), binary_shift_ops(), atom()])),
    p(seq([atom(), bitwise_ops(), atom()])),
    p(seq([atom(), binary_add_ops(), atom()])),
    p(seq([atom(), binary_times_ops(), atom()])),
    p(seq([atom(), compare_ops(), atom()])),
    p(seq([atom(), equality_ops(), atom()])),
    p(seq([atom(), op("??"), atom()])),
  ];

  let ternary_exp = () => seq([atom(), c("?"), atom(), c(":"), atom()]);

  let new_exp = () => seq([kw("new"), primary_exp(), opt(arguments())]);
  let yield_exp = () =>
    seq([kw("yield"), alt([seq([c("*"), atom()]), opt(atom())])]);

  //Low -> high prec
  let tbl = () =>
    [p(arrow_function())]
    @ binary_exp()
    @ [
      p(ternary_exp()),
      p(assignment_exp()),
      p(await_exp()),
      p(unary_exp()),
      p(update_exp()),
      p(new_exp()),
      p(yield_exp()),
      p(member_exp()),
    ]
    @ [p(operand())];
}
and Stat: SORT = {
  let sort = () => Sort.of_str("Stat");
  let atom = (~filter=[], ()) => nt(filter, sort());
  let paren_exp = () => seq([brc(L, "("), Exp.atom(), brc(R, ")")]);

  let stat_block = () =>
    seq([brc(L, "{"), star(seq([atom(), c(";")])), brc(R, "}")]);

  let method_def = () =>
    seq([
      opt(kw(~space=(false, true), ~indent=false, "static")),
      opt(kw(~space=(false, true), ~indent=false, "async")),
      opt(
        alt([
          kw(~space=(false, true), ~indent=false, "get"),
          kw(~space=(false, true), ~indent=false, "set"),
          kw(~space=(false, true), ~indent=false, "*"),
        ]),
      ),
      t(Id_lower),
      params(Exp.atom, Pat.atom),
      stat_block(),
    ]);
  let call_signature = () => params(Exp.atom, Pat.atom);

  let module_export_name = () => alt([t(Id_lower) /* , t(String_lit) */]);
  let export_specifier = () =>
    seq([
      module_export_name(),
      opt(seq([kw("as"), module_export_name()])),
    ]);
  let export_clause = () =>
    seq([
      brc(L, "{"),
      export_specifier(),
      star(seq([c(","), export_specifier()])),
      opt(c(",")),
      brc(R, "}"),
    ]);

  //TODO:
  // let from_clause =seq([kw("from"), t(String_lit)])
  let from_clause = () => seq([kw("from"), t(Id_lower)]);
  let namespace_export = () =>
    seq([c("*"), kw("as"), module_export_name()]);

  let export_statement = () =>
    alt([
      seq([
        kw("export"),
        alt([
          seq([c("*"), from_clause()]),
          seq([namespace_export(), from_clause()]),
          seq([export_clause(), from_clause()]),
          export_clause(),
        ]),
        c(";"),
      ]),
      seq([kw("export"), atom()]),
    ]);

  let namespace_import = () => seq([c("*"), kw("as"), t(Id_lower)]);
  let import_specifier = () =>
    alt([
      t(Id_lower),
      seq([module_export_name(), kw("as"), t(Id_lower)]),
    ]);
  let named_imports = () =>
    seq([
      brc(L, "{"),
      import_specifier(),
      star(seq([c(","), import_specifier()])),
      brc(R, "}"),
    ]);
  let import_clause = () =>
    alt([
      namespace_import(),
      named_imports(),
      seq([
        t(Id_lower),
        opt(seq([c(","), alt([namespace_import(), named_imports()])])),
      ]),
    ]);
  let import_statement = () =>
    seq([
      kw("import"),
      alt([seq([import_clause(), from_clause()]) /* , t(String) */]),
      c(";"),
    ]);

  let func_declaration = () =>
    seq([
      opt(kw(~space=(false, true), ~indent=false, "async")),
      kw(~space=(false, true), "function"),
      opt(t(Id_lower)),
      call_signature(),
      stat_block(),
      opt(c(";")),
    ]);

  let property_name = () => alt([t(Id_lower)]);
  let _initializer = () => seq([op("="), Exp.atom()]);
  let field_def = () =>
    seq([
      opt(kw(~space=(false, true), "static")),
      property_name(),
      opt(_initializer()),
    ]);

  let class_static_block = () => seq([kw("static"), c(";"), stat_block()]);
  let class_heritage = () => seq([kw("extends"), Exp.atom()]);
  let class_body = () =>
    seq([
      brc(L, "{"),
      star(
        alt([
          seq([method_def(), c(";")]),
          seq([field_def(), c(";")]),
          class_static_block(),
          c(";"),
        ]),
      ),
      brc(R, "}"),
    ]);

  let class_declaration = () =>
    seq([
      kw(~space=(false, true), "class"),
      opt(t(Id_lower)),
      opt(class_heritage()),
      class_body(),
    ]);

  let init = () => seq([op("="), Exp.atom()]);
  let var_declarator = () => seq([t(Id_lower), opt(init())]);

  let lexical_declaration = () =>
    seq([alt([kw("let"), kw("const")]), comma_sep(var_declarator())]);

  let var_declaration = () => seq([kw("var"), comma_sep(var_declarator())]);

  let declaration = () =>
    alt([
      func_declaration(),
      class_declaration(),
      lexical_declaration(),
      var_declaration(),
    ]);

  let switch_case = () =>
    seq([kw("case"), Exp.atom(), c(":"), star(seq([atom(), c(";")]))]);
  let switch_default = () =>
    seq([kw("default"), c(":"), star(seq([atom(), c(";")]))]);
  let switch_body = () =>
    seq([
      brc(L, "{"),
      star(alt([switch_case(), switch_default()])),
      brc(R, "}"),
    ]);
  let switch_statement = () =>
    seq([kw("switch"), paren_exp(), switch_body()]);

  let debugger_statement = () => seq([kw("debugger")]);

  let empty_statement = () => c(";");

  let break_statement = () => seq([kw("break"), opt(t(Id_lower))]);

  let continue_statement = () => seq([kw("continue"), opt(t(Id_lower))]);

  let return_statement = () =>
    seq([kw("return"), opt(Exp.atom()), c(";")]);

  let throw_statement = () => seq([kw("throw"), Exp.atom(), c(";")]);

  let exp_statement = () => Exp.atom();

  let do_statement = () =>
    seq([kw("do"), atom(), kw("while"), paren_exp()]);

  let operand = () =>
    alt([
      continue_statement(),
      do_statement(),
      break_statement(),
      return_statement(),
      throw_statement(),
      empty_statement(),
      debugger_statement(),
      export_statement(),
      import_statement(),
      declaration(),
      stat_block(),
      switch_statement(),
    ]);

  let for_statement = () =>
    seq([
      kw("for"),
      brc(L, "("),
      alt([
        seq([lexical_declaration(), c(";")]),
        seq([var_declaration(), c(";")]),
        //NOTE: manually resolve problem with exp statement needing trailing semicolon
        seq([exp_statement(), c(";")]),
        empty_statement(),
      ]),
      alt([seq([exp_statement(), c(";")]), empty_statement()]),
      opt(Exp.atom()),
      brc(R, ")"),
      atom(),
    ]);

  let for_header = () =>
    seq([
      brc(L, "("),
      alt([
        alt([Pat.atom(~filter=["lhs_exp"], ()), paren_exp()]),
        seq([
          kw("var"),
          alt([t(Id_lower), Pat.atom(~filter=["destruct_pat"], ())]),
          opt(init()),
        ]),
        seq([
          alt([kw("let"), kw("const")]),
          alt([t(Id_lower), Pat.atom(~filter=["destruct_pat"], ())]),
        ]),
      ]),
      alt([kw("in"), kw("of")]),
      Exp.atom(),
      brc(R, ")"),
    ]);

  let for_in_statement = () =>
    seq([kw("for"), opt(kw("await")), for_header(), atom()]);

  let else_clause = () => seq([kw("else"), atom()]);
  let if_statement = () =>
    seq([kw("if"), paren_exp(), atom(), opt(else_clause())]);

  let while_statement = () => seq([kw("while"), paren_exp(), atom()]);

  let catch_clause = () =>
    seq([
      kw("catch"),
      opt(
        seq([
          brc(L, "("),
          alt([t(Id_lower), Pat.atom(~filter=["destruct_pat"], ())]),
          brc(R, ")"),
        ]),
      ),
      stat_block(),
    ]);
  let finally_clause = () => seq([kw("finally"), stat_block()]);
  let try_statement = () =>
    seq([
      kw("try"),
      stat_block(),
      opt(catch_clause()),
      opt(finally_clause()),
    ]);

  let with_statement = () => seq([kw("with"), paren_exp(), atom()]);

  let label_statement = () => seq([t(Id_lower), c(":"), atom()]);

  let tbl = () => [
    p(if_statement()),
    p(for_statement()),
    p(for_in_statement()),
    p(while_statement()),
    p(try_statement()),
    p(with_statement()),
    p(label_statement()),
    p(exp_statement()),
    p(operand()),
  ];
}
and Prog: SORT = {
  let sort = () => Sort.of_str("Prog");
  let atom = (~filter=[], ()) => nt(filter, sort());

  let tbl = () => [p(star(seq([Stat.atom(), c(";")])))];
};

type t = Sort.Map.t(Prec.Table.t(Regex.t));
let v =
  [
    Pat.(sort(), tbl()),
    Stat.(sort(), tbl()),
    Exp.(sort(), tbl()),
    Prog.(sort(), tbl()),
  ]
  |> List.to_seq
  |> Sort.Map.of_seq;
