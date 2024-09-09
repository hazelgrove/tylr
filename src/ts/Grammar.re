/*
     Notes about the typescript grammar

     Wherever TS defines an opt(automatic_semicolon) we are requiring a semicolon

     The pat sort is just an lhs_exp. To get a true pat you must use the top level pat and pass in a Pat.atom() as the argument. This was done due to recursion conflicts within pat/lhs_exp

     "statement_identifier" never seems to be defined in the TreeSitter grammar so we are just ignoring it and assuming it is a normal ident
 */

//TODO: Look into writing an extension to the tylr grammar to do more of a ts precedence with a table of named values and each individual form associated with a particular named value - also with the nested arrays for separate precedence levels by form/sort?

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

/*Indent flag is saying "do I indent next token if followed by a new line" ie
      static
          name = 1
      OR
      static
      name = 1
  */

let c = (~p=Padding.none, s) => t(Label.const(~padding=p, s));
let kw = (~l=true, ~r=true, ~indent=true) =>
  c(~p=Padding.kw(~l, ~r, ~indent, ()));
let op = (~l=true, ~r=true, ~indent=true) =>
  c(~p=Padding.op(~l, ~r, ~indent, ()));
let brc = (side: Dir.t) => c(~p=Padding.brc(side));

//let tokc_alt = ss => alt(List.map(c, ss));
let tokop_alt = ss => alt(List.map(op, ss));

let comma_sep = (r: Regex.t) => seq([r, star(seq([c(","), r]))]);

let pat = atom => seq([opt(c("...")), atom()]);
let rest_pat = atom => seq([c("..."), atom()]);

let private_property_ident = seq([c("#"), t(Id_lower)]);
let import = kw("import");

//TODO
let property_name = alt([t(Id_lower)]);
let optional_chain = c("?.");

let member_exp = exp =>
  seq([
    alt([exp(), import]),
    alt([c("."), optional_chain]),
    alt([private_property_ident, t(Id_lower)]),
  ]);

let subscript_exp = exp =>
  seq([
    alt([exp()]),
    opt(optional_chain),
    brc(L, "["),
    exp(),
    brc(R, "]"),
  ]);

let assignment_pat = (exp: unit => Regex.t, pat: unit => Regex.t) =>
  seq([pat(), op("="), exp()]);
let array_pat = (exp, pat) =>
  seq([
    brc(L, "["),
    comma_sep(opt(alt([pat(), assignment_pat(pat, exp)]))),
    brc(R, "]"),
  ]);

let destruct_pat =
    (exp: unit => Regex.t, pat: unit => Regex.t, obj_pat: unit => Regex.t) =>
  alt([obj_pat(), array_pat(exp, pat)]);

//Export statement used both in statement and in the typ object typ
let module_export_name = alt([t(Id_lower) /* , t(String_lit) */]);
let export_specifier =
  seq([module_export_name, opt(seq([kw("as"), module_export_name]))]);
let export_clause =
  seq([
    brc(L, "{"),
    export_specifier,
    star(seq([c(","), export_specifier])),
    opt(c(",")),
    brc(R, "}"),
  ]);

//TODO:
// let from_clause =seq([kw("from"), t(String_lit)])
let from_clause = seq([kw("from"), t(Id_lower)]);
let namespace_export = seq([c("*"), kw("as"), module_export_name]);

let export_statement = stat =>
  alt([
    seq([
      kw("export"),
      alt([
        seq([c("*"), from_clause]),
        seq([namespace_export, from_clause]),
        seq([export_clause, from_clause]),
        export_clause,
      ]),
      c(";"),
    ]),
    seq([kw("export"), stat()]),
  ]);

//Used in exp and typ
let param = lhs_exp => alt([pat(lhs_exp) /*, assignnment_pat*/]);
let params = lhs_exp =>
  seq([
    brc(L, "("),
    param(lhs_exp),
    star(seq([c(","), param(lhs_exp)])),
    brc(R, ")"),
  ]);

//This is an example of something that should be of the exp sort but is duplicated
let _initializer = exp => seq([op("="), exp()]);

let number = alt([t(Int_lit), t(Float_lit)]);

let typ_ident = t(Id_lower);
let _constraint = typ => seq([alt([kw("extends"), c(":")]), typ()]);
let default_typ = typ => seq([op("="), typ()]);
let typ_parameter = typ =>
  seq([
    opt(kw("const")),
    typ_ident,
    opt(_constraint(typ)),
    opt(default_typ(typ)),
  ]);
let typ_params = typ =>
  seq([brc(L, "<"), comma_sep(typ_parameter(typ)), brc(R, ">")]);

module Filter = {
  //Whitelisted strings
  type t = list(string);
};

module type SORT = {
  // let atom: (~filter: Filter.t = ?, unit) => Regex.t;
  let atom: unit => Regex.t;
  let sort: unit => Sort.t;
  let tbl: unit => Prec.Table.t(Regex.t);
  //Define the form whitelist & blacklisting functions here that construct a filter
  // let whitelist: ()
};

module rec Typ: SORT = {
  let sort = () => Sort.of_str("Typ");
  let atom = () => nt(sort());

  let function_typ = seq([typ_params(Typ.atom)]);

  let tbl = () => [
    p(PrimaryTyp.atom()),
    p(function_typ),
    // p(readonly_typ),
    // p(constructor_typ),
    // p(infer_typ),
  ];
}
and PrimaryTyp: SORT = {
  let sort = () => Sort.of_str("PrimaryTyp");
  let atom = () => nt(sort());

  let paren_type = seq([brc(L, "("), Typ.atom(), brc(R, ")")]);
  let predefined_type =
    alt([
      kw("any"),
      kw("number"),
      kw("boolean"),
      kw("string"),
      kw("symbol"),
      kw("void"),
      kw("unknown"),
      kw("null"),
      kw("never"),
      kw("object"),
    ]);

  let nested_ident =
    seq([
      star(seq([member_exp(Exp.atom), c("."), t(Id_lower)])),
      c("."),
      t(Id_lower),
    ]);
  let nested_typ_ident =
    seq([alt([t(Id_lower), nested_ident]), c("."), typ_ident]);

  let typ_arguments =
    seq([brc(L, "<"), comma_sep(Typ.atom()), brc(R, ">")]);
  let generic_typ = seq([alt([typ_ident, nested_typ_ident]), typ_arguments]);

  let typ_annotation = seq([c(":"), Typ.atom()]);
  let omitting_typ_annotation = seq([c("-?:"), Typ.atom()]);
  let adding_typ_annotation = seq([c("+?:"), Typ.atom()]);
  let opting_typ_annotation = seq([c("?:"), Typ.atom()]);

  let accessibility_modifier =
    alt([kw("public"), kw("protected"), kw("private")]);
  let override_modifier = kw("override");
  let property_signature =
    seq([
      opt(accessibility_modifier),
      opt(kw("static")),
      opt(override_modifier),
      opt(kw("readonly")),
      property_name,
      opt(c("?")),
      opt(typ_annotation),
    ]);

  let this = kw("this");
  let typ_predicate =
    seq([alt([t(Id_lower), this]), kw("is"), Typ.atom()]);
  let typ_predicate_annotation = seq([c(":"), typ_predicate]);

  let asserts_annotation =
    seq([c(":"), kw("asserts"), alt([typ_predicate, t(Id_lower), this])]);
  let call_signature =
    seq([
      opt(typ_params(Typ.atom)),
      params(LHSExp.atom),
      opt(
        alt([typ_annotation, asserts_annotation, typ_predicate_annotation]),
      ),
    ]);

  let construct_signature =
    seq([
      opt(kw("abstract")),
      kw("new"),
      opt(typ_params(Typ.atom)),
      params(LHSExp.atom),
      opt(typ_annotation),
    ]);
  let index_signature =
    seq([
      opt(seq([opt(alt([c("-"), c("+")])), kw("readonly")])),
      brc(L, "["),
      alt([seq([t(Id_lower), c(":"), Typ.atom()])]),
      brc(R, "]"),
      alt([
        typ_annotation,
        omitting_typ_annotation,
        adding_typ_annotation,
        opting_typ_annotation,
      ]),
    ]);

  let method_signature =
    seq([
      opt(accessibility_modifier),
      opt(kw("static")),
      opt(override_modifier),
      opt(kw("readonly")),
      opt(kw("async")),
      opt(alt([kw("get"), kw("set"), c("*")])),
      property_name,
      opt(c("?")),
      call_signature,
    ]);

  let obj_typ =
    seq([
      alt([brc(L, "{"), brc(L, "{|")]),
      opt(
        seq([
          c(","),
          comma_sep(
            alt([
              export_statement(Stat.atom),
              property_signature,
              call_signature,
              construct_signature,
              index_signature,
              method_signature,
            ]),
          ),
          c(","),
        ]),
      ),
      alt([brc(R, "}"), brc(R, "|}")]),
    ]);

  let arr_typ = seq([PrimaryTyp.atom(), c("[]")]);

  let param_name =
    seq([
      opt(accessibility_modifier),
      opt(override_modifier),
      opt(kw("readonly")),
      alt([pat(LHSExp.atom), this]),
    ]);
  let required_param =
    seq([param_name, opt(typ_annotation), opt(_initializer(Exp.atom))]);
  let optional_param =
    seq([
      param_name,
      c("?"),
      opt(typ_annotation),
      opt(_initializer(Exp.atom)),
    ]);
  let optional_typ = seq([Typ.atom(), c("?")]);
  let rest_typ = seq([c("..."), Typ.atom()]);

  let tuple_typ_member =
    alt([required_param, optional_param, optional_typ, rest_typ, Typ.atom()]);
  let tuple_typ =
    seq([brc(L, "("), comma_sep(tuple_typ_member), brc(R, ")")]);

  let flow_maybe_typ = seq([c("?"), PrimaryTyp.atom()]);

  //NOTE: the treesitter grammar for this is weirdly restrictive and (extremely) self-recursive. Due to this, I'm just allowing any exp to go here (as that makes more sense?)
  let typ_query = seq([kw("typeof"), Exp.atom()]);

  let index_typ_query = seq([kw("keyof"), PrimaryTyp.atom()]);

  let existential_typ = kw("*");

  let literal_typ =
    alt([
      number,
      //TODO:
      // t(String_lit),
      kw("true"),
      kw("false"),
      kw("null"),
      kw("undefined"),
    ]);

  let const = kw("const");

  // let template_chars = t(Template_chars);
  //NOTE: template chars are: ` | \0 | ${} | \\
  let template_chars = alt([c("`"), c("\\0"), c("${}"), c("\\\\")]);
  let infer_typ =
    seq([kw("infer"), typ_ident, opt(seq([kw("extends"), Typ.atom()]))]);
  let template_typ =
    seq([c("${"), alt([PrimaryTyp.atom(), infer_typ]), c("}")]);
  //TODO: ask david - should this be brc?
  let template_literal_typ =
    seq([
      brc(L, "`"),
      star(alt([template_chars, template_typ])),
      brc(R, "`"),
    ]);

  let operand =
    alt([
      paren_type,
      predefined_type,
      typ_ident,
      nested_typ_ident,
      generic_typ,
      obj_typ,
      arr_typ,
      tuple_typ,
      this,
      existential_typ,
      literal_typ,
      template_literal_typ,
      const,
    ]);

  let lookup_typ =
    seq([PrimaryTyp.atom(), brc(L, "["), Typ.atom(), brc(R, "]")]);
  let conditional_typ =
    seq([
      Typ.atom(),
      kw("extends"),
      Typ.atom(),
      c("?"),
      Typ.atom(),
      c(":"),
      Typ.atom(),
    ]);
  let intersection_typ = seq([opt(Typ.atom()), c("&"), Typ.atom()]);
  let union_typ = seq([opt(Typ.atom()), c("|"), Typ.atom()]);

  let tbl = () => [
    p(flow_maybe_typ),
    p(typ_query),
    p(index_typ_query),
    p(lookup_typ),
    p(conditional_typ),
    p(intersection_typ),
    p(union_typ),
    p(operand),
  ];
}
and ObjectPat: SORT = {
  let sort = () => Sort.of_str("ObjectPat");
  let atom = () => nt(sort());

  let pair_pat =
    seq([
      property_name,
      c(":"),
      alt([
        pat(LHSExp.atom),
        assignment_pat(Exp.atom, () => pat(LHSExp.atom)),
      ]),
    ]);

  let obj_assignmnet_pat =
    seq([
      alt([t(Id_lower)]),
      c("="),
      Exp.atom(),
      destruct_pat(Exp.atom, ObjectPat.atom, () => pat(LHSExp.atom)),
    ]);

  let obj_pat =
    seq([
      brc(L, "{"),
      comma_sep(
        alt([
          pair_pat,
          rest_pat(() => pat(LHSExp.atom)),
          obj_assignmnet_pat,
        ]),
      ),
      brc(R, "}"),
    ]);

  let operand = alt([obj_pat]);

  let tbl = () => [p(operand)];
}
and LHSExp: SORT = {
  let sort = () => Sort.of_str("Pat");
  let atom = () => nt(sort());

  let lhs_exp =
    alt([
      t(Id_lower),
      member_exp(Exp.atom),
      subscript_exp(Exp.atom),
      destruct_pat(Exp.atom, ObjectPat.atom, LHSExp.atom),
    ]);

  let pat = lhs_exp;

  let tbl = () => [p(pat)];
}
and Exp: SORT = {
  let sort = () => Sort.of_str("Exp");
  let atom = () => nt(sort());

  let stat_block =
    seq([brc(L, "{"), star(Stat.atom()), c(";"), brc(R, "}")]);

  let num = alt([t(Int_lit), t(Float_lit)]);

  //NOTE: for now we are making the primary same as exp atom - doing this to test our grammar designs on the assumption that treesitter separates them for precedence but our "global" exp precedence will work
  let primary_exp = atom();

  let pair = seq([t(Id_lower), c(":"), atom()]);
  let spread_element = seq([c("..."), atom()]);

  let arguments =
    seq([
      brc(L, "("),
      alt([atom(), spread_element]),
      star(seq([c(","), alt([atom(), spread_element])])),
      brc(R, ")"),
    ]);

  let member_exp = member_exp(atom);

  //TODO:assignment_pat

  let method_def =
    seq([
      opt(kw(~l=false, ~indent=false, "static")),
      opt(kw(~l=false, ~indent=false, "async")),
      opt(
        alt([
          kw(~l=false, ~indent=false, "get"),
          kw(~l=false, ~indent=false, "set"),
          kw(~l=false, ~indent=false, "*"),
        ]),
      ),
      t(Id_lower),
      params(LHSExp.atom),
      stat_block,
    ]);

  let obj_elements = alt([pair, spread_element /*method_def*/]);

  let obj =
    seq([
      brc(L, "{"),
      obj_elements,
      star(seq([c(","), obj_elements])),
      brc(R, "}"),
    ]);

  let arr =
    seq([
      brc(L, "["),
      opt(
        seq([
          alt([spread_element, atom()]),
          star(seq([c(","), alt([spread_element, atom()])])),
        ]),
      ),
      brc(R, "]"),
    ]);

  let call_signature = params;
  let func_exp =
    seq([
      opt(kw(~l=false, ~indent=false, "async")),
      kw(~l=false, "function"),
      opt(t(Id_lower)),
      call_signature(LHSExp.atom),
      stat_block,
    ]);

  let arrow_function =
    seq([
      opt(kw(~l=false, ~indent=false, "async")),
      alt([t(Id_lower), call_signature(LHSExp.atom)]),
      op("=>"),
      alt([atom(), stat_block]),
    ]);

  //async function*

  let generator_function =
    seq([
      opt(kw(~l=false, ~indent=false, "async")),
      c("function"),
      kw(~l=false, "*"),
      opt(t(Id_lower)),
      call_signature(LHSExp.atom),
      stat_block,
    ]);

  let field_def =
    seq([
      opt(kw(~l=false, "static")),
      property_name,
      opt(_initializer(Exp.atom)),
    ]);

  let class_static_block = seq([kw("static"), c(";"), stat_block]);

  let class_heritage = seq([kw("extends"), atom()]);
  let class_body =
    seq([
      brc(L, "{"),
      star(
        alt([
          seq([method_def, c(";")]),
          seq([field_def, c(";")]),
          class_static_block,
          c(";"),
        ]),
      ),
      brc(R, "}"),
    ]);

  let _class =
    seq([
      kw(~l=false, "class"),
      opt(t(Id_lower)),
      opt(class_heritage),
      class_body,
    ]);

  //TODO: template strings
  let template_string = alt([]);

  let call_exp =
    alt([
      seq([alt([atom(), import]), alt([arguments, template_string])]),
      seq([primary_exp, optional_chain, arguments]),
    ]);

  let paren_exp = seq([brc(L, "("), atom(), brc(R, ")")]);

  let primary_exp_list =
    alt([
      t(Id_lower),
      c("this"),
      c("super"),
      num,
      //TODO: string lit
      kw("true"),
      kw("false"),
      kw("null"),
      obj,
      arr,
      func_exp,
      //NOTE: removed the arrow exp from the primary exp list bc they are not bi-delimited and seem
      // arrow_function,
      generator_function,
      _class,
      call_exp,
      paren_exp,
      member_exp,
    ]);

  //End of "primary" expressions
  let assignment_exp =
    seq([alt([paren_exp, LHSExp.atom()]), op("="), atom()]);
  let await_exp = seq([kw("await"), atom()]);
  let unary_exp =
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
  let update_exp =
    alt([
      seq([atom(), alt([c("++"), c("--")])]),
      seq([alt([c("++"), c("--")]), atom()]),
    ]);

  let logical_ops = tokop_alt(["||", "&&"]);
  let binary_shift_ops = tokop_alt([">>", ">>>", "<<"]);
  let bitwise_ops = tokop_alt(["&", "|", "^"]);
  let binary_add_ops = tokop_alt(["+", "-"]);
  let binary_times_ops = tokop_alt(["*", "/", "%"]);
  let compare_ops = tokop_alt(["<", ">", "<=", ">=", "instanceof", "in"]);
  let equality_ops = tokop_alt(["==", "!=", "===", "!=="]);

  let operand = alt([primary_exp_list]);

  let binary_exp = [
    p(seq([atom(), logical_ops, atom()])),
    p(seq([atom(), binary_shift_ops, atom()])),
    p(seq([atom(), bitwise_ops, atom()])),
    p(seq([atom(), binary_add_ops, atom()])),
    p(seq([atom(), binary_times_ops, atom()])),
    p(seq([atom(), compare_ops, atom()])),
    p(seq([atom(), equality_ops, atom()])),
    p(seq([atom(), op("??"), atom()])),
  ];

  let ternary_exp = seq([atom(), c("?"), atom(), c(":"), atom()]);

  let new_exp = seq([kw("new"), primary_exp, opt(arguments)]);
  let yield_exp =
    seq([kw("yield"), alt([seq([c("*"), atom()]), opt(atom())])]);

  //Low -> high prec
  let tbl = () =>
    [p(arrow_function)]
    @ binary_exp
    @ [
      p(ternary_exp),
      p(assignment_exp),
      p(await_exp),
      p(unary_exp),
      p(update_exp),
      p(new_exp),
      p(yield_exp),
    ]
    @ [p(operand)];
}
and Stat: SORT = {
  let sort = () => Sort.of_str("Stat");
  let atom = () => nt(sort());

  let paren_exp = seq([brc(L, "("), Exp.atom(), brc(R, ")")]);

  let stat_block =
    seq([brc(L, "{"), star(Stat.atom()), c(";"), brc(R, "}")]);

  let param = alt([pat(LHSExp.atom) /*, assignment_pat*/]);
  let params =
    seq([brc(L, "("), param, star(seq([c(","), param])), brc(R, ")")]);
  let method_def =
    seq([
      opt(kw(~l=false, ~indent=false, "static")),
      opt(kw(~l=false, ~indent=false, "async")),
      opt(
        alt([
          kw(~l=false, ~indent=false, "get"),
          kw(~l=false, ~indent=false, "set"),
          kw(~l=false, ~indent=false, "*"),
        ]),
      ),
      t(Id_lower),
      params,
      stat_block,
    ]);
  let call_signature = params;

  let namespace_import = seq([c("*"), kw("as"), t(Id_lower)]);
  let import_specifier =
    alt([t(Id_lower), seq([module_export_name, kw("as"), t(Id_lower)])]);
  let named_imports =
    seq([
      brc(L, "{"),
      import_specifier,
      star(seq([c(","), import_specifier])),
      brc(R, "}"),
    ]);
  let import_clause =
    alt([
      namespace_import,
      named_imports,
      seq([
        t(Id_lower),
        opt(seq([c(","), alt([namespace_import, named_imports])])),
      ]),
    ]);
  let import_statement =
    seq([
      kw("import"),
      alt([seq([import_clause, from_clause]) /* , t(String) */]),
      c(";"),
    ]);

  let exp_statement = seq([Exp.atom(), c(";")]);

  let func_declaration =
    seq([
      opt(kw(~l=false, ~indent=false, "async")),
      kw(~l=false, "function"),
      opt(t(Id_lower)),
      //TODO: raise call_signature and stat block to the top level
      call_signature,
      stat_block,
      opt(c(";")),
    ]);

  let property_name = alt([t(Id_lower)]);
  let _initializer = seq([op("="), Exp.atom()]);
  let field_def =
    seq([opt(kw(~l=false, "static")), property_name, opt(_initializer)]);

  let class_static_block = seq([kw("static"), c(";"), stat_block]);
  let class_heritage = seq([kw("extends"), Exp.atom()]);
  let class_body =
    seq([
      brc(L, "{"),
      star(
        alt([
          seq([method_def, c(";")]),
          seq([field_def, c(";")]),
          class_static_block,
          c(";"),
        ]),
      ),
      brc(R, "}"),
    ]);

  let class_declaration =
    seq([
      kw(~l=false, "class"),
      opt(t(Id_lower)),
      opt(class_heritage),
      class_body,
    ]);

  let init = seq([op("="), Exp.atom()]);
  let var_declarator = seq([t(Id_lower), opt(init)]);

  let lexical_declaration =
    seq([
      alt([kw("let"), kw("const")]),
      comma_sep(var_declarator),
      c(";"),
    ]);

  let var_declaration =
    seq([kw("var"), comma_sep(var_declarator), c(";")]);

  //TODO: generator func decl
  let declaration =
    alt([
      func_declaration,
      class_declaration,
      lexical_declaration,
      var_declaration,
    ]);

  let statement_block =
    seq([brc(L, "{"), star(atom()), brc(R, "}"), opt(c(";"))]);

  let switch_case =
    seq([kw("case"), Exp.atom(), c(":"), star(Stat.atom())]);
  let switch_default = seq([kw("default"), c(":"), star(Stat.atom())]);
  let switch_body =
    seq([
      brc(L, "{"),
      star(alt([switch_case, switch_default])),
      brc(R, "}"),
    ]);
  let switch_statement = seq([kw("switch"), paren_exp, switch_body]);

  let debugger_statement = seq([kw("debugger"), c(";")]);

  let empty_statement = c(";");

  let operand =
    alt([
      empty_statement,
      debugger_statement,
      export_statement(Stat.atom),
      import_statement,
      declaration,
      statement_block,
      switch_statement,
    ]);

  let for_statement =
    seq([
      kw("for"),
      brc(L, "("),
      alt([
        lexical_declaration,
        var_declaration,
        exp_statement,
        empty_statement,
      ]),
      alt([exp_statement, empty_statement]),
      opt(Exp.atom()),
      brc(R, ")"),
      Stat.atom(),
    ]);

  let for_header =
    seq([
      brc(L, "("),
      alt([
        alt([LHSExp.atom(), paren_exp]),
        seq([
          kw("var"),
          alt([
            t(Id_lower),
            destruct_pat(Exp.atom, ObjectPat.atom, () => pat(LHSExp.atom)),
          ]),
          opt(init),
        ]),
        seq([
          alt([kw("let"), kw("const")]),
          alt([
            t(Id_lower),
            destruct_pat(Exp.atom, ObjectPat.atom, () => pat(LHSExp.atom)),
          ]),
        ]),
      ]),
      alt([kw("in"), kw("of")]),
      Exp.atom(),
      brc(R, ")"),
    ]);

  let for_in_statement =
    seq([kw("for"), opt(kw("await")), for_header, Stat.atom()]);

  let else_clause = seq([kw("else"), Stat.atom()]);
  let if_statement =
    seq([kw("if"), paren_exp, Stat.atom(), opt(else_clause)]);

  let while_statement = seq([kw("while"), paren_exp, Stat.atom()]);

  let do_statement =
    seq([kw("do"), Stat.atom(), kw("while"), paren_exp, c(";")]);

  let catch_clause =
    seq([
      kw("catch"),
      opt(
        seq([
          brc(L, "("),
          alt([
            t(Id_lower),
            destruct_pat(Exp.atom, ObjectPat.atom, () => pat(LHSExp.atom)),
          ]),
          brc(R, ")"),
        ]),
      ),
      stat_block,
    ]);
  let finally_clause = seq([kw("finally"), stat_block]);
  let try_statement =
    seq([kw("try"), stat_block, opt(catch_clause), opt(finally_clause)]);

  let with_statement = seq([kw("with"), paren_exp, Stat.atom()]);

  let break_statement = seq([kw("break"), opt(t(Id_lower)), c(";")]);

  let continue_statement =
    seq([kw("continue"), opt(t(Id_lower)), c(";")]);

  let return_statement = seq([kw("return"), opt(Exp.atom()), c(";")]);

  let throw_statement = seq([kw("throw"), Exp.atom(), c(";")]);

  let label_statement = seq([t(Id_lower), c(":"), Stat.atom()]);

  let tbl = () => [
    p(exp_statement),
    p(if_statement),
    p(for_statement),
    p(for_in_statement),
    p(while_statement),
    p(do_statement),
    p(try_statement),
    p(with_statement),
    p(break_statement),
    p(continue_statement),
    p(return_statement),
    p(throw_statement),
    p(label_statement),
    p(operand),
  ];
}
and Module: SORT = {
  let sort = () => Sort.of_str("Module");
  let atom = () => nt(sort());

  let operand = alt([]);

  let tbl = () => [p(operand)];
};

type t = Sort.Map.t(Prec.Table.t(Regex.t));
let v =
  [
    ObjectPat.(sort(), tbl()),
    LHSExp.(sort(), tbl()),
    Stat.(sort(), tbl()),
    Exp.(sort(), tbl()),
    Module.(sort(), tbl()),
  ]
  |> List.to_seq
  |> Sort.Map.of_seq;
