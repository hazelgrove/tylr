let ascii = [%sedlex.regexp? 0 .. 255];
let space = [%sedlex.regexp? Plus(white_space)];

// let grout_op = [%sedlex.regexp? "⬣"];
// let grout_pre = [%sedlex.regexp? "«"];
// let grout_pos = [%sedlex.regexp? "»"];
// let grout_in = [%sedlex.regexp? "⧗"];
// let grout = [%sedlex.regexp? grout_op | grout_pre | grout_pos | grout_in];

let digit = [%sedlex.regexp? '0' .. '9'];
let alpha_lower = [%sedlex.regexp? 'a' .. 'z'];
let alpha_upper = [%sedlex.regexp? 'A' .. 'Z'];
let alpha = [%sedlex.regexp? alpha_lower | alpha_upper];
let alpha_num = [%sedlex.regexp? alpha | digit | '_'];

let int_lit = [%sedlex.regexp? Plus(digit | '_')];
let float_lit = [%sedlex.regexp?
  (Plus(digit), '.', Star(digit) | Star(digit), '.', Plus(digit))
];

let id_lower = [%sedlex.regexp?
  (alpha_lower | '_', Star(alpha | digit | '_'))
];
let id_upper = [%sedlex.regexp?
  (alpha_upper | '_', Star(alpha | digit | '_'))
];

let round = [%sedlex.regexp? '(' | ')'];
let square = [%sedlex.regexp? '[' | ']'];
let curly = [%sedlex.regexp? '{' | '}'];
let brack = [%sedlex.regexp? round | square | curly];

let op_char = [%sedlex.regexp? Sub(ascii, alpha_num | brack | white_space)];
let operator = [%sedlex.regexp? Plus(op_char)];

let keyword = [%sedlex.regexp? Plus(alpha_lower)];

let lexeme = Sedlexing.Latin1.lexeme;

let pop = buf => {
  let mk = (text: string, lbl: Label.t) => {
    // let _ = failwith("review dropping labels");
    // let lbls =
    //   switch (Labels.completions(text)) {
    //   | [] => [lbl];
    //   // drop labels like id_lower for exact keyword matches
    //   | [_, ..._] as lbls => Labels.is_const(text) ? lbls : [lbl, ...lbls]
    //   };
    let lbls =
      Labels.completions(text)
      // avoid duplicating exact match const label
      |> (Label.is_const(lbl) ? Fun.id : List.cons(lbl));
    Some(Token.Unmolded.mk(~text, Mtrl.Tile(lbls)));
  };
  // I'm guessing buf state is altered by this switch expression?
  // so I can't call lexeme(buf) before it?
  switch%sedlex (buf) {
  | space =>
    let text = lexeme(buf);
    Some(Token.Unmolded.mk(~text, Mtrl.Space(White(Usr))));
  | int_lit => mk(lexeme(buf), Int_lit)
  | float_lit => mk(lexeme(buf), Float_lit)
  | id_lower => mk(lexeme(buf), Id_lower)
  | id_upper => mk(lexeme(buf), Id_upper)
  | brack
  | operator
  | keyword =>
    let text = lexeme(buf);
    // padding filled in by label completions
    mk(text, Label.const(text));

  | eof => None
  | _ => assert(false)
  };
};

let label = (s: string): list(Token.Unmolded.t) => {
  let buf = Sedlexing.Latin1.from_string(s);
  let rev = ref([]);
  let rec go = () =>
    switch (pop(buf)) {
    | None => ()
    | Some(p) =>
      rev := [p, ...rev^];
      go();
    };
  go();
  List.rev(rev^);
};

let single = (s: string): option(Token.Unmolded.t) =>
  switch (label(s)) {
  | [tok] => Some(tok)
  | _ => None
  };
