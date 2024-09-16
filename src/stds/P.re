let log = print_endline;
let show = (name, shown) => log(name ++ " = " ++ shown);
let sexp = (name, sexp) =>
  log(name ++ " = " ++ Sexplib.Sexp.to_string_hum(sexp));
