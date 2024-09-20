let log = print_endline;
let show = (name, shown) => log(name ++ " = " ++ shown);
let sexp = (name, sexp) =>
  log(name ++ " = " ++ Sexplib.Sexp.to_string_hum(sexp));

// o for observe. useful for debugging pipes.
let oshow = (name, show_x, x) => {
  show(name, show_x(x));
  x;
};
