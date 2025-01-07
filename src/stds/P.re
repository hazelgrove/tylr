// let log = (~n as name="", ~h as show=?, ~x as sexp=?, a) => {
//   let s =
//     switch (sexp, show) {
//     | (None, None) => ""
//     | (Some(sexp), _) => Sexplib.Sexp.to_string_hum(sexp(a))
//     | (_, Some(show)) => show(a)
//     };
//   let s =
//     switch (name, s) {
//     | ("", _) => s
//     | (_, "") => name
//     | _ => name ++ " = " ++ s
//     };
//   print_endline(s);
// };
let log = print_endline;
let show = (name, shown) => log(name ++ " = " ++ shown);
let sexp = (name, sexp) =>
  log(name ++ " = " ++ Sexplib.Sexp.to_string_hum(sexp));

// o for observe. useful for debugging pipes.
let oshow = (name, show_x, x) => {
  show(name, show_x(x));
  x;
};
