open Sexplib.Std;

// make an enum
[@deriving (show, sexp)]
type t = string;

let is_var = token => Re.Str.string_match(Re.Str.regexp("[a-z]"), token, 0);
let is_num = token => Re.Str.string_match(Re.Str.regexp("[0-9]"), token, 0);
let ops_in = ["+", "-", "*", "/", ","];
let whitespace = [" "];
let delims_non_kw = [["(", ")"], ["[", "]"], ["?", ":"], ["=>"], ["="]];
let delims_kw = [["fun"], ["let", "in"]];

let is_alphanum = t => is_var(t) || is_num(t);
let is_op = t => List.mem(t, ops_in);
let is_whitespace = t => List.mem(t, whitespace);
let is_delim_non_kw = t => List.mem(t, List.flatten(delims_non_kw));
let is_delim_kw = t => List.mem(t, List.flatten(delims_kw));
let delims = List.flatten(delims_non_kw @ delims_kw);
let is_delim = t => is_delim_kw(t) || is_delim_non_kw(t);
let is_special = t => is_whitespace(t) || is_delim_non_kw(t) || is_op(t);
let is_valid_char = t => is_alphanum(t) || is_special(t);

module Index = {
  type t = int;
};
