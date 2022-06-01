open Sexplib.Std;

// make an enum
[@deriving (show, sexp)]
type t = string;

let regexp = (r, s) => Re.Str.string_match(Re.Str.regexp(r), s, 0);

/* NOTE: right now I am attempting to maintain the invariant
   that every substring of every token is a valid token. */

let is_var = regexp("^[a-z]*$");
let is_num = regexp("^[0-9]*$");
let ops = ["+", "-", "*", "/", ",", "=", ">"];
let whitespace = [" ", "\n"];
let delims_non_kw = [["(", ")"], ["[", "]"], ["?", ":"], ["=>"], ["="]];
let delims_kw = [["fun"], ["let", "in"]];

let is_alphanum = t => is_var(t) || is_num(t);
let is_op = t => List.mem(t, ops);
let is_whitespace = t => List.mem(t, whitespace);
let is_delim_non_kw = t => List.mem(t, List.flatten(delims_non_kw));
let is_delim_kw = t => List.mem(t, List.flatten(delims_kw));
let delims = List.flatten(delims_non_kw @ delims_kw);
let is_delim = t => is_delim_kw(t) || is_delim_non_kw(t);
let is_symbol = t => is_op(t) || is_delim_kw(t) || is_delim_non_kw(t);
let is_non_whitespace = t => is_alphanum(t) || is_symbol(t);
let is_valid = t => is_non_whitespace(t) || is_whitespace(t);

module Index = {
  type t = int;
};
