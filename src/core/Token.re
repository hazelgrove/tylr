// make an enum
type t = string;

let is_var = token => Re.Str.string_match(Re.Str.regexp("[a-z]"), token, 0);
let is_num = token => Re.Str.string_match(Re.Str.regexp("[0-9]"), token, 0);

module Index = {
  type t = int;
};