let is_var = s => Str.string_match(Str.regexp("[a-z]"), s, 0);
let is_num = s => Str.string_match(Str.regexp("[0-9]"), s, 0);
