type movement =
  | Char
  | Mono
  | Token;

type settings = {
  mutable movement,
  mutable whitespace_debug: bool,
};
let s = {movement: Char, whitespace_debug: true};
