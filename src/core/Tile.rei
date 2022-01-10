type s = list(t)
and t =
  | Sep
  | Hole(Sort.t)
  | Tokens(Molded.t(Aba.t(Token.t, s)));

// 1 + let x = 2 in x + 3
// --delete let tile-->
// 1 + () >< x >< 2 >< x + 3
// --construct lam-->
// 1 + \ x . 2 >< x + 3

let tokens: t => Tokens.t;

let molds: t => list(Mold.t);

module Frame = {
  type tile = t;

  // example: y subject
  // 1 + let |y = x + 2 in y + 3

  // [let] _ [= x + 2 in]
  type t = Molded.t(Aba.Frame.b(Token.t, s));

  // [1 +] _ [y + 3]
  type s = ListFrame.t(tile);
};