let decode = (s: string): list(Uchar.t) =>
  s
  |> Uutf.String.fold_utf_8(
       (decoded, _) =>
         fun
         | `Malformed(_) => failwith("bug: invalid utf-8")
         | `Uchar(u) => [u, ...decoded],
       [],
     )
  |> List.rev;

let length = s => List.length(decode(s));

let encode = (us: list(Uchar.t)) => {
  let b = Buffer.create(List.length(us));
  us |> List.iter(Buffer.add_utf_8_uchar(b));
  Buffer.contents(b);
};

let split_sub = (i, j, s) => {
  let (sub, (l, r)) = Lists.Framed.sublist(i, j, decode(s));
  (encode(List.rev(l)), encode(sub), encode(r));
};

let split = (i, s) => {
  let (l, r) = Base.List.split_n(decode(s), i);
  (encode(l), encode(r));
};
