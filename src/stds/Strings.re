let is_empty = (==)("");

let to_list = Base.String.to_list;

let insert_nth = (n, s, t) => {
  assert(n < String.length(t));
  String.sub(t, 0, n) ++ s ++ String.sub(t, n, String.length(t) - n);
};

let repeat = (n, s) => String.concat("", List.init(n, _ => s));

let count = c => Base.String.count(~f=(==)(c));

let split = Base.String.split;

let rev = Base.String.rev;
