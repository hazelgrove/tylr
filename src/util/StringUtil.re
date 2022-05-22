let remove_nth = (n, t) => {
  assert(n < String.length(t));
  String.sub(t, 0, n) ++ String.sub(t, n + 1, String.length(t) - n - 1);
};

let insert_nth = (n, s, t) => {
  assert(n < String.length(t));
  String.sub(t, 0, n) ++ s ++ String.sub(t, n, String.length(t) - n);
};

let split_nth = (n, t) => {
  assert(n < String.length(t));
  (String.sub(t, 0, n), String.sub(t, n, String.length(t) - n));
};
