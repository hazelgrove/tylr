let swap = ((a, b)) => (b, a);
let map_fst = (f, (x, y)) => (f(x), y);
let map_snd = (f, (x, y)) => (x, f(y));
let map2 = (f, (a, b)) => (f(a), f(b));

let map3 = (f, (a, b, c)) => (f(a), f(b), f(c));
