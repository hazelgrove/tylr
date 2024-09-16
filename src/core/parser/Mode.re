type t =
  | Navigating
  | Inserting(string)
  | Deleting(Dir.t);

let v = ref(Navigating);

let get = () => v^;
let set = m => v := m;
let reset = () => set(Navigating);

let run_with = (m, f) => {
  let old = get();
  set(m);
  let r = f();
  set(old);
  r;
};
