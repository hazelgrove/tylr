[@deriving sexp]
type t =
  | Undefined
  | Cant_move
  | Cant_construct(Sort.t, Sort.t)
  | Cant_pick_up_selection
  | Cant_put_down_selection(Sort.t, Sort.t)
  | Cant_construct_in_restructuring;
