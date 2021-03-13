let err_holes = ((subject, frame): Zipper_exp.t) => {
  let rec of_frame: Frame_exp.t => list(Decoration.ErrHole.profile) =
    fun
    | Uni(uni) => of_uni(uni)
    | Bi(bi) => of_bi(bi)
  and of_uni: Frame_exp.unidelimited => _ =
    fun
    | Pre_r => ()
  and of_bi: Frame_exp.bidelimited => _ =
    fun
    | Root => []
    | Closed() => raise(Frame_exp.Void_closed)
    | Open(Paren_body())
}