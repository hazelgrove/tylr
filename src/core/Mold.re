type t = Base.t(LR.t(Sort.t));

let merge = (~merge_sorts, mold_l: t('sorts), mold_r: t('sorts)): t('sorts) => {
  tips: {l: mold_l.tips.l, r: mold_r.tips.r},
  sorts: merge_sorts(mold_l.sorts, mold_r.sorts),
};
