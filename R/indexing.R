#' Find missing classes in a set of indices compared to reference indices.
#'
#' @param idx a vector of indices.
#' @param idx_ref a vector of indices.
#' @param onehot a onehot table.
#'
#' @return a vector of classes.
#' @export
#'
MissingClass <- function(idx, idx_ref, onehot) {

  cls <- IdxToClass(idx, onehot)
  cls_ref <- IdxToClass(idx, onehot)
  diff_cls <- setdiff(cls, cls_ref)

  intersect(diff_cls, cls_ref)
}
