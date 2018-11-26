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

  if (is.null(idx_ref)) {
    err_null_arg(idx_ref)
  }
  if (is.null(idx)) {
    return(idx_ref)
  }

  cls <- IdxToClass(idx, onehot)
  cls_ref <- IdxToClass(idx_ref, onehot)

  setdiff(cls_ref, cls)
}

#' Swap portion of the missing classes from idx_ref to idx.
#'
#' Could be useful to re-construct idx and idx_ref with no missing classes. Set
#' swap_ratio to 0 results in a relatively (but not globally) minimal swapping.
#'
#' @param idx a vector of indices.
#' @param idx_ref a vector of indices.
#' @param onehot a onehot table.
#' @param swap_ratio ratio of swapped elements from idx_ref.
#' @param swap_random whether swapped elements are randomly selected by sample(),
#' otherwise elements are selected in index order.
#'
#' @return a named list with new idx and idx_ref.
#' @export
#'
SwapMissingClass <- function(idx, idx_ref, onehot,
                             swap_ratio = 0.1, swap_random = FALSE) {

  if (swap_ratio < 0 || swap_ratio >= 1) {
    err_invalid_value(swap_ratio)
  }

  #faster look-up list for indices
  lookup_idx <- DecodeOnehot(t(onehot), 0L)

  mc <- MissingClass(idx, idx_ref, onehot)
  while (length(mc)) {

    swap_cls <- mc[1]

    swap_idx <- intersect(idx_ref, lookup_idx[[swap_cls]])
    nswap <- max(floor(length(swap_idx) * swap_ratio), 1L)
    if (swap_random) {
      swap_idx <- sample(swap_idx, size = nswap)
    } else {
      swap_idx <- swap_idx[seq_len(nswap)]
    }

    #put swap_idx to idx
    idx <- c(idx, swap_idx)
    #remove swap_idx from idx_ref
    idx_ref <- setdiff(idx_ref, swap_idx)

    #update missing classes
    mc <- MissingClass(idx, idx_ref, onehot)
  }

  ret <- list()
  ret$idx <- idx
  ret$idx_ref <- idx_ref

  ret
}
