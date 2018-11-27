#' Encode a list of classes to a onehot table.
#'
#' @param cls_list a list of integer classes.
#' @param nclass OPTIONAL, number of classes.
#' @param value_offset OPTIONAL, a value to offset smaller than 1 values in cls_list.
#'
#' @return a onehot table.
#' @export
#'
EncodeClassList <- function(cls_list, nclass = NULL, value_offset = NULL) {

  if (is.character(cls_list[[1]])) {
    cls_list <- lapply(cls_list,
                       function(x) as.integer(
                         strsplit(x, " ", fixed = TRUE)[[1]]
                         )
                       )
  }

  if (is.null(nclass)) {
    nclass <- length(unique(unlist(cls_list)))
  }
  if (is.null(value_offset)) {
    value_offset <- 1L - min(unlist(cls_list))
  }

  onehot <- matrix(0L, nrow = length(cls_list), ncol = nclass)
  for (i in seq_along(cls_list)) {
    hot_idx <- cls_list[[i]] + value_offset
    onehot[i, hot_idx] <- 1L
  }

  onehot
}

#' Decode a onehot table to a list of classes
#'
#' @param onehot a onehot table.
#' @param value_offset the offset value used to encode onehot table.
#'
#' @return a list of integer classes
#' @export
#'
DecodeOnehot <- function(onehot, value_offset) {

  cls_list <- list()
  for (i in seq_len(nrow(onehot))) {
    cls_list[[i]] <- which(as.logical(onehot[i, ])) - value_offset
  }

  cls_list
}

#' Get hot indices from classes.
#'
#' @param cls a vector of classes.
#' @param onehot a onehot table.
#' @param f_reduce function to reduce, if NULL, union is performed.
#' @param ret.sorted return sorted indices.
#' @param fast whether to use faster algorithm when f_reduce is NULL, result is
#' always sorted in this case.
#' @param ... arguments passed to f_reduce.
#'
#' @return a vector of indices.
#' @export
#'
ClassToIdx <- function(cls, onehot, f_reduce = NULL, ret.sorted = FALSE,
                       fast = TRUE, ...) {

  if (is.null(cls)) {
    return(integer())
  }

  if (is.null(f_reduce) && fast) {
    tmp <- as.logical(colSums(t(onehot[, cls, drop = FALSE])))
    idx <- seq_len(nrow(onehot))

    return(idx[tmp])
  }

  which_onehot(cls, onehot, f_reduce, ret.sorted, ...)
}

#' Get hot classes from indices
#'
#' @param idx a vector of indices.
#' @param onehot a onehot table.
#' @param f_reduce function to reduce, if NULL, union is performed.
#' @param ret.sorted return sorted classes.
#' @param fast whether to use faster algorithm when f_reduce is NULL, result is
#' always sorted in this case.
#' @param ... arguments passed to f_reduce.
#'
#' @return a vector of classes.
#' @export
#'
IdxToClass <- function(idx, onehot, f_reduce = NULL, ret.sorted = FALSE,
                       fast = TRUE, ...) {

  if (is.null(idx)) {
    return(integer())
  }

  if (is.null(f_reduce) && fast) {
    tmp <- as.logical(colSums(onehot[idx, , drop = FALSE]))
    cls <- seq_len(ncol(onehot))

    cls[tmp]
  }

  tmp <- t(onehot)
  which_onehot(idx, tmp, f_reduce, ret.sorted, ...)
}

which_onehot <- function(x, onehot, f_reduce, ret.sorted, ...) {

  idx <- purrr::map(x, function(t) which(as.logical(onehot[, t])))
  if (is.null(f_reduce)) {
    ret <- unique(unlist(idx))
  } else {
    ret <- purrr::reduce(idx, f_reduce, ...)
  }

  if (ret.sorted) {
    return(sort(ret))
  } else {
    return(ret)
  }
}

#' Find all related classes.
#'
#' By related classes I mean all classes that share a same index.
#'
#' @param cls a vector of classes.
#' @param onehot a onehot table.
#' @param ... arguments passed to IdxToClass and ClassToIdx.
#'
#' @return a vector of classes.
#' @export
#'
RelatedClass <- function(cls, onehot, ...) {

  cls %>%
    ClassToIdx(onehot, ...) %>%
    IdxToClass(onehot, ...)
}

#' Find all related indices.
#'
#' By related indices I mean all indices that share a same class.
#'
#' @param idx a vector of indices.
#' @param onehot a onehot table.
#' @param ... arguments passed to IdxToClass and ClassToIdx.
#'
#' @return a vector of indices.
#' @export
#'
RelatedIdx <- function(idx, onehot, ...) {

  idx %>%
    IdxToClass(onehot, ...) %>%
    ClassToIdx(onehot, ...)
}

#' Count number of indices of each class.
#'
#' @param onehot a onehot table.
#' @param ret.density wheter to return density, otherwise number of indices.
#'
#' @return a numeric vector.
#' @export
#'
CountClass <- function(onehot, ret.density = FALSE) {

  n <- colSums(onehot)
  if (ret.density) {
    n <- n / sum(n)
  }

  n
}

#' Find classes that is scarce.
#'
#' @param threshold a threshold, any number of idx < threshold will return.
#' @param onehot a onehot table.
#'
#' @return an integer vector.
#' @export
#'
ScarceClass <- function(threshold, onehot) {

  cc <- CountClass(onehot)

  which(cc < threshold)
}
