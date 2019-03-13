#' Find indices
#'
#' @description this niche function compares key to val by cmpf(val, key), and
#' returns the indices that passed cmpf(). FindIdx assumes that val and key are
#' sorted by cmpf and finds indices in O(n). FindIdxUnsorted assumes that val
#' and key are not sorted and finds indices in O(n^2). Since cmpf is arbitrary,
#' binary search is not used.
#'
#' @param val a vector of values.
#' @param key a vector of keys to find in val.
#' @param cmpf a comparison function.
#'
#' @return a vector of found indices.
#' @export
#'
#' @examples
#' measured <- sort(sample(seq(1000000), 500000))
#' sampled <- sort(sample(measured, 20000) + runif(20000, -100, 0))
#' indices <- FindIdx(measured, sampled, cmpf = `>=`)
#' stopifnot(all(measured[indices] >= sampled))
FindIdx <- function(val, key, cmpf = `>=`) {

  n <- length(key)
  itr_key <- iterator_atomic(key)
  ans <- rep(NA_integer_, n)

  idx_ans <- 0L
  val_key <- itr_key()
  for (i in seq_along(val)) {
    if (cmpf(val[i], val_key)) {
      idx_ans <- idx_ans + 1L
      val_key <- itr_key()
      ans[idx_ans] <- i
      if (idx_ans == n) {
        break
      }
    }
  }

  ans
}

#' @rdname FindIdx
#' @export
#'
FindIdxUnsorted <- function(val, key, cmpf = `>=`) {

  n <- length(key)
  ans <- rep(NA_integer_, n)

  for (i in seq_along(key)) {
    for (j in seq_along(val)) {
      if (cmpf(val[i], key[j])) {
        ans[i] <- j
        break
      }
    }
  }

  ans
}

#' Pad indices to left or right
#'
#' @param idx a vector of indices
#' @param start left most index
#' @param end right most index
#'
#' @return a vector of padded indices
#' @export
#'
#' @examples
#' n = 1000
#' xi <- sort(sample(seq(n), 100))
#' xleft <- pad_idx_left(xi)
#' xright <- pad_idx_right(xi, n)
pad_idx_left <- function(idx, start = 1L) {

  n <- length(idx)

  c(start, idx[seq.int(n - 1L)] + 1L)
}

#' @rdname pad_idx_left
#' @export
#'
pad_idx_right <- function(idx, end = NA) {

  n <- length(idx)

  c(idx[2L:n] - 1L, end)
}

#' @rdname pad_idx_left
#' @export
#'
pad_idx <- function(idx) {

  n <- length(idx)

  idx[2L:n] - 1L
}
