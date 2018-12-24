#' Indices of samples
#'
#' @description this niche function compare sampled values to measured values
#' cmpf(), and returns a vector of first passed indices for every sampled value.
#' The assumption of measured and sampled being sorted by cmpf guarantees cmpf
#' is called only N times provided length(measured) == N.
#'
#' @param measured a vector of measured values, must be sorted by cmpf
#' @param sampled a vector of sampled values, must be sorted by cmpf
#' @param cmpf a test function to compare sampled values to measured values
#'
#' @return a vector of indices of measured values
#' @export
#'
#' @examples
#' measured <- sort(sample(seq(1000000), 500000))
#' sampled <- sort(sample(measured, 20000) + runif(20000, -100, 0))
#' indices <- sample_idx(measured, sampled, cmpf = `>=`)
#' stopifnot(all(measured[indices] >= sampled))
sample_idx <- function(measured, sampled, cmpf = `>=`) {

  n <- length(sampled)
  itr <- iterator_atomic(sampled)
  ans <- rep(NA_integer_, n)

  idx <- 0L
  cmpval <- itr()
  for (i in seq_along(measured)) {
    if (cmpf(measured[i], cmpval)) {
      idx <- idx + 1L
      cmpval <- itr()
      ans[idx] <- i
      if (idx == n) {
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
