#' Collect multiple iterate values from an iterator.
#'
#' @param itr an iterator or a counter function.
#' @param n how many iterates to collect.
#'
#' @return a vector/matrix/list depending on iterator.
#' @export
#'
#' @examples
#' itr <- iterator_zip(1:3, 6:8)
#' collect(itr, 5)
collect <- function(itr, n = 1L) {

  if (!(is.iterator(itr) || is.function(itr))) {
    err_invalid_class(itr, "is not an iterator or a function.")
  }

  sapply(seq_len(n), function(x) itr())
}

#' Size of an iterator.
#'
#' @param itr an iterator.
#'
#' @return an integer size.
#' @export
#'
#' @examples
#' itr <- iterator_product(1:3, 5:6, 2)
#' size(itr)
size <- function(itr) {

  if (!is.iterator(itr)) {
    err_invalid_class(itr, "is not an iterator.")
  }

  attr(itr, "size")
}

#' Test if an object is an iterator.
#'
#' @param x an object.
#'
#' @return TRUE or FALSE
#' @export
#'
#' @examples
#' is.iterator(counter(1, 3)) #not an iterator
#' is.iterator(iterator_atomic(1:3))
is.iterator <- function(x) {

  is(x, "iterator")
}

assert_length <- function(x, len = NULL) {
  if (is.null(len)) {
    len <- length(x[[1]])
  }
  all(sapply(x, function(item) length(item) == len))
}

assert_size <- function(x, s = NULL) {
  if (is.null(s)) {
    s <- size(x[[1]])
  }
  all(sapply(x, function(item) size(item) == s))
}

gcd_ <- function(x, y) {
  mod <- x %% y
  ifelse(mod != 0L, gcd_(y, mod), y)
}
lcm_ <- function(x, y) {
  abs(x * y) %/% gcd_(x, y)
}
