#basic iterators

#' Create iterator over an atomic vector.
#'
#' @param x an iterable atomic vector.
#'
#' @return an iterator.
#' @export
#'
#' @examples
#' itr <- iterator_atomic(3:5)
#' #Created iterator loops indefinitely.
#' print(collect(itr, 10))
#' itr <- iterator_atomic(c("a", "b", "c", "d"))
#' itr()
iterator_atomic <- function(x) {

  x <- unlist(x)
  if (is.list(x)) {
    f <- `[[`
  } else {
    f <- `[`
  }

  idx <- 0L
  n <- length(x)
  if (!n) {
    err_invalid_value(x, "argument is of zero length.")
  }

  iter <- function() {
    if (idx == n) {
      idx <<- 1L
    } else {
      idx <<- idx + 1L
    }

    f(x, idx)
  }
  attr(iter, "class") <- "iterator"
  attr(iter, "size") <- n

  iter
}

#' Iterate over batch_size elements a time.
#'
#' @param x an iterable atomic vector.
#' @param batch_size batch size.
#' @param part_size partition size.
#'
#' @return an iterator.
#' @export
#'
#' @examples
#' itr <- iterator_batch(500:5000, 32)
#' itr()
iterator_batch <- function(x, batch_size = 1L) {

  if (batch_size == 1L) {
    return(iterator_atomic(x))
  }

  x <- unlist(x)

  n <- length(x)
  if (!n) {
    err_invalid_value(x, "argument is of zero length.")
  }
  if (batch_size > n) {
    err_invalid_length(batch_size, "batch size is larger than iterator.")
  }

  idx_start <- 0L
  idx_end <- 0L
  iter <- function() {
    if (idx_end == n) {
      idx_start <<- 1L
    } else {
      idx_start <<- idx_end + 1L
    }
    tmp <- idx_start + batch_size - 1L
    if (tmp > n) {
      idx_end <<- n
    } else {
      idx_end <<- tmp
    }

    x[seq.int(idx_start, idx_end)]
  }
  attr(iter, "class") <- "iterator"
  attr(iter, "size") <- n %/% batch_size + as.logical(n %% batch_size)

  iter
}

#' @rdname iterator_batch
#' @export
iterator_partition <- function(x, part_size = 1L) {

  iterator_batch(x, part_size)
}

#composite iterators

#' Iterate over a list of iterators or atomic vectors. Zip style.
#'
#' @param ... iterators or atomic vectors.
#'
#' @return an iterator.
#' @export
#'
#' @examples
#' itr1 <- iterator_atomic(1:6)
#' itr <- iterator_zip(itr1, 7:12)
#' collect(itr, 10)
#'
#' #A warnning will be thrown if size of iterators are different.
#' itr1 <- iterator_atomic(1:6)
#' itr2 <- iterator_atomic(1:4)
#' itr <- iterator_zip(itr1, itr2)
#' collect(itr, 15)
iterator_zip <- function(...) {

  args <- list(...)
  if (!length(args)) {
    err_invalid_value(args, "argument is of zero length.")
  }
  for (i in seq_along(args)) {
    if (!is.iterator(args[[i]])) {
      args[[i]] <- iterator_atomic(args[[i]])
    }
  }
  if (!assert_size(args)) {
    warning("Iterators are of different sizes.")
  }

  empty <- list()
  iter <- function() {
    sapply(args, do.call, empty)
  }
  attr(iter, "class") <- "iterator"
  attr(iter, "size") <- Reduce(lcm_, sapply(args, size), 1L)

  iter
}

iterator_inner <- function(x, inner) {

  if (!is.iterator(x)) {
    x <- iterator_atomic(x)
  }

  idx <- 0L
  n <- size(x)
  elem_left <- inner()
  elem_right <- NULL

  iter_inner <- function() {
    if (idx == n) {
      idx <<- 1L
      elem_left <<- inner()
    } else {
      idx <<- idx + 1L
    }
    elem_right <<- x()

    c(elem_left, elem_right)
  }
  attr(iter_inner, "class") <- "iterator"
  attr(iter_inner, "size") <- n * size(inner)

  iter_inner
}

#' Iterate over a list of iterators or atomic vectors. Cartesian product style.
#'
#' @param ... iterators or atomic vectors.
#'
#' @return an iterator.
#' @export
#'
#' @examples
#' itr1 <- iterator_atomic(1:3)
#' itr <- iterator_product(itr1, 1:2)
#' collect(itr, 10)
#'
#' itr1 <- iterator_atomic(1:3)
#' itr2 <- iterator_atomic(1:2)
#' itr3 <- iterator_product(itr1, itr2)
#' itr <- iterator_product(itr3, 5:8)
#' collect(itr, 20)
iterator_product <- function(...) {

  args <- list(...)
  n <- length(args)
  if (!n) {
    err_invalid_value(args, "argument is of zero length.")
  }
  for (i in seq_along(args)) {
    if (!is.iterator(args[[i]])) {
      args[[i]] <- iterator_atomic(args[[i]])
    }
  }

  iter <- args[[1]]
  if (n > 1L) {
    for (i in seq.int(2L, n)) {
      iter <- iterator_inner(args[[i]], iter)
    }
  }

  iter
}

#' Create a counter that counts forever (before overflowing).
#'
#' @param start counter start
#' @param step step size
#'
#' @return a counter function.
#' @export
#'
#' @examples
#' ctr <- counter(1, .5)
#' collect(ctr, 10)
counter <- function(start = 1L, step = 1L) {

  x <- start - step
  dx <- step

  function() {
    x <<- x + dx
    x
  }
}
