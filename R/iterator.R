iterator_atomic <- function(x) {

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

    x[idx]
  }

  iter
}

iterator_batch <- function(x, batch_size = 1L) {

  if (batch_size == 1L) {
    return(iterator_atomic(x))
  }

  n <- length(x)
  if (!n) {
    err_invalid_value(x, "argument is of zero length.")
  }
  if (batch_size > n) {
    err_invalid_value(batch_size, "batch size is larger than iterator.")
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

  iter
}

iterator_inner <- function(x, iter) {

  idx <- 0L
  n <- length(x)
  if (!n) {
    err_invalid_value(x, "argument is of zero length.")
  }

  elem_left <- iter()
  elem_right <- x[1]

  iter_left <- function() {
    if (idx == n) {
      idx <<- 1L
      elem_left <<- iter()
    } else {
      idx <<- idx + 1L
    }
    elem_right <<- x[idx]

    c(elem_left, elem_right)
  }

  iter_left
}

iterator_product <- function(...) {

  args <- list(...)
  n <- length(args)
  if (!n) {
    err_invalid_value(args, "argument is of zero length.")
  }
  for (i in seq_len(n)) {
    if (!is.vector(args[[i]])) {
      #TODO: fix error message
      arg_class <- class(args[[i]])
      err_invalid_value(arg_class, "is not iterable atomic vector.")
    }
  }

  iter <- iterator_atomic(args[[1]])
  if (n > 1L) {
    for (i in seq(2L, n)) {
      iter <- iterator_inner(args[[i]], iter)
    }
  }

  iter
}
