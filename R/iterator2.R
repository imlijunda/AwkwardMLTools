#more awkwardness

inject_env <- function(itr, names) {
  names <- unlist(names)
  function() {
    x <- parent.frame()
    ans <- itr()
    for (i in seq_along(names)) {
      x[[names[i]]] <- ans[i]
    }

    invisible(NULL)
  }
}

#' Make an iterator return named values.
#'
#' @details Since this is just a wrapper of iterator, renaming an iterator means
#' additional wrapped function call which adds to overhead thus worse performance.
#'
#' @param x an iterator
#' @param value a character vector of desired names
#'
#' @return an iterator
#' @export
#'
#' @examples
#' itr <- iterator_product(1:3, 4:5)
#' names(itr) <- c("a", "b")
#' collect(itr, 5)
`names<-.iterator` <- function(x, value) {

  add_names(x, value)
}

#'  Make an iterator returns named values.
#'
#' @param itr an iterator
#' @param value a character vector of desired names
#'
#' @return an iterator
#' @export
#'
#' @examples
#' itr <- iterator_product(1:3, 4:5)
#' itr_named <- add_names(itr, c("a", "b"))
#' collect(itr, 5)
add_names <- function(itr, value) {

  value <- as.character(unlist(value))
  if (!length(value)) {
    err_invalid_value(value, "argument is of zero length.")
  }
  ans <- rep(NA, length(value))
  names(ans) <- value
  iter <- function() {
    tmp <- itr()
    for (i in seq_along(ans)) {
      ans[i] <<- tmp[i]
    }

    ans
  }
  attr(iter, "class") <- "iterator"
  attr(iter, "size") <- size(itr)
  attr(iter, "elem_names") <- value

  iter
}

#' Element names of an iterator
#'
#' @param x an iterator.
#'
#' @return a character or NULL
#' @export
#'
#' @examples
#' itr <- iterator_product(1:3, 4:5)
#' print(names(itr))
#' names(itr) <- c("a", "b")
#' print(names(itr))
names.iterator <- function(x) {

  attr(x, "elem_names")
}

#' Fake vectorisation of function
#'
#' @param f a non-vectorised function
#' @param ... additional arguments passed to wrapped function
#'
#' @return a function
#' @export
#'
#' @examples
#' f <- function(x) length(x)
#' f2 <- fake_vectorise(f)
#' f(c(1,2,3,4)) #4
#' f2(c(1,2,3,4)) #[1,1,1,1]
fake_vectorise <- function(f, ...) {

  function(x) {
    sapply(x, f, ...)
  }
}

#' Conditioned atomic iterator
#'
#' @param x an atomic vector
#' @param cond a conditional function, must be vectorised.
#'
#' @return an iterator
#' @export
#'
#' @examples
#' x <- runif(n = 10)
#' f <- function(a) a < 0.8
#' itr1 <- iterator_atomic(x)
#' itr2 <- iterator_atomic_cond(x, f)
#' collect(itr1)
#' collect(itr2)
iterator_atomic_cond <- function(x, cond) {

  x <- unlist(x)
  if (!is.vector(x)) {
    err_invalid_class(x, "is not iterable atomic vector.")
  }

  if (is.function(cond)) {
    cond_eval <- cond(x)
  } else {
    cond_eval <- as.logical(cond)
  }

  x <- x[cond_eval]
  n <- length(x)
  if (!n) {
    err_invalid_length(x, "satisfying value is of zero length.")
  }

  iterator_atomic(x)
}

#' Conditioned batch iterator
#'
#' @param x an atomic vector
#' @param batch_size batch size
#' @param part_size partition size.
#' @param cond a conditional function, must be vectorised.
#'
#' @return an iterator
#' @export
#'
#' @examples
#' x <- runif(n = 50)
#' f <- function(a) a < 0.8
#' itr1 <- iterator_batch(x, 3)
#' itr2 <- iterator_batch_cond(x, 3, f)
#' collect(itr1)
#' collect(itr2)
iterator_batch_cond <- function(x, batch_size = 1L, cond) {

  if (batch_size == 1L) {
    return(iterator_atomic_cond(x, cond))
  }

  x <- unlist(x)
  if (!is.vector(x)) {
    err_invalid_class(x, "is not iterable atomic vector.")
  }

  cond_eval <- cond(x)
  x <- x[cond_eval]
  n <- length(x)
  if (!n || n < batch_size) {
    err_invalid_length(x, "satisfying value is smaller than batch size.")
  }

  iterator_batch(x, batch_size)
}

#' @rdname iterator_batch_cond
#' @export
iterator_partition_cond <- function(x, part_size = 1L, cond) {

  iterator_batch_cond(x, part_size, cond)
}
