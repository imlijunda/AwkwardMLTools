iterator_atomic <- function(x) {

  len <- length(x)
  if (!len) {
    err_invalid_value(x, "argument is of zero length.")
  }
  idx <- 1L
  next_idx <- function() {
    if (idx == len) {
      idx <<- 1L
    } else {
      idx <<- idx + 1L
    }
  }

  iter <- function() {

    i <- idx
    next_idx()

    x[i]
  }

  iter
}

iterator_batch <- function(x, batch_size = 1L) {

  if (batch_size == 1L) {
    return(iterator_atomic(x))
  }

  len <- length(x)
  if (batch_size > len) {
    err_invalid_value(batch_size, "batch size is larger than iterator.")
  }

  idx_start <- 1L
  idx_end <- batch_size
  next_idx <- function() {
    if (idx_end == len) {
      idx_start <<- 1L
    } else {
      idx_start <<- idx_end + 1L
    }
    tmp <- idx_start + batch_size - 1L
    if (tmp > len) {
      idx_end <<- len
    } else {
      idx_end <<- tmp
    }
  }

  iter <- function() {

    batch <- seq.int(idx_start, idx_end)
    next_idx()

    x[batch]
  }

  iter
}

iterator_combi <- function(...) {

  args <- list(...)
  if (!length(args)) err_iterator_args()
  args_len <- sapply(args, length, USE.NAMES = FALSE)
  args_num <- length(args)

  idx <- rep(1L, args_num)
  idx[1] <- 0L
  done <- FALSE

  itr <- vector(mode = "list", length = args_num)
  names(itr) <- names(args)

  iter <- function(reset = FALSE) {

    if (reset) {
      idx <<- rep(1L, args_num)
      idx[1] <<- 0L
      done <<- FALSE
      return(NULL)
    }

    if (done) {
      return(NULL)
    }

    idx[1] <<- idx[1] + 1L
    for (i in seq_len(args_num)) {
      if (idx[i] > args_len[i]) {
        if (i == args_num) {
          done <<- TRUE
          return(NULL)
        }
        idx[i] <<- 1L
        idx[i + 1] <<- idx[i + 1] + 1L
      }
    }

    for (i in seq_len(args_num)) {
      itr[[i]] <<- args[[i]][[idx[i]]]
    }

    itr
  }

  iter
}
