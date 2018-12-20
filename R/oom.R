#' Wrap a loader function to a generator.
#'
#' DataGenerator creates a stateful function wrapper for the loader function as
#' a workaround to load out-of-memory datasets in keras.
#' The returned generator function can be passed to fit_generator() directly and
#' loops over sample_ids indefinitely.
#'
#' Since it can be tricky to implement a python-like iterator in R, using
#' fit_generator() provided by package keras may be restricted to using the
#' built-in generators only, DataGenerator is provided as a workaround but due
#' to not loading all data into memory at once, it is not compatible with data
#' augmentation provided by image_data_generator() at the moment. However, this
#' can be implemented in loader function as a workaround.
#'
#' A loader function should take a subset of sample_ids (which should be a vector
#' of atomic type values) as the only argument and return loaded training data
#' which is ready to be supplied as a batch of training data x.
#' Since DataGenerator is transparent to data structure and it doesn't care about
#' anything returned from loader() but collects and passes them directly, the
#' loader function should take care of error processing, shape of returned data
#' etc.
#'
#' DataGeneratorNonblock is also provided for a somehow non-blocking (overhead and
#' transfering a large chunk or memory can still be "blocking") version of generator.
#' The non-blocking generator offloads the loader() job to a child process and
#' fetches the result everytime generator() is called. Since it is implemented by
#' calling mcparallel() and mccollect() from parallel package, this function is
#' not supported in Windows system. You should also notice that the forked child
#' process will stay in the background even training is over and no more loading
#' is needed. In order to stop the child process and cleanup memory, you should
#' call CleanupGenerator(generator), or generator(STOP = TRUE) to signal the child
#' process and garbage collection.
#'
#' DataGeneratorNonblock(...) is equivalent to DataGeneratorMultiProc(..., n_workers = 1)
#'
#' @param loader a loader function that accepts a subset of sample_ids and returns
#' loaded data. See Details for more information.
#' @param sample_ids a vector of ids passed to loader.
#' @param sample_cls OPTIONAL, a vector of classes returned as y.
#' @param batch_size an integer of batch size.
#' @param n_workers number of workers in case of DataGeneratorMultiProc
#'
#' @return a generator function
#' @export
#'
DataGenerator <- function(loader, sample_ids, sample_cls = NULL, batch_size) {

  sample_ids <- unlist(sample_ids)
  sample_cls <- unlist(sample_cls)

  if (batch_size > length(sample_ids)) {
    err_invalid_value(batch_size, "batch size larger than sample size.")
  }

  next_batch_idx <- iterator_batch(seq_along(sample_ids), batch_size)

  generator <- function() {

    bidx <- next_batch_idx()
    x <- loader(sample_ids[bidx])
    y <- sample_cls[bidx]

    if (is.null(sample_cls)) {
      return(list(x))
    } else {
      return(list(x, y))
    }
  }

  generator
}

#' @rdname DataGenerator
#' @export
DataGeneratorNonblock <- function(loader, sample_ids, sample_cls = NULL, batch_size) {

  DataGeneratorMultiProc(loader, sample_ids = sample_ids, sample_cls = sample_cls,
                         batch_size = batch_size, n_workers = 1L)
}

parallel_rmChild <- utils::getFromNamespace("rmChild", "parallel")

#' @rdname DataGenerator
#' @export
DataGeneratorMultiProc <- function(loader, sample_ids, sample_cls = NULL, batch_size,
                                   n_workers) {

  sample_ids <- unlist(sample_ids)
  sample_cls <- unlist(sample_cls)

  if (batch_size > length(sample_ids)) {
    err_invalid_value(batch_size, "batch size larger than sample size.")
  }
  if (batch_size * n_workers > length(sample_ids)) {
    n_workers <- length(sample_ids) %/% batch_size
    msg <- sprintf("n_workers reduced to %d.", n_workers)
    warning(msg)
  }

  next_batch_idx <- iterator_batch(seq_along(sample_ids), batch_size)
  next_worker_idx <- iterator_atomic(seq_len(n_workers))

  #create initial worker jobs
  workers <- list()
  for (i in seq_len(n_workers)) {
    widx <- next_worker_idx()
    bidx <- next_batch_idx()
    workers[[widx]] <- list()
    workers[[widx]]$proc <- parallel::mcparallel({
      loader(sample_ids[bidx])
    })
    workers[[widx]]$batch_idx <- bidx
  }

  stopped <- FALSE

  force_stop <- function() {
    for (worker in workers) {
      parallel_rmChild(worker$proc$pid)
    }
    stopped <<- TRUE

    NULL
  }

  check_stop <- function() {
    #check stop state
    if (stopped) {
      pids <- NULL
      for (worker in workers) {
        pids <- c(pids, worker$proc$pid)
      }
      err_worker_stop(pids)
    }

    NULL
  }

  check_val <- function(x, widx) {
    #check for error
    if (class(x) == "try-error") {
      force_stop()
      err_worker(x)
    }
    #check for NULL
    if (is.null(x)) {
      #could be a problem of loader, or the child process is killed.
      force_stop()
      err_worker_null(workers[[widx]]$proc$pid)
    }

    NULL
  }

  generator <- function(STOP = FALSE) {

    check_stop()
    #update stop state
    if (STOP) {
      force_stop()
      return(NULL)
    }

    #get data from worker
    widx <- next_worker_idx()
    bidx <- next_batch_idx()
    x <- parallel::mccollect(workers[[widx]]$proc, wait = TRUE)[[1]]
    y <- sample_cls[workers[[widx]]$batch_idx]
    check_val(x, widx)

    #start new worker
    workers[[widx]]$proc <<- parallel::mcparallel({
      loader(sample_ids[bidx])
    })
    workers[[widx]]$batch_idx <<- bidx

    if (is.null(sample_cls)) {
      return(list(x))
    } else {
      return(list(x, y))
    }
  }

  #BUG: fork.c throws warnings in mc_select_children about process not existing
  #when multiple child processes are forked.
  if (getRversion() >= "3.5.0" && getRversion() <= "3.5.1") {
    f <- function(STOP = FALSE) {
      suppressWarnings(generator(STOP))
    }
  } else {
    f <- generator
  }

  f
}


#' Stop a non-blocking generator worker process and cleanup.
#'
#' Because a generator loops indefinitely, when invoking non-blocking generators,
#' child processes are always forked in background loading next batches. Even though
#' the generators are not need anymore. Calling this function results in stopping
#' the child process and any consequence IO.
#'
#' This function force stops the multi-process generator and remove the function
#' from its parent frame to make gc works.
#'
#' @param generator a generator function.
#'
#' @return last result from generator(), invisibly.
#' @export
#'
CleanupGenerator <- function(generator){

  generator(TRUE)

  name <- as.character(substitute(generator))
  penv <- parent.frame()
  rm(list = name, envir = penv)

  NULL
}
