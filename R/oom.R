#' Wrap a loader function to a generator.
#'
#' DataGenerator creates a stateful function wrapper for the loader function as
#' a workaround to load out-of-memory datasets in keras.
#' The returned generator function can be passed to git_generator() directly and
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
#' @param loader a loader function that accepts a subset of sample_ids and returns
#' loaded data. See Details for more information.
#' @param sample_ids a vector of ids passed to loader.
#' @param sample_cls OPTIONAL, a vector of classes returned as y.
#' @param batch_size an integer of batch size.
#'
#' @return a generator function
#' @export
#'
DataGenerator <- function(loader, sample_ids, sample_cls = NULL, batch_size) {

  if (batch_size > length(sample_ids)) {
    err_invalid_value(batch_size,
                      elaborate = "batch size too large for samples.")
  }

  sample_ids <- unlist(sample_ids)
  sample_cls <- unlist(sample_cls)
  nsample <- length(sample_ids)

  #initial state
  idx <- batch_size + 1L
  batch_idx <- seq_len(batch_size)

  generator <- function() {

    #get data from loader
    x <- loader(sample_ids[batch_idx])
    y <- sample_cls[batch_idx]

    #update batch_idx
    idx_end <- min(nsample, idx + batch_size - 1L)
    batch_idx <<- seq.int(idx, idx_end)
    idx <<- ifelse(idx_end == nsample, 1L, idx_end + 1L)

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

  if (batch_size > length(sample_ids)) {
    err_invalid_value(batch_size,
                      elaborate = "batch size too large for samples.")
  }

  sample_ids <- unlist(sample_ids)
  sample_cls <- unlist(sample_cls)
  nsample <- length(sample_ids)

  #initial state
  idx <- batch_size + 1L
  batch_idx <- seq_len(batch_size)
  worker <- parallel::mcparallel({
    loader(sample_ids[batch_idx])
  })

  #simple process management
  stopped <- FALSE

  generator <- function(STOP = FALSE) {

    #check stop state
    if (stopped) {
      err_worker_stop(worker$pid)
    }

    #update stop state
    stopped <<- STOP
    if (STOP) {
      #received user instruction to stop, collect remaining result and stop
      ret <- parallel::mccollect(worker, wait = TRUE)[[1]]
      return(ret)
    }

    #get data from worker
    x <- parallel::mccollect(worker, wait = TRUE)[[1]]
    y <- sample_cls[batch_idx]

    #check for error
    if (class(x) == "try-error") {
      err_worker(x)
    }

    #update batch_idx
    idx_end <- min(nsample, idx + batch_size - 1L)
    batch_idx <<- seq.int(idx, idx_end)
    idx <<- ifelse(idx_end == nsample, 1L, idx_end + 1L)

    #start new worker
    worker <<- parallel::mcparallel({
      loader(sample_ids[batch_idx])
    })

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
DataGeneratorMultiProc <- function(loader, sample_ids, sample_cls = NULL, batch_size,
                                   n_workers) {

  if (batch_size * n_workers > length(sample_ids)) {
    err_invalid_value(n_workers,
                      elaborate = "too many workers for the batch size. Try reducing either.")
  }

  sample_ids <- unlist(sample_ids)
  sample_cls <- unlist(sample_cls)
  nsample <- length(sample_ids)

  batch_mask <- 0L
  update_batch_mask <- function() {
    if (length(batch_mask) < batch_size) {
      #reset loop
      tmp <- seq_len(batch_size)
    } else {
      tmp <- (batch_mask + batch_size) %% nsample
    }
    tmp[tmp == 0L] <- nsample
    tmp <- tmp[seq_len(which.max(tmp))]
    batch_mask <<- tmp

    tmp
  }
  worker_idx <- 0L
  update_worker_idx <- function() {
    tmp <- (worker_idx + 1L) %% n_workers
    if (tmp == 0L) {
      tmp <- n_workers
    }
    worker_idx <<- tmp

    tmp
  }

  #create initial worker jobs
  workers_proc <- list()
  workers_mask <- list()
  for (i in seq_len(n_workers)) {

    update_batch_mask()
    update_worker_idx()

    workers_mask[[worker_idx]] <- batch_mask
    workers_proc[[worker_idx]] <- parallel::mcparallel({
      loader(sample_ids[batch_mask])
    })
  }

  #simple process management
  stopped <- FALSE

  generator <- function(STOP = FALSE) {

    #check stop state
    if (stopped) {
      pids <- NULL
      for (w in workers_proc) {
        pids <- c(pids, w$pid)
      }
      err_worker_stop(pids)
    }

    #update stop state
    stopped <<- STOP
    if (STOP) {
      ret <- parallel::mccollect(workers_proc, wait = TRUE)
      return(ret)
    }

    #get data from worker
    update_worker_idx()
    x <- parallel::mccollect(workers_proc[[worker_idx]], wait = TRUE)[[1]]
    y <- sample_cls[workers_mask[[worker_idx]]]

    #check for error
    if (class(x) == "try-error") {
      err_worker(x)
    }

    #start new worker
    update_batch_mask()
    workers_mask[[worker_idx]] <<- batch_mask
    workers_proc[[worker_idx]] <<- parallel::mcparallel({
      loader(sample_ids[batch_mask])
    })

    if (is.null(sample_cls)) {
      return(list(x))
    } else {
      return(list(x, y))
    }
  }

  generator
}


#' Stop a non-blocking generator worker process and cleanup.
#'
#' Because a generator loops indefinitely, when invoking a non-blocking generator,
#' a child process is always forked in background loading next batch. Even though
#' the generator is not need anymore. Calling this function results in stopping
#' the child process and any consequence IO.
#'
#' This function might block since it waits for last worker to return. It has the
#' same effect of calling generator(STOP = TRUE) but returns value invisibly with
#' an extra call to gc().
#'
#' @param generator a generator function.
#'
#' @return last result from generator(), invisibly.
#' @export
#'
CleanupGenerator <- function(generator){

  invisible(generator(TRUE))

  gc()
}
