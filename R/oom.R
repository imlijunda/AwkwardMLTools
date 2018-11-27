#' Non-blocking data generator.
#'
#' @param loader a loader function that accepts a subset of sample_ids and returns
#' loaded data.
#' @param sample_ids a vector of ids passed to loader.
#' @param sample_cls OPTIONAL, a vector of classes returned as y.
#' @param batch_size an integer of batch size.
#'
#' @return a generator function.
#' @export
#'
DataGeneratorNonblock <- function(loader, sample_ids, sample_cls = NULL, batch_size) {

  if (batch_size > length(sample_ids)) {
    err_invalid_value(batch_size,
                      elaborate = "batch size too large for samples.")
  }

  sample_ids <- unlist(sample_ids)
  sample_cls <- unlist(sample_cls)

  nsample <- length(sample_ids)

  #initial state
  x <- NULL
  idx <- batch_size + 1L
  batch_idx <- seq_len(batch_size)
  worker <- future({
    loader(sample_ids[batch_idx])
  })

  function() {

    #get data from worker
    x <<- value(worker)
    y <- sample_cls[batch_idx]

    #update batch_idx
    idx_end <- min(nsample, idx + batch_size - 1L)
    batch_idx <<- seq.int(idx, idx_end)
    idx <<- ifelse(idx_end == nsample, 1L, idx_end + 1L)

    #fire new worker future
    worker <<- future({
      loader(sample_ids[batch_idx])
    })

    if (is.null(sample_cls)) {
      return(list(x))
    } else {
      list(x, y)
    }
  }
}
