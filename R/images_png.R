#' Load and concatenate png channels.
#'
#' Channel last is assumed. Use aperm to convert to channel first if needed.
#'
#' If h or w is not given, an extra loading/evaluation on first channel is done
#' on first channel unless ret.flatten, so some performance loss I guess.
#'
#' @param channels file names for each channel.
#' @param h OPTIONAL, height of the image.
#' @param w OPTIONAL, width of the image.
#' @param ret.flatten return a flatten vector.
#'
#' @return a channel last 3-d array.
#' @export
#'
LoadPNGChan <- function(channels, h = NULL, w = NULL, ret.flatten = FALSE) {

  channels <- as.character(unlist(channels))
  nch <- length(channels)

  if (!ret.flatten && (is.null(h) || is.null(w))) {
    tmp <- png::readPNG(channels[1])
    d <- dim(tmp)
    h <- d[1]
    w <- d[2]
  }

  #not sure if a wrapper is need, lapply is just wierd
  f <- function(x) png::readPNG(x)
  img <- sapply(channels, f)
  if (ret.flatten)
  {
    dim(img) <- NULL
  } else {
    dim(img) <- c(h, w, nch)
  }

  img
}

#' Load a list of png.
#'
#' This function is essentially an lapply of LoadPNGChan on files.
#'
#' @param files a list of filenames or a list of channel filenames.
#' @param h height of image.
#' @param w width of image.
#' @param nch total channels of image.
#' @param file_idx_first whether to put file index in fisrt dimension, otherwise it's put in last dim.
#'
#' @return a 4-d array.
#' @export
#'
LoadPNGList <- function(files, h, w, nch, file_idx_first = TRUE) {

  n <- length(files)
  if (file_idx_first) {
    all_img <- array(data = 0.0, dim = c(n, h, w, nch))
    for (i in seq_len(n)) {
      all_img[i, , , ] <- LoadPNGChan(files[[i]], h, w)
    }
  } else {
    all_img <- sapply(X = files, FUN = LoadPNGChan, ret.flatten = TRUE)
    dim(all_img) <- c(h, w, nch, n)
  }

  all_img
}


#' Estimate memory usage of images.
#'
#' @param n number of images to load.
#' @param h height of image.
#' @param w width of image.
#' @param nch number of channels.
#' @param unit what unit to use, "k" for KB, "m" for MB and "g" for GB,
#' otherwise bytes.
#'
#' @return a numeric.
#' @export
#'
EstimateMem <- function(n, h, w, nch, unit = "k") {

  mem_byte <- 8.0 * h * w * nch * n
  mem <- switch(substr(toupper(unit), 1L, 1L),
                K = mem_byte / 1024.0,
                M = mem_byte / 1048576.0,
                G = mem_byte / 1073741824.0,
                mem_byte)

  mem
}
