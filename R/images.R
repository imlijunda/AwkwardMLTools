#' Load images and concatenate channels.
#'
#' Load images using the given loader() function, and concatenate all images
#' loaded as a new multi-channel image.
#'
#' It is encouraged to provide h and w unless ret.flatten is TRUE, since an extra
#' loading is run to evaluate them thus reducing performance.
#'
#' @param files files to load.
#' @param loader a function to load the images.
#' @param h OPTIONAL, height of the image. Default: Evaluated during run.
#' @param w OPTIONAL, width of the image. Default: Evaluated during run.
#' @param nch OPTIONAL, total channel number of the concatenated image. Default: Evaluated during run.
#' @param channel_last whether the images are channel_last format. Default: TRUE
#' @param ret.flatten whether to return a flatten vector. Default: FALSE
#'
#' @return a 3-d array.
#' @export
#'
LoadImage <- function(files, loader, h = NULL, w = NULL, nch = NULL,
                      channel_last = TRUE, ret.flatten = FALSE) {

  files <- unlist(files)

  #determine h, w if needed
  if (!ret.flatten && (is.null(h) || is.null(w))) {
    tmp <- loader(files[1])
    d <- dim(tmp)
    if (channel_last) {
      h <- d[1]
      w <- d[2]
    } else {
      h <- d[2]
      w <- d[3]
    }
  }

  #load images
  img <- sapply(files, loader)

  #set dimensions
  if (ret.flatten) {
    dim(img) <- NULL
  } else {
    if (is.null(nch)) {
      nch <- length(img) %/% h %/% w
    }
    if (channel_last) {
      dim(img) <- c(h, w, nch)
    } else {
      dim(img) <- c(nch, h, w)
    }
  }

  img
}

#' Load a batch of images.
#'
#' @details The argument files can be either a list of files to load, or a list
#' of which each element contains a list of files to load and concatenate by
#' channel.
#' The latter can be handy when data is provided in single-channel image files.
#'
#' @param files a list of files or a list of files by channel.
#' @param loader a function to load the images.
#' @param h height of images.
#' @param w width of images.
#' @param nch number of channels of each image.
#' @param channel_last whether the images are channel_last format. Default: TRUE
#' @param batch_last whether the batch is indexed in last dimension. Default: FALSE
#'
#' @return a 4-d array.
#' @export
#'
LoadImageBatch <- function(files, loader, h, w, nch, channel_last = TRUE,
                           batch_last = FALSE) {

  n <- length(files)
  all_img <- sapply(X = files, FUN = LoadImage,
                    loader = loader, channel_last = channel_last, ret.flatten = TRUE)
  if (batch_last) {
    if (channel_last) {
      dim(all_img) <- c(h, w, nch, n)
    } else {
      dim(all_img) <- c(nch, h, w, n)
    }
  } else {
    all_img <- t(all_img)
    if (channel_last) {
      dim(all_img) <- c(n, h, w, nch)
    } else {
      dim(all_img) <- c(n, nch, h, w)
    }
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
