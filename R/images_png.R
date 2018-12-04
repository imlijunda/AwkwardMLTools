#' @rdname LoadImage
#' @export
#'
LoadPNG <- function(files, h = NULL, w = NULL, nch = NULL,
                    channel_last = TRUE, ret.flatten = FALSE) {

  LoadImage(files, loader = png::readPNG, h = h, w = w, nch = nch,
            channel_last = channel_last, ret.flatten = ret.flatten)
}

#' @rdname LoadImageBatch
#' @export
#'
LoadPNGBatch <- function(files, h, w, nch, channel_last = TRUE,
                         batch_last = FALSE, ret.flatten = FALSE) {

  LoadImageBatch(files, loader = png::readPNG, h = h, w = w, nch = nch,
                 channel_last = channel_last, batch_last = batch_last,
                 ret.flatten = ret.flatten)
}

