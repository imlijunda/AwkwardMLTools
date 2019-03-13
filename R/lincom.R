fitness <- function(M, b, distf) {
  function(w) {
    distf(M %*% w - b)
  }
}
