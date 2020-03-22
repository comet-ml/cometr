`%||%` <- function(x, y) {
  if (length(x) > 0) x else y
}

epoch_ms <- function() {
  as.integer(Sys.time()) * 1000
}

isBool <- function(x) {
  isTRUE(x) || isFALSE(x)
}
