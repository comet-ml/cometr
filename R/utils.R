`%||%` <- function(x, y) {
  if (!is.null(x)) x else y
}

epoch_ms <- function() {
  as.integer(Sys.time()) * 1000
}
