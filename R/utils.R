`%||%` <- function(x, y) {
  if (length(x) > 0) x else y
}

epoch_ms <- function() {
  round(as.numeric(Sys.time()) * 1000)
}

isBool <- function(x) {
  isTRUE(x) || isFALSE(x)
}

get_values_from_list <- function(list, key) {
  unlist(lapply(list, function(element) element[[key]]))
}
