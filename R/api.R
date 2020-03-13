check_status <- function(res) {
  if (!httr::status_code(res) == 200) {
    stop("Comet API returned an error.", call. = FALSE)
  }
}

get_version <- function() {
  .cometenv$COMET_API_VERSION
}
