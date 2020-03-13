#' Get user workspaces
#' @param api_key Comet API key
#' @export
workspaces <- function(api_key = NULL) {
  call_api(endpoint = "/workspaces", method = "GET", api_key = api_key)
}
