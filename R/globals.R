PACKAGE_NAME <- "cometr"
.cometenv <- new.env(parent = emptyenv())

.cometenv$COMET_CONFIG_FILE_NAME <- ".comet.yml"
.cometenv$COMET_API_DEFAULT_URL <- "https://www.comet.ml/clientlib/"
.cometenv$COMET_API_VERSION <- "v2"
.cometenv$COMET_API_ENDPOINT_BASE <- paste0("/api/rest/", .cometenv$COMET_API_VERSION)

.cometenv$LOG_LEVEL_MAP <- list(DEBUG = 1, INFO = 2, ERROR = 3)
.cometenv$LOG_LEVEL_SHORTHAND <- list(DEBUG = "DBG", INFO = "INF", ERROR = "ERR")
.cometenv$logging_enabled <- TRUE
.cometenv$logging_queue <- list()

.cometenv$cache <- list()
.cometenv$cache$config <- list()

reset_comet_cache <- function() {
  .cometenv$cache <- list()
  .cometenv$cache$config <- list()
}
