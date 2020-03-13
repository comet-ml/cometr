.cometenv <- new.env(parent = emptyenv())

.cometenv$COMET_CONFIG_FILE_NAME <- ".comet.yml"
.cometenv$COMET_API_DEFAULT_URL <- "https://www.comet.ml"
.cometenv$COMET_API_VERSION <- "v2"
.cometenv$COMET_API_ENDPOINT_BASE <- paste0("/api/rest/", .cometenv$COMET_API_VERSION)

.cometenv$LOG_LEVEL_MAP <- list(DEBUG = 1, INFO = 2, ERROR = 3)
.cometenv$log_init <- FALSE

.cometenv$cache <- list()
.cometenv$cache$config <- list()
