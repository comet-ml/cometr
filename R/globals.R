PACKAGE_NAME <- "cometr"
.cometrenv <- new.env(parent = emptyenv())

.cometrenv$COMET_CONFIG_FILE_NAME <- ".comet.yml"
.cometrenv$COMET_API_DEFAULT_URL <- "https://www.comet.com/clientlib/"
.cometrenv$COMET_API_VERSION <- "v2"
.cometrenv$COMET_API_ENDPOINT_BASE <- paste0("/api/rest/", .cometrenv$COMET_API_VERSION)

.cometrenv$LOG_LEVEL_MAP <- list(DEBUG = 1, INFO = 2, WARNING = 3, ERROR = 4)
.cometrenv$LOG_LEVEL_SHORTHAND <- list(DEBUG = "DBG", INFO = "INF", WARNING = "WARN", ERROR = "ERR")

.cometrenv$curexp <- NULL
.cometrenv$cancreate <- FALSE

.cometrenv$cache <- list()
.cometrenv$cache$canlog <- NULL
.cometrenv$cache$config <- list()
