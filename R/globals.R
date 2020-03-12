.cometenv <- new.env(parent = emptyenv())

.cometenv$COMET_CONFIG_FILE_NAME <- ".comet.yml"
.cometenv$COMET_API_DEFAULT_URL <- "https://www.comet.ml"
.cometenv$COMET_API_VERSION <- "v2"
.cometenv$COMET_API_ENDPOINT_BASE <- paste0("/api/rest/", .cometenv$COMET_API_VERSION)

.cometenv$api_key <- ""
.cometenv$baseurl <- ""
