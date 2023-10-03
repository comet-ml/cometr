get_artifact_files <- function(artifact_id = NULL,
                               workspace = NULL,
                               artifact_name = NULL,
                               version = NULL,
                               alias = NULL,
                               api_key = NULL) {
  endpoint <- "/artifacts/version/files"
  method <- "GET"
  params <- list(
    artifact_id = artifact_id,
    workspace = workspace,
    artifactName = artifact_name,
    version = version,
    alias = alias
  )
  call_api(
    endpoint = endpoint,
    method = method,
    params = params,
    api_key = api_key
  )
}
