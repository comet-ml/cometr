#' @title A Comet Artifact object
#' @description
#' Comet Artifacts allow keeping track of assets beyond any particular experiment. You can keep
#' track of Artifact versions, create many types of assets, manage them, and use them in any
#' step in your ML pipelines - from training to production deployment.
#'
#' Artifacts live in a Comet Project, are identified by their name and version string number.
#'
#' @examples
#' \dontrun{
#' library(cometr)
#' # Assuming you have COMET_API_KEY, COMET_WORKSPACE, COMET_PROJECT_NAME variables define
#' exp <- create_experiment()
#'
#' # Create a Comet Artifact
#' artifact <- Artifact$new(name = "Artifact-Name", artifact_type = "Artifact-Type")
#' artifact$add("local-file")
#'
#' exp$log_artifact(artifact)
#' exp$stop()
#' }
#'
#' @export
Artifact <- R6::R6Class(
  cloneable = FALSE,

  "Artifact",

  public = list(
    initialize = function(name, artifact_type = NULL, version = NULL,
                          aliases = NULL, metadata = NULL, version_tags = NULL) {

    },

  ),

  private = list(
    name = NULL,
    artifact_type = NULL,
    version = NULL,
    aliases = NULL,
    metadata = NULL,
    version_tags = NULL,
  )
)
