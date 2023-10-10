#' @import R6
NULL

#' @title A Logged Comet Artifact object
#' @description
#' Comet Artifacts allow keeping track of assets beyond any particular experiment.
#' The `LoggedArtifact` is a Comet [`Artifact`] that already logged to the Comet servers
#' and can be used to access the artifact version assets and download them locally.
#'
#'
#' @examples
#' \dontrun{
#' library(cometr)
#' # Assuming you have COMET_API_KEY, COMET_WORKSPACE, COMET_PROJECT_NAME variables define
#' exp <- create_experiment()
#'
#' # Get a Comet Artifact
#' artifact <- exp$get_artifact(artifact_name = "workspace/artifact-name:versionOrAlias")
#'
#' exp$stop()
#' }
#'
#' @export
LoggedArtifact <- R6::R6Class(
  cloneable = FALSE,

  "LoggedArtifact",

  public = list(
    #' @description
    #' Creates new `LoggedArtifact` object with provided parameters. Do not use
    #' this method directly. Use `Experiment$get_artifact()`
    #' to retrieve `LoggedArtifact`.
    #' @param artifact_name (Required) Artifact name.
    #' @param artifact_type (Required) The artifact type.
    #' @param artifact_id (Required) The ID of artifact.
    #' @param artifact_version_id (Required) The ID of Artifact Version.
    #' @param workspace (Required) The workspace where artifact saved.
    #' @param experiment_key (Required) The ID of the associated experiment.
    #' @param artifact_version (Required) The latest artifact version.
    #' @param aliases (Required) List of Artifact Version aliases.
    #' @param artifact_tags (Required) The list of artifact tags.
    #' @param version_tags (Required) List of Artifact Version tags.
    #' @param size (Required) The total size of logged artifact version.
    #' It is the sum of all the artifact version assets.
    #' @param metadata The meta-data of Artifact Version.
    #' @param source_experiment_key The ID of the experiment that created this artifact version.
    initialize = function(artifact_name,
                          artifact_type,
                          artifact_id,
                          artifact_version_id,
                          workspace,
                          experiment_key,
                          artifact_version,
                          aliases,
                          artifact_tags,
                          version_tags,
                          size,
                          metadata = NULL,
                          source_experiment_key = NULL) {
      private$artifact_name <- artifact_name
      private$artifact_type <- artifact_type
      private$artifact_id <- artifact_id
      private$artifact_version_id <- artifact_version_id
      private$workspace <- workspace
      private$experiment_key <- experiment_key
      private$aliases <- aliases
      private$artifact_tags <- artifact_tags
      private$version_tags <- version_tags
      private$.size <- size
      private$metadata <- metadata
      private$source_experiment_key <- source_experiment_key

      if (!is.null(artifact_version)) {
        private$artifact_version <- numeric_version(artifact_version)
      }

    },

    #' @description
    #' Get the name of the artifact.
    get_artifact_name = function() {
      private$artifact_name
    },

    #' @description
    #' Get the type of the artifact.
    get_artifact_type = function() {
      private$artifact_type
    },

    #' @description
    #' Get the version of the artifact.
    get_artifact_version = function() {
      private$artifact_version
    },

    #' @description
    #' Get the ID of the artifact.
    get_artifact_id = function() {
      private$artifact_id
    },

    #' @description
    #' Get the tags of the artifact.
    get_artifact_tags = function() {
      private$artifact_tags
    },

    #' @description
    #' Get the version of the artifact.
    get_aliases = function() {
      private$aliases
    },

    #' @description
    #' Get the metadata of the artifact.
    get_metadata = function() {
      private$metadata
    },

    #' @description
    #' Get the list of tags of the artifact version.
    get_version_tags = function() {
      private$version_tags
    },

    #' @description
    #' Get the workspace of the Artifact.
    get_workspace = function() {
      private$workspace
    },

    #' @description
    #' The ID of current Artifact Version
    get_artifact_version_id = function() {
      private$artifact_version_id
    },

    #' @description
    #'The ID of the experiment that created this artifact version.
    get_source_experiment_key = function() {
      private$source_experiment_key
    },

    #' @description
    #'The ID of the associated experiment.
    get_experiment_key = function() {
      private$experiment_key
    },

    #' @description
    #'Get/set artifact size.
    #'@param size The new size for the Artifact or `NULL` if retrieving existing
    #'size of the Artifact.
    size = function(size = NULL) {
      if (!is.null(size)) {
        private$.size <- size
      }
      private$.size
    },

    #' @description
    #' Get the list of all [`LoggedArtifactAsset`] that have been logged with
    #' this `LoggedArtifact` from Comet server.
    get_assets = function() {
      files = private$load_artifact_assets()
      if (length(files)) {
        lapply(files, function (file)
          private$to_logged_asset(file))
      } else {
        list()
      }
    },

    #' @description
    #' Get the list of remote [`LoggedArtifactAsset`] that have been logged
    #' with this `LoggedArtifact` from Comet server.
    get_remote_assets = function() {
      files = private$load_artifact_assets()
      if (length(files)) {
        remote_indexes = sapply(files, function(f)
          f$remote)
        lapply(files[remote_indexes], function (file)
          private$to_logged_asset(file))
      } else {
        list()
      }
    },

    #' @description
    #' Update the logged artifact tags
    #' @param artifact_tags The new tags for the artifact
    update_artifact_tags = function(artifact_tags) {
      update_arifact(
        artifact_id = private$artifact_id,
        tags = artifact_tags
      )
      private$artifact_tags <- artifact_tags
      invisible(self)
    },

    #' @description
    #' Update the logged artifact version tags
    #' @param version_tags The new tags for the artifact version
    update_version_tags = function(version_tags) {
      version_tags <- unique(version_tags)

      update_artifact_version(
        artifact_version_id = private$artifact_version_id,
        version_tags = version_tags
      )
      private$version_tags <- version_tags
      invisible(self)
    },

    #' @description
    #' Update the logged artifact version aliases
    #' @param aliases The new aliases for the artifact version
    update_aliases = function(aliases) {
      aliases <- unique(aliases)

      update_artifact_version(
        artifact_version_id = private$artifact_version_id,
        version_aliases = aliases
      )
      private$aliases <- aliases
      invisible(self)
    }
  ),

  private = list(
    artifact_name = NULL,
    artifact_type = NULL,
    artifact_id = NULL,
    artifact_version_id = NULL,
    workspace = NULL,
    experiment_key = NULL,
    artifact_version = NULL,
    aliases = NULL,
    artifact_tags = NULL,
    metadata = NULL,
    version_tags = NULL,
    .size = 0,
    source_experiment_key = NULL,

    to_logged_asset = function(asset) {
      asset_metadata <- asset[["metadata"]]
      if (!is.null(asset_metadata)) {
        asset_metadata <- decode_metadata(asset_metadata)
      }

      LoggedArtifactAsset$new(
        logical_path = asset[["fileName"]],
        remote = as.logical(asset[["remote"]]),
        size = as.integer(asset[["fileSize"]]),
        link = asset[["link"]],
        metadata = asset_metadata,
        asset_type = asset[["type"]],
        id = asset[["assetId"]],
        artifact_version_id = private$artifact_version_id,
        artifact_id = private$artifact_id,
        experiment_key = private$experiment_key
      )
    },

    load_artifact_assets = function() {
      artifact_files = get_artifact_files(
        workspace = private$workspace,
        artifact_name = private$artifact_name,
        version = as.character(private$artifact_version)
      )
      artifact_files[["files"]]
    }
  )
)

#' @title An Artifact Asset object that was already logged.
#' @description
#' The `LoggedArtifactAsset` represent local or remote asset already logged with
#' particular [`Artifact`] to the Comet.
#' @export
LoggedArtifactAsset <- R6::R6Class(
  cloneable = FALSE,

  "LoggedArtifactAsset",

  inherit = ArtifactAsset,

  public = list(
    #' @description
    #' Creates a new `LoggedArtifactAsset` object with provided parameters.
    #' @param logical_path the logical file name.
    #' @param remote Is the asset a remote asset or not.
    #' @param size The size if the asset of a non-remote asset.
    #' @param metadata The metadata to be associated with the asset.
    #' @param asset_type The type of asset.
    #' @param id The ID of the asset
    #' @param artifact_version_id The ID of Artifact Version associated with this asset.
    #' @param artifact_id The ID of Artifact associated with this asset.
    #' @param experiment_key The experiment key of the experiment that logged this asset.
    #' @param link The remote link if the asset is remote.
    initialize = function(logical_path,
                          remote,
                          size,
                          metadata,
                          asset_type,
                          id,
                          artifact_version_id,
                          artifact_id,
                          experiment_key,
                          link = NULL) {
      super$initialize(
        logical_path = logical_path,
        overwrite = FALSE,
        remote = remote,
        size = size,
        link = link,
        local_path = NULL,
        metadata = metadata,
        asset_type = asset_type
      )

      private$id <- id
      private$artifact_version_id <- artifact_version_id
      private$artifact_id <- artifact_id
      private$experiment_key <- experiment_key
    },

    #' @description
    #' Asset unique ID
    get_id = function() {
      private$id
    },

    #' @description
    #' The ID of Artifact Version associated with this asset
    get_artifact_version_id = function() {
      private$artifact_version_id
    },

    #' @description
    #' The ID of Artifact associated with this asset
    get_artifact_id = function() {
      private$artifact_id
    }
  ),

  private = list(
    id = NULL,
    artifact_version_id = NULL,
    artifact_id = NULL,
    experiment_key = NULL
  )

)
