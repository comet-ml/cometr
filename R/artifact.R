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
#' artifact <- Artifact$new(artifact_name = "Artifact-Name", artifact_type = "Artifact-Type")
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
    #' @title Creates new [`Artifact`] object
    #' @description
    #' Creates new [`Artifact`] object with provided parameters. After that,
    #' the [`Artifact`] object can be used to save assets and can be logged
    #' with an [`Experiment`].
    #' @param artifact_name (Required) Artifact name.
    #' @param artifact_type (Required) The artifact type, for example 'dataset'.
    #' @param artifact_version The version number to create. If not provided,
    #' a new version number will be created automatically.
    #' @param aliases List of aliases. Some aliases to attach to the future Artifact
    #' Version. The aliases list is de-duplicated.
    #' @param metadata Some additional meta-data to attach to the future Artifact Version.
    #' @param version_tags List of tags to be attached to the future Artifact Version.
    initialize = function(artifact_name, artifact_type, artifact_version = NULL,
                          aliases = NULL, metadata = NULL, version_tags = NULL) {
      stopifnot(is.character(artifact_name))
      stopifnot(is.character(artifact_type))

      private$artifact_name <- artifact_name
      private$artifact_type <- artifact_type

      if (!is.null(artifact_version)) {
        private$artifact_version <- numeric_version(artifact_version)
      }

      if (is.vector(aliases)) {
        private$aliases <- unique(aliases)
      } else {
        private$aliases <- c()
      }

      if (is.vector(version_tags)) {
        private$version_tags <- unique(version_tags)
      } else {
        private$version_tags <- c()
      }

      if (is.list(metadata)) {
        private$metadata <- metadata
      } else if(!is.null(metadata)) {
        comet_stop("Invalid metadata, expecting list", metadata)
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
    #' Get the list of assets of the artifact version.
    get_assets = function() {
      private$assets
    },

    #' @description
    #' Add a local asset to the current pending artifact object.
    #' @param local_path Either a file/directory path of the files you want to log
    #' @param overwrite If [`TRUE`] will overwrite all existing assets with the same name.
    #' @param logical_path A custom file name to be displayed. If not
    #' provided the file name from the [`local_path`] argument will be used.
    #' @param metadata Some additional data to attach to the asset.
    add = function(local_path, overwrite=FALSE, logical_path = NULL, metadata = NULL) {
      if (is.null(local_path)) {
        comet_stop("local_path cannot be NULL")
      }
      if (file.exists(local_path)) {
        comet_stop("local_path doesn't exists", local_path)
      }
      if (dir.exists(local_path)) {
        private$add_assets_from_folder(local_path = local_path, overwrite = overwrite,
                                       logical_path = logical_path, metadata = metadata)
      } else {
        private$add_file_asset(local_path = local_path, overwrite = overwrite,
                               logical_path = logical_path, metadata = metadata)
      }
    }

  ),

  private = list(
    artifact_name = NULL,
    artifact_type = NULL,
    artifact_version = NULL,
    aliases = NULL,
    metadata = list(),
    version_tags = NULL,
    assets = list(),

    add_assets_from_folder = function(local_path, overwrite=FALSE,
                                      logical_path = NULL, metadata = NULL) {

    }

    add_file_asset = function(local_path, overwrite=FALSE,
                              logical_path = NULL, metadata = NULL) {
      if (is.null(logical_path)) {
        logical_path <- local_path
      }
      asset = create_asset_from_file(asset_file = local_path,
                                     logical_path = logical_path,
                                     overwrite = overwrite,
                                     metadata = metadata)

    }
  )
)

#' @title An Artifact Asset object
#' @description
#' ArtifactAsset represent local and remote assets added to an Artifact object but not yet uploaded
#' @export
ArtifactAsset <- R6::R6Class(
  cloneable = FALSE,

  "ArtifactAsset",

  public = list(
    initialize = function(logical_path, overwrite = FALSE, remote = FALSE, size = 0,
                          link = NULL, local_path = NULL, metadata = NULL, asset_type = NULL) {
      private$logical_path <- logical_path
      private$remote <- remote
      private$size <- size
      private$link <- link
      private$local_path <- local_path
      private$metadata <- metadata
      private$asset_type <- asset_type
    },

    #' @description
    #' Asset local path if the asset is non-remote
    get_local_path = function() {
      private$local_path
    },

    #' @description
    #' Asset logical file name
    get_logical_path = function() {
      private$logical_path
    },

    #' @description
    #' Is the asset a remote asset or not
    is_remote = function() {
      private$remote
    },

    #' @description
    #' Is the asset will overwrite existing asset with the same name.
    has_overwrite = function() {
      private$overwrite
    },

    #' @description
    #' Asset size if the asset is a non-remote asset
    get_size = function() {
      private$size
    },

    #' @description
    #' Asset remote link if the asset is remote or NULL
    get_link = function() {
      private$link
    },

    #' @description
    #' Asset metadata
    get_metadata = function() {
      private$metadata
    },

    #' @description
    #' Asset type
    get_asset_type = function() {
      if (is.null(private$asset_type)) {
        "asset"
      } else {
        private$asset_type
      }
    }
  ),

  private = list(
    logical_path = NULL,
    overwrite=FALSE,
    remote = FALSE,
    size = 0,
    link = NULL,
    local_path = NULL,
    metadata = NULL,
    asset_type = NULL
  )
)

create_assets_from_folder = function(folder, logical_path, overwrite, metadata) {
  if (is.null(logical_path)) {
    # include root folder into name
    full_names <- TRUE
  } else {
    full_names <- FALSE
  }

  asset_files = list.files(path = folder, recursive = TRUE, full.names = full_names)
  unlist(lapply(asset_files, function(f) {
      if (!is.null(logical_path)) {
        # prefix file name with with logical_path
        logical_file_name <- file.path(logical_path, f)
        asset_file <- file.path(folder, f)
      } else {
        logical_file_name <- f
        asset_file <- f
      }
      create_asset_from_file(asset_file = asset_file, logical_path = logical_file_name,
                             overwrite = overwrite, metadata = metadata)
    })
  )
}

create_asset_from_file = function(asset_file, logical_path, overwrite, metadata) {
  size <- file.size(asset_file)
  ArtifactAsset$new(logical_path = logical_path, overwrite = overwrite,
                    local_path = asset_file, metadata = metadata, size = size)
}
