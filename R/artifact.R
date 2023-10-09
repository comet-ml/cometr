#' @import R6
NULL

#' @title Create Comet Artifact object
#' @description
#' Creates new `Artifact` object with provided parameters. After that,
#' the `Artifact` object can be used to save assets and can be logged
#' with an [`Experiment`].
#' @param artifact_name (Required) Artifact name.
#' @param artifact_type (Required) The artifact type, for example 'dataset'.
#' @param artifact_version The version number to create. If not provided,
#' a new version number will be created automatically.
#' @param aliases List of aliases. Some aliases to attach to the future Artifact
#' Version. The aliases list is normalized to remove duplicates.
#' @param metadata Some additional meta-data to attach to the future Artifact Version.
#' @param version_tags List of tags to be attached to the future Artifact Version.
create_artifact <-
  function(artifact_name,
           artifact_type,
           artifact_version = NULL,
           aliases = NULL,
           metadata = NULL,
           version_tags = NULL) {
    Artifact$new(
      artifact_name = artifact_name,
      artifact_type = artifact_type,
      artifact_version = artifact_version,
      aliases = aliases,
      metadata = metadata,
      version_tags = version_tags
    )
  }

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
    #' @description
    #' Creates new `Artifact` object with provided parameters. After that,
    #' the `Artifact` object can be used to save assets and can be logged
    #' with an [`Experiment`].
    #' @param artifact_name (Required) Artifact name.
    #' @param artifact_type (Required) The artifact type, for example 'dataset'.
    #' @param artifact_version The version number to create. If not provided,
    #' a new version number will be created automatically.
    #' @param aliases List of aliases. Some aliases to attach to the future Artifact
    #' Version. The aliases list is normalized to remove duplicates.
    #' @param metadata Some additional meta-data to attach to the future Artifact Version.
    #' @param version_tags List of tags to be attached to the future Artifact Version.
    initialize = function(artifact_name,
                          artifact_type,
                          artifact_version = NULL,
                          aliases = NULL,
                          metadata = NULL,
                          version_tags = NULL) {
      stopifnot("Artifact name is mandatory" = is.character(artifact_name))
      stopifnot("Artifact type is mandatory" = is.character(artifact_type))

      private$artifact_name <- artifact_name
      private$artifact_type <- artifact_type

      if (!is.null(artifact_version)) {
        private$artifact_version <- numeric_version(artifact_version)
      }

      if (is.list(aliases)) {
        private$aliases <- unique(aliases)
      } else if (!is.null(aliases)) {
        comet_stop("aliases is not a list: ", aliases)
      }

      if (is.list(version_tags)) {
        private$version_tags <- unique(version_tags)
      } else if(!is.null(version_tags)) {
        comet_stop("version_tags is not a list: ", version_tags)
      }

      if (is.list(metadata)) {
        private$metadata <- metadata
      } else if (!is.null(metadata)) {
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
    #' @param local_path (Required) Either a file/directory path of the files you want to log
    #' @param overwrite If [`TRUE`] will overwrite all existing assets with the same name.
    #' @param logical_path A custom file name to be displayed. If not
    #' provided the file name from the `local_path` argument will be used.
    #' @param metadata Some additional data to attach to the asset.
    add = function(local_path,
                   overwrite = FALSE,
                   logical_path = NULL,
                   metadata = NULL) {
      if (is.na(local_path) || is.null(local_path)) {
        comet_stop("local_path can not be NULL")
      }
      if (!file.exists(local_path)) {
        comet_stop("local_path doesn't exists: ", local_path)
      }

      if (dir.exists(local_path)) {
        private$add_assets_from_folder(
          local_path = local_path,
          overwrite = overwrite,
          logical_path = logical_path,
          metadata = metadata
        )
      } else {
        private$add_file_asset(
          local_path = local_path,
          overwrite = overwrite,
          logical_path = logical_path,
          metadata = metadata
        )
      }
      invisible(self)
    },

    #' @description
    #' Add a remote asset to the current pending artifact object. A Remote Asset is an asset but
    #' its content is not uploaded and stored on Comet. Rather a link for its location is stored so
    #' you can identify and distinguish between two experiment using different version of a dataset
    #' stored somewhere else.
    #' @param uri (Required) The remote asset location, there is no imposed format and it could be a
    #' private link.
    #' @param logical_path The "name" of the remote asset, could be a dataset
    #' name, a model file name.
    #' @param overwrite If [`TRUE`] will overwrite all existing assets with the same name.
    #' @param metadata Some additional data to attach to the asset.
    add_remote = function(uri,
                          logical_path = NULL,
                          overwrite = FALSE,
                          metadata = NULL) {
      if (is.na(uri) || is.null(uri)) {
        comet_stop("uri can not be NULL")
      }

      if (is.null(logical_path)) {
        # Try to parse the URI to see if we can extract a useful file name
        logical_path <- remote_asset_name_from_uri(uri)
      }

      asset = ArtifactAsset$new(
        logical_path = logical_path,
        overwrite = overwrite,
        remote = TRUE,
        link = uri,
        metadata = metadata
      )
      private$add_asset(asset)
      invisible(self)
    }

  ),

  private = list(
    artifact_name = NULL,
    artifact_type = NULL,
    artifact_version = NULL,
    aliases = NULL,
    metadata = NULL,
    version_tags = NULL,
    assets = list(),
    assets_names = vector(mode = "character"),

    add_assets_from_folder = function(local_path,
                                      overwrite,
                                      logical_path,
                                      metadata) {
      assets = create_assets_from_folder(
        folder = local_path,
        logical_path = logical_path,
        overwrite = overwrite,
        metadata = metadata
      )
      for (asset in assets) {
        private$add_asset(asset)
      }
    },

    add_file_asset = function(local_path,
                              overwrite,
                              logical_path,
                              metadata) {
      if (is.null(logical_path)) {
        logical_path <- basename(local_path)
      }
      asset = create_asset_from_file(
        asset_file = local_path,
        logical_path = logical_path,
        overwrite = overwrite,
        metadata = metadata
      )
      private$add_asset(asset)
    },

    add_asset = function(asset) {
      logical_path <- asset$get_logical_path()
      if (any(private$assets_names == logical_path)) {
        comet_stop(
          sprintf(
            "Cannot add new asset with logical_path '%s', an existing asset already exists with this logical_path. To add this asset to this artifact you should use a new unique logical_path.",
            logical_path
          )
        )
      } else {
        private$assets = append(private$assets, asset)
        private$assets_names = append(private$assets_names, logical_path)
      }
    }
  )
)

#' @title An Artifact Asset object
#' @description
#' The `ArtifactAsset` represent local or remote asset added to an
#' [`Artifact`] object but not yet uploaded
#' @export
ArtifactAsset <- R6::R6Class(
  cloneable = FALSE,

  "ArtifactAsset",

  public = list(
    #' @description
    #' Creates a new `ArtifactAsset` object with provided parameters.
    #' @param logical_path the logical file name.
    #' @param overwrite If [`TRUE`] will overwrite all existing assets with the same name.
    #' @param remote Is the asset a remote asset or not.
    #' @param size The size if the asset of a non-remote asset.
    #' @param link The remote link if the asset is remote.
    #' @param local_path The local file path if the asset is non-remote.
    #' @param metadata The metadata to be associated with the asset.
    #' @param asset_type The type of asset.
    initialize = function(logical_path,
                          overwrite = FALSE,
                          remote = FALSE,
                          size = 0,
                          link = NULL,
                          local_path = NULL,
                          metadata = NULL,
                          asset_type = NULL) {
      private$logical_path <- logical_path
      private$remote <- remote
      private$size <- size
      private$link <- link
      private$local_path <- local_path
      private$metadata <- metadata
      private$asset_type <- asset_type
      private$overwrite <-  overwrite
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
    overwrite = FALSE,
    remote = FALSE,
    size = 0,
    link = NULL,
    local_path = NULL,
    metadata = NULL,
    asset_type = NULL
  )
)

create_assets_from_folder = function(folder, logical_path, overwrite, metadata) {
  asset_files = list.files(path = folder,
                           recursive = TRUE,
                           full.names = FALSE)
  lapply(asset_files, function(f) {
    if (!is.null(logical_path)) {
      # prefix logical file name with with logical_path
      logical_file_name <- file.path(logical_path, f)
    } else {
      # prefix logical file name with with folder name
      logical_file_name <- file.path(basename(folder), f)
    }
    asset_file <- file.path(folder, f)
    create_asset_from_file(
      asset_file = asset_file,
      logical_path = logical_file_name,
      overwrite = overwrite,
      metadata = metadata
    )
  })
}

create_asset_from_file = function(asset_file,
                                  logical_path,
                                  overwrite,
                                  metadata) {
  size <- file.size(asset_file)
  ArtifactAsset$new(
    logical_path = logical_path,
    overwrite = overwrite,
    local_path = asset_file,
    metadata = metadata,
    size = size
  )
}
