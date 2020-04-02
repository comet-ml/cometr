get_all_system_details <- function() {
  list(
    command = get_system_command(),
    executable = get_system_executable(),
    hostname = get_system_hostname(),
    installedPackages = get_system_packages(),
    os = get_system_os(),
    osType = get_system_osType(),
    pid = get_system_pid(),
    user = get_system_user(),
    pythonVersion = get_system_pythonVersion(),
    pythonVersionVerbose = get_system_pythonVersionVerbose()
  )
}

get_system_command <- function() {
  command <- c(get_system_script(), get_system_args())
  if (is.null(command)) {
    NULL
  } else {
    as.list(command)
  }
}

# Get the full path of the script that was called by the shell
get_system_script <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_idx <- grep("^--file=", args)
  if (length(file_idx) == 1) {
    filename <- sub("^--file=", "", args[file_idx])
    filename <- R.utils::getAbsolutePath(filename, expandTilde = TRUE)
    filename
  } else {
    NULL
  }
}

# Get the command-line arguments that were used to run this script
get_system_args <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) > 0) {
    args
  } else {
    NULL
  }
}

get_system_packages <- function() {
  packages <- utils::installed.packages(noCache = TRUE, priority = "NA")
  packages <- as.data.frame(packages, stringsAsFactors = FALSE)
  rownames(packages) <- NULL
  packages <- packages[, c("Package", "Version")]
  packages <- paste(packages$Package, packages$Version, sep = "@")
  packages
}

get_system_executable <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  if (length(args) >= 1) {
    args[1]
  } else {
    NULL
  }
}

get_system_hostname <- function() {
  hostname <- R.utils::getHostname.System()
  if (length(hostname) >= 1) {
    hostname[[1]]
  } else {
    NULL
  }
}

get_system_pid <- function() {
  Sys.getpid()
}

get_system_pythonVersion <- function() {
  as.character(getRversion())
}

get_system_pythonVersionVerbose <- function() {
  R.version.string
}

get_system_user <- function() {
  user <- R.utils::getUsername.System()
  if (length(user) >= 1) {
    user[[1]]
  } else {
    NULL
  }
}

get_system_osType <- function() {
  as.character(Sys.info()["sysname"])
}

get_system_os <- function() {
  paste(
    as.character(Sys.info()["sysname"]),
    as.character(Sys.info()["release"]),
    as.character(Sys.info()["version"])
  )
}
