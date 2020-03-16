# Get the full path of the script that was called by the shell,
# or "" if it cannot be determined
get_system_script <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_idx <- grep("^--file=", args)
  filename <- ""
  if (length(file_idx) == 1) {
    filename <- sub("^--file=", "", args[file_idx])
    filename <- R.utils::getAbsolutePath(filename, expandTilde = TRUE)
  }
  filename
}

# Get the command-line arguments that were used to run this script
get_system_args <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  args <- paste(args, collapse = " ")
  args
}

get_system_packages <- function() {
  packages <- installed.packages(noCache = TRUE, priority = "NA")
  packages <- as.data.frame(packages, stringsAsFactors = FALSE)
  rownames(packages) <- NULL
  packages <- packages[, c("Package", "Version")]
  packages
}

get_system_executable <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  if (length(args) >= 1) {
    args[1]
  } else {
    ""
  }
}

get_system_hostname <- function() {
  hostname <- R.utils::getHostname.System()
  if (length(hostname) >= 1) {
    hostname[[1]]
  } else {
    ""
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
    ""
  }
}

get_system_osType <- function() {
  as.character(Sys.info()["sysname"])
}

get_system_os <- function() {
  paste(as.character(Sys.info()["sysname"]),
        as.character(Sys.info()["release"]),
        as.character(Sys.info()["version"]))
}
