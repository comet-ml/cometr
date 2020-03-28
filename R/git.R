get_git_metadata_details <- function() {
  info <- try({
    if (git2r::in_repository()) {
      repo <- git2r::repository()
      branch <- git2r::repository_head(repo)
      config <- git2r::config(repo)
      commit <- git2r::last_commit(repo)
      list(
        branch = branch$name,
        origin = config$local$remote.origin.url,
        parent = git2r::branch_target(branch),
        root = dirname(repo$path),
        user = paste0(commit$author$name, " <", commit$author$email, ">")
      )
    } else {
      list()
    }
  }, silent = TRUE)

  if ("try-error" %in% class(info)) {
    list()
  } else {
    info
  }
}

get_git_patch_file <- function() {
  file <- NULL
  tryCatch({
    if (git2r::in_repository()) {
      repo <- git2r::repository()
      diff <- git2r::diff(repo, as_char = TRUE)
      if (nzchar(diff)) {
        tmpfile <- tempfile(fileext = ".patch")
        writeLines(diff, tmpfile)
        file <- tmpfile
      }
    }
  })
  file
}
