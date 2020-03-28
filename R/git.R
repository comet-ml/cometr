get_git_metadata_details <- function() {
  if (git2r::in_repository()) {
    info <- try({
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
    }, silent = TRUE)
    if ("try-error" %in% class(info)) {
      list()
    } else {
      info
    }
  } else {
    list()
  }
}

