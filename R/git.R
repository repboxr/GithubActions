#' Updates the Github repository to the new content of the local repo directory
gh_update = function(repodir, msg="update", remote="origin",branch="main") {
  git_commit_all(repodir, msg)
  git_push(repodir, remote, branch)
}

#' Removes all old commits of a branch and only keeps current local files
#'
#' Based on https://superuser.com/questions/926915/how-can-i-delete-old-commits-in-github-via-terminal
#' WARNING: Don't use for real code repos, in particular not for repos multiple users work with.
gh_remove_history = function(repodir, branch = "main") {
  oldwd = getwd(); setwd(repodir)
  # Make new branch
  system("git checkout --orphan temp_latest_branch")

  # Add all current local files to latest_branch and commit
  system("git add -A")
  system("git commit -m restart")

  # Remove the specified branch
  system(paste0("git branch -D ", branch))

  # Rename temp_latest_branch to specified branch
  system(paste0("git branch -m ", branch))

  # Push changes
  system(paste0("git push -f origin ", branch))

  setwd(oldwd)
}

gh_pull = function(repodir, branch = "main") {
  oldwd = getwd(); setwd(repodir)
  system(paste0("git pull origin ", branch))
  setwd(oldwd)
}

#' Remove a branch from local and github repository
gh_remove_branch = function(repodir, branch) {
  oldwd = getwd(); setwd(repodir)
  system(paste0("git branch -d ", branch))
  system(paste0("git push -d origin ", branch))
  setwd(oldwd)
}

git_commit_all = function(repodir, msg="update") {
  oldwd = getwd(); setwd(repodir)
  cmd = "git add -A"
  system(cmd)

  cmd = paste0('git commit -m "', msg,'"')
  system(cmd)
  setwd(oldwd)
}

git_push = function(repodir, remote="origin", branch="main") {
  oldwd = getwd(); setwd(repodir)
  cmd = paste0('git push origin ', branch)
  system(cmd)
  setwd(oldwd)
}


#' Revert the working tree (and HEAD) to an earlier commit
#'
#' @param repodir Path to the local Git repository.
#' @param commit  Commit hash, tag or any revision that Git accepts.
#' @param keep_local_files Character vector of file paths that should retain
#'        their current contents after the revert (copied out and restored).
#' @param just_files Character vector of file paths that should be taken from
#'        the old commit while everything else stays unchanged.  In this mode
#'        the repository itself is NOT reset.
#' @param commit_msg Commit message used when the function creates a commit.
#'
#' @details
#' * If *just_files* is supplied the function runs
#'   `git checkout <commit> -- <files>`
#'   and then commits the result.
#' * Otherwise it performs `git reset --hard <commit>`.
#'   When *keep_local_files* is given, those files are copied to a temporary
#'   folder beforehand and restored afterwards, followed by a new commit.
#'
#' The function only changes the local repository; push if you need the change
#' on GitHub.
#'
gh_revert_to_commit = function(
  repodir,
  commit,
  keep_local_files = NULL,
  just_files = NULL,
  commit_msg = "revert"
) {
  if (!dir.exists(file.path(repodir, ".git")))
    stop("repodir does not point to a Git repository: ", repodir)

  if (!is.null(keep_local_files) && !is.null(just_files))
    stop("Specify either keep_local_files or just_files, not both.")

  oldwd = getwd()
  on.exit(setwd(oldwd), add = TRUE)
  setwd(repodir)

  # --------------------------------------------------------------------------
  # 1) Only selected files from the old commit
  # --------------------------------------------------------------------------
  if (!is.null(just_files)) {
    files = paste(shQuote(just_files), collapse = " ")
    cmd = paste("git checkout", commit, "--", files)
    system(cmd)

    # commit the result so Git history is linear
    git_commit_all(repodir, msg = paste0("Revert selected files to ", commit))
    return(invisible())
  }

  # --------------------------------------------------------------------------
  # 2) Full reset (optionally keep some local files)
  # --------------------------------------------------------------------------
  tmpdir = NULL
  if (!is.null(keep_local_files)) {
    tmpdir = tempfile("keep_")
    for (f in keep_local_files) {
      src = file.path(repodir, f)
      if (!file.exists(src))
        stop("File listed in keep_local_files not found: ", f)
      dest = file.path(tmpdir, f)
      dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
      file.copy(src, dest, overwrite = TRUE)
    }
  }

  # hard reset
  system(paste("git reset --hard", commit))

  # restore kept files
  if (!is.null(tmpdir)) {
    keep_files = list.files(tmpdir, recursive = TRUE, full.names = TRUE)
    rel_paths = substring(keep_files, nchar(tmpdir) + 2)
    file.copy(keep_files, file.path(repodir, rel_paths), overwrite = TRUE)
  }

  git_commit_all(repodir, msg = paste0(commit_msg, " to ", commit))
  invisible()
}

