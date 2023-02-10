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
