#' Login to github with your email and username
gh_login = function(email, username) {
  pat = gh_pat()
  if (is.null(pat)) {
    cat("\nPlease first set the personal access toke via gh_set_pat.")
    return()
  }

  system("gh auth setup-git")
  system("gh auth login")

  cmd = paste0('git config --global user.email "', email,'"')
  system(cmd)

  cmd = paste0('git config --global user.name "', username,'"')
  system(cmd)

}


#' Set a repository secret using github client
#'
#' @param repodir path to your local repository
#' @param name name of the secret
#' @param value of the secret
gh_set_secret = function(repodir, name, value) {
  restore.point("gh_set_secret")
  oldwd = getwd(); setwd(repodir)
  cmd = paste0("gh secret set ", name ,' --body "', value,'"')
  res = system(cmd, intern=TRUE)
  res = gsub(value, "***", res, fixed = TRUE)
  cat("\n",res,"\n")
  setwd(oldwd)
  return(res)
}

#' Remove a secret from the repository
#'
gh_remove_secret = function(repodir, name) {
  restore.point("gh_remove_secret")
  oldwd = getwd(); setwd(repodir)
  cmd = paste0("gh secret deleta ", name)
  system(cmd)
  return(res)
}

#' Get log of a particular workflow run
gh_run_log = function(repodir, runid) {
  oldwd = getwd(); setwd(repodir)
  cmd = paste0("gh run view ", runid," --log")
  res = system(cmd, intern=TRUE)
  setwd(oldwd)
  return(res)
}

#' Create a new repo on github and locally
gh_new_repo = function(reponame, parentdir, access=c("public","private")[2], pat=gh_pat()) {
  restore.point("gh_new_repo")
  oldwd = getwd(); setwd(parentdir)
  # gh repo create [<name>] [flags]
  cmd = paste0("gh repo create ", reponame, " --", access, " --clone")
  cat("\n",cmd,"\n")
  system(cmd)
  cat(paste0("\nLocal repo in ", parentdir,"/", reponame),"\n")
  setwd(oldwd)
}

#' Show authentication status
gh_auth_status = function() {
  cmd = "gh auth status"
  system(cmd)
}

#' Clone a repository on Github to a local directory
gh_clone = function(repo, repodir) {
  #gh repo clone <repository> [<directory>] [-- <gitflags>...]
  if (dir.exists(repodir)) {
    cat("\nrepodir ", repodir, " already exists.\n")
    return(invisible())
  }
  cmd = paste0("gh repo clone ", repo," ", repodir)
  system(cmd)
}
