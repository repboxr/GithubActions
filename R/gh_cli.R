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

gh_auth_status = function() {
  cmd = "gh auth status"
  system(cmd)
}

gh_clone = function(repo, repodir) {
  #gh repo clone <repository> [<directory>] [-- <gitflags>...]
  if (dir.exists(repodir)) {
    cat("\nrepodir ", repodir, " already exists.\n")
    return(invisible())
  }
  cmd = paste0("gh repo clone ", repo," ", localdir)
  system(cmd)
}
