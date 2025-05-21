#' Upload a single binary asset (e.g. MP3) as its own GitHub release
#'
#' @param repo_dir          Path to the local git repository.
#' @param tag               Tag that identifies the episode / version.
#' @param file              Path to the binary file you want to upload.
#' @param title             Release title (defaults to tag if NULL).
#' @param notes             Release notes (single string). Use "" for none.
#' @param target            Commitish or branch to tag. Default "main".
#' @param prerelease        Logical, mark release as a prerelease.
#' @param draft             Logical, mark release as a draft.
#' @param overwrite         Logical, overwrite existing release (gh --clobber).
#' @param show              Logical, print CLI output.
#' @return                  Character vector with gh CLI output (invisible).
#' @examples
#' gh_upload_binary_file(
#'   repo_dir = ".",
#'   tag      = "jep_38_4_2",
#'   file     = "jep_38_4_2.mp3",
#'   title    = "The Political Economy of Industrial Policy",
#'   notes    = "Show notes here ..."
#' )

gh_upload_binary_file = function(
  repo_dir, tag, file,
  title = NULL, notes = "",
  target = "main", prerelease = FALSE, draft = FALSE,
  overwrite = FALSE, show = TRUE
) {
  # Sanity checks -------------------------------------------------------------
  if (!file.exists(file)) stop("Asset not found: ", file)
  if (!nzchar(tag))       stop("Argument 'tag' must be a non-empty string.")

  old_wd = getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(repo_dir)

  # Build argument vector -----------------------------------------------------
  args = c("release", "create", shQuote(tag), shQuote(file))

  if (!is.null(title))      args = c(args, "--title",       shQuote(title))
  args = c(args, "--notes", shQuote(notes))

  if (target != "main")     args = c(args, "--target",      shQuote(target))
  if (prerelease)           args = c(args, "--prerelease")
  if (draft)                args = c(args, "--draft")
  if (overwrite)            args = c(args, "--clobber")

  cmd = paste("gh", paste(args, collapse = " "))

  if (show) cat("\nRunning:\n", cmd, "\n")
  res = system(cmd, intern = TRUE)
  if (show) cat(res, sep = "\n")

  invisible(res)
}

#' Remove a release created with `gh_upload_binary_file()` and delete its tag
#'
#' @param repo_dir Path to local repository.
#' @param tag      Tag / release name to delete.
#' @param yes      Logical, pass --yes to skip confirmation prompts.
#' @param show     Logical, print CLI output.
#' @return         Character vector with gh CLI output (invisible).
#' @examples
#' gh_remove_binary_file(repo_dir = ".", tag = "jep_38_4_2", yes = TRUE)

gh_remove_binary_file = function(repo_dir, tag, yes = FALSE, show = TRUE) {
  if (!nzchar(tag)) stop("Argument 'tag' must be a non-empty string.")

  old_wd = getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(repo_dir)

  # First attempt to delete the release --------------------------------------
  rel_cmd = paste("gh release delete", shQuote(tag), if (yes) "--yes" else "")
  if (show) cat("\nRunning (release delete):\n", rel_cmd, "\n")
  rel_res = system(rel_cmd, intern = TRUE, ignore.stderr = TRUE)
  if (show) cat(rel_res, sep = "\n")

  # GitHub responds with an error message if no release exists.
  failed = any(grepl("not found", rel_res, ignore.case = TRUE))

  # Always delete the tag afterwards -----------------------------------------
  tag_cmd = paste("gh tag delete", shQuote(tag), if (yes) "--yes" else "")
  if (show) cat("\nRunning (tag delete):\n", tag_cmd, "\n")
  tag_res = system(tag_cmd, intern = TRUE, ignore.stderr = TRUE)
  if (show) cat(tag_res, sep = "\n")

  invisible(c(rel_res, tag_res))
}

# -------------------------------------------------------------------
#' List all releases (tags) of a GitHub repository
#'
#' @param repo_dir    Path to local repo (optional if owner_repo given).
#' @param owner_repo  "owner/repo" string (optional if repo_dir given).
#' @param limit       Max # of releases to fetch (GitHub max 100).
#' @param show        Logical, echo the gh CLI call.
#'
#' @return Character vector of tag names (0-length if none).
#' @examples
#' gh_list_releases(owner_repo = "skranz/jep_podcast")
# -------------------------------------------------------------------
gh_list_releases = function(repo_dir = NULL,
                            owner_repo = NULL,
                            limit      = 100,
                            show       = FALSE) {

  args = c("release", "list",
           "--limit", limit,
           "--json", "tagName",
           "--jq", ".[].tagName")

  if (!is.null(owner_repo)) {
    args = c(args, "-R", owner_repo)
  } else if (!is.null(repo_dir)) {
    if (!file.exists(file.path(repo_dir, ".git")))
        stop("repo_dir does not point to a git repository: ", repo_dir)
  } else {
    repo_dir = getwd()
  }

  old_wd = getwd()
  if (is.null(owner_repo) && !is.null(repo_dir)) {
    on.exit(setwd(old_wd), add = TRUE)
    setwd(repo_dir)
  }

  if (show) cat("\nRunning:\n", "gh", paste(args, collapse = " "), "\n")
  tags = suppressWarnings(system2("gh", args, stdout = TRUE, stderr = TRUE))
  tags = tags[nzchar(tags)]     # drop empty lines

  return(tags)
}

# -------------------------------------------------------------------
#' TRUE/FALSE: does a release with this tag exist?
#'
#' Just a wrapper around gh_list_releases().
# -------------------------------------------------------------------
gh_has_release = function(tag,
                          repo_dir   = NULL,
                          owner_repo = NULL,
                          limit      = 100,
                          show       = FALSE) {

  if (!nzchar(tag)) stop("Argument 'tag' must be a non-empty string.")
  tags = gh_list_releases(repo_dir   = repo_dir,
                          owner_repo = owner_repo,
                          limit      = limit,
                          show       = show)
  tag %in% tags
}

# ------------------------------------------------------------------
# Filename normalizer ----------------------------------------------
# ------------------------------------------------------------------

#' Normalize file names of binary file so that GitHub keeps
#' them unchanged when uploaded as a release.
#' By default every run of non-alphanumeric characters (including spaces)
#' is collapsed into a single underscore. Accents are transliterated to
#' plain ASCII. Leading/trailing separators are trimmed. Multiple
#' separators are collapsed.
#'
#' @param files    Character vector of *existing* file paths.
#' @param dry_run  Logical. If TRUE, do not rename, only print what would happen.
#' @param to_lower Logical. If TRUE, convert the stem (not the extension) to lower case.
#' @param sep      Replacement separator (default "_"). Use "-" or "." if you prefer.
#' @param verbose  Logical. Print each operation.
#' @return         Invisibly returns the vector of new file paths.
#' @examples
#' gh_normalize_files("Episode 1 - Intro.mp3")

gh_normalize_file_names = function(
  files,
  dry_run = FALSE,
  to_lower = FALSE,
  sep = "_",
  verbose = TRUE
) {
  if (length(files)==0) return(files)

  files = normalizePath(files, mustWork = FALSE)
  new_paths = character(length(files))

  for (i in seq_along(files)) {
    f = files[i]
    if (!file.exists(f)) stop("File not found: ", f)

    dir  = dirname(f)
    base = basename(f)
    ext  = tools::file_ext(base)

    stem = sub(paste0("\\.", ext, "$"), "", base)
    if (to_lower) stem = tolower(stem)

    # ASCII transliteration and replacement
    stem = stringi::stri_trans_general(stem, "Latin-ASCII")
    stem = gsub("[^A-Za-z0-9]+", sep, stem)

    # collapse multiple separators and trim edges
    pat  = paste0(sep, "+")
    stem = gsub(pat, sep, stem)
    stem = gsub(paste0("^", sep, "|", sep, "$"), "", stem)

    new_base = paste0(stem, if (nzchar(ext)) paste0(".", ext) else "")
    new_path = file.path(dir, new_base)
    new_paths[i] = new_path

    if (identical(f, new_path)) {
      if (verbose) cat("Already normalized:", new_base, "\n")
      next
    }

    if (!dry_run) {
      ok = file.rename(f, new_path)
      if (!ok) stop("Could not rename ", f, " to ", new_path)
    }
    if (verbose) cat(if (dry_run) "Would rename" else "Renamed", base, "->", new_base, "\n")
  }
  new_paths
}

