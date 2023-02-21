# GithubActions

Tools for working with Github and Github Actions from R.

**This repo is still in early development and may substantially change.**

The full functionality requires that you have [git](https://git-scm.com/book/en/v2/Getting-Started-Installing-Git) and [Github cli](https://cli.github.com/manual/installation) installed and that the binaries are on your path.

Before using all functions, you must perform two steps:

1. **Set your Personal Access Token** [https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/creating-a-personal-access-token] using the `gh_set_pat` function. For example, if you stored the PAT in a simple text file `mypat.txt` (not secure), you could write:

```
pat = readLines("~/mypat.txt")
gh_set_pat(pat)
```

The [gh](https://cran.r-project.org/web/packages/gh/index.html) package allows a more secure way to authorize, but I have not yet implemented it. So make sure nobody has access to the file in which you store your PAT.

2. **Login**

Afterwards you should login with your email adress and username using the function `gh_login`.

## Functions

GithubActions contains several helper functions, not all yet well documented. (As I said the package is work in progress and developed mainly for internal use.)

Some functions call `git` or `gh` from the command line. They typically take `repodir` as argument, which is the path to your local repositor.

Other function use the Github REST API and typically take the repo in format `username/reponame` as argument. 
