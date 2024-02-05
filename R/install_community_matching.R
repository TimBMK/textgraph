#' Install the Community Matching Algorithm required for Dynamic Topic Matching
#'
#' Installs the required packages and the matching algorithm into a specified reticulate Python environment.
#' Generally recommended to use the fresh `textgraph` environment when setting up.
#' For the cloning of the Git repository, Git needs to be installed on the system (requires the `git clone` command).
#'
#' @param envname String; Name of the environment used for installation. Defaults to a specific "textgraph" environment
#' @param method See [reticulate::py_install()]
#' @param install_dependencies Logical; Should additionally required packages be installed as well? Note that
#'  the required three packages `numpy`, `pandas`, and `scipy` are set to fixed versions to avoid future conflicts.
#'  Recommended to set this to `false` if using an existing environment for `envname` to not overwrite existing
#'  package versions (but be prepared for errors caused by mismatching package versions).
#' @param ... Additional arguments to pass on to [reticulate::py_install()]
#'
#' @return Sets up a reticulate environment
#' @export
#'
#' @importFrom reticulate py_install use_virtualenv py_discover_config
#'
#' @examples
#' \dontrun{
#' install_community_matching()
#' }
install_community_matching <- function(envname = "textgraph",
                                       method = "auto",
                                       install_dependencies = TRUE,
                                       ...) {

  if (install_dependencies){
    # install python packages
    reticulate::py_install("numpy==1.24.4", # fix package version to ensure updates don't break the installation
                           envname = envname,
                           method = method, ...)

    reticulate::py_install("pandas==2.0.3", # fix package version to ensure updates don't break the installation
                           envname = envname,
                           method = method, ...)

    reticulate::py_install("scipy==1.10.1", # fix package version to ensure updates don't break the installation
                           envname = envname,
                           method = method, ...)
  }

  # install memory community matching
  ## needs to be installed locally, as the git repo is not recognized as a python package

  git_link <- "https://github.com/TimBMK/memory_community_matching.git"

  cat("\nInstalling memory community matching from", git_link, "\n")

  reticulate::use_virtualenv(envname) # switch to environment (seems necessary to get the proper config)

  py_config <- reticulate::py_discover_config() # get config for path and version

  package_path <- file.path(py_config$virtualenv,
                            "lib",
                            paste0("python", py_config$version),
                            "site-packages",
                            "memory_community_matching")

  if(file.exists(package_path)) { # remove if already exists
    cat("Previous installation found. Removing...\n")
    unlink(package_path, recursive = TRUE)
  }

  system(paste("git clone", git_link,
               package_path))

}
