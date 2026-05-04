#' Import specific functions from packages
#' @keywords internal
#' @name install_import_external_functions
#' @importFrom utils install.packages
#' @importFrom remotes install_github
NULL

#' Install MStargetR and Its Dependencies
#'
#' @description Helper function to install MStargetR from GitHub via
#' BiocManager, ensuring all CRAN and Bioconductor dependencies are
#' installed. Existing packages are not upgraded automatically.
#'
#' @return Called for its side effects. Invisibly returns \code{NULL}.
#'
#' @export
#' @examples
#' \dontrun{
#' install_MStargetR()
#' }
install_MStargetR <- function() {
  if (!base::requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager", repos = "https://cloud.r-project.org")

  message("Installing MStargetR and dependencies; this may take several minutes ",
          "on a slow connection...")
  # BiocManager::install delegates GitHub slugs ("owner/repo") to
  # remotes::install_github under the hood, so remotes is a runtime
  # dependency. The unevaluated reference below makes the dependency
  # visible to R CMD check's static analysis (otherwise it would emit
  # "Namespace in Imports field not imported from: 'remotes'") without
  # changing runtime behaviour.
  invisible(remotes::install_github)
  BiocManager::install("MStargetR/MStargetR", ask = FALSE, update = FALSE,
                       build_vignettes = TRUE)
}
