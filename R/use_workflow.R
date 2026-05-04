#' Use an MStargetR Workflow Template
#'
#' Lists available workflow templates or copies one to a specified directory
#' for customisation. Workflow templates are pre-configured R Markdown files
#' that demonstrate common MStargetR pipelines.
#'
#' @param workflow Character string specifying which workflow to use. Use
#'   \code{NULL} (the default) to list all available workflows.
#' @param output_dir Character string specifying the directory to copy the
#'   workflow into. Defaults to the current working directory.
#' @param open Logical indicating whether to open the file after copying.
#'   Defaults to \code{TRUE} in interactive RStudio sessions, \code{FALSE}
#'   otherwise.
#' @param overwrite Logical indicating whether to overwrite an existing
#'   destination file. Defaults to \code{FALSE} so a repeat call does not
#'   silently replace user edits. Set to \code{TRUE} to force replacement.
#'
#' @return If \code{workflow} is \code{NULL}, returns a character vector of
#'   available workflow names (invisibly) and prints them. If a workflow is
#'   specified, returns the path to the copied file (invisibly).
#'
#' @details
#' Available workflows:
#' \describe{
#'   \item{\code{"generic"}}{A template for any user with placeholder
#'     parameters for project path, QC label, MRM templates, and sample tags.}
#'   \item{\code{"CCSM"}}{Pre-configured workflow for ANPC CCSM lipidomics
#'     with built-in MRM templates. Includes single-project and multi-project
#'     batch processing examples.}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # List available workflows
#' use_workflow()
#'
#' # Copy the generic workflow to current directory
#' use_workflow("generic")
#'
#' # Copy the CCSM workflow to a specific directory
#' use_workflow("CCSM", output_dir = "~/my_project")
#' }
use_workflow <- function(workflow = NULL,
                         output_dir = getwd(),
                         open = interactive() &&
                           requireNamespace("rstudioapi", quietly = TRUE),
                         overwrite = FALSE) {

  workflows <- list(
    generic = list(
      file = "workflow_generic.Rmd",
      description = "Generic MStargetR pipeline template (any user)"
    ),
    CCSM = list(
      file = "workflow_HS.Rmd",
      description = "ANPC CCSM lipidomics workflow (single + multi-project)"
    )
  )

  if (is.null(workflow)) {
    message("Available MStargetR workflow templates:\n")
    for (nm in names(workflows)) {
      message("  ", nm, " - ", workflows[[nm]]$description)
    }
    message("\nUse MStargetR::use_workflow(\"", names(workflows)[1],
            "\") to copy a template to your working directory.")
    return(invisible(names(workflows)))
  }

  if (!is.character(workflow) || length(workflow) != 1) {
    stop("'workflow' must be a single character string.", call. = FALSE)
  }

  wf_lower <- tolower(names(workflows))
  workflow_lc <- tolower(workflow)
  if (!workflow_lc %in% wf_lower) {
    stop("'workflow' must be one of: ",
         paste(names(workflows), collapse = ", "),
         ". Got: ", shQuote(workflow), call. = FALSE)
  }
  matched <- workflow_lc
  wf_name <- names(workflows)[match(matched, wf_lower)]
  wf <- workflows[[wf_name]]

  src <- system.file("rmd", wf$file, package = "MStargetR")
  if (src == "") {
    stop("Workflow file not found. Is MStargetR installed correctly?",
         call. = FALSE)
  }

  if (!dir.exists(output_dir)) {
    stop("Output directory does not exist: ", output_dir, call. = FALSE)
  }

  if (file.access(output_dir, mode = 2L) != 0L) {
    stop("Output directory is not writable: ", output_dir, call. = FALSE)
  }

  dest <- file.path(output_dir, wf$file)

  if (file.exists(dest) && !isTRUE(overwrite)) {
    stop("File already exists: ", dest,
         "\nPass overwrite = TRUE to replace it.", call. = FALSE)
  }

  file.copy(src, dest, overwrite = isTRUE(overwrite))
  message("Workflow template copied to: ", dest)
  message("Edit the file to set your project path and parameters, then knit or source it.")

  rstudio_ok <- requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable()
  if (open && rstudio_ok) {
    rstudioapi::navigateToFile(dest)
  }

  invisible(dest)
}
