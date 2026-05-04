#' MStargetR: Targeted MRM Mass Spectrometry Data Processing and Quality Control
#'
#' MStargetR provides a comprehensive workflow for processing targeted multiple
#' reaction monitoring (MRM) mass spectrometry data from raw vendor files through
#' to concentration values ready for statistical analysis.
#'
#' The package includes tools for:
#' \itemize{
#'   \item Raw data conversion from vendor formats to mzML via Docker
#'     (\code{\link{msConvertR}})
#'   \item Peak picking, retention time optimisation, and integration
#'     (\code{\link{PeakForgeR}})
#'   \item Quality control assessment, batch correction, and reporting
#'     (\code{\link{qcCheckR}})
#'   \item Standalone interbatch correction for external data
#'     (\code{\link{batchCorrectR}})
#'   \item An interactive Shiny application for visual workflow management
#'     (\code{\link{launchMStargetR}})
#'   \item Utility functions for transition list validation
#'     (\code{\link{transition_checkR}}, \code{\link{compare_mrm_template_with_guide}})
#' }
#'
#' @section Getting started:
#' See \code{vignette("MStargetR-vignette")} for a comprehensive guide to
#' using the package. The typical workflow is:
#' \enumerate{
#'   \item Set up a project directory with raw vendor files
#'   \item Convert files with \code{\link{msConvertR}}
#'   \item Process peaks with \code{\link{PeakForgeR}}
#'   \item Run quality control with \code{\link{qcCheckR}}
#' }
#'
#' @section Requirements:
#' \itemize{
#'   \item R >= 4.1.0
#'   \item Docker Desktop (for \code{msConvertR} and \code{PeakForgeR})
#'   \item Bioconductor packages: mzR, ropls, statTarget
#' }
#'
#' @seealso
#' Useful links:
#' \itemize{
#'   \item \url{https://github.com/MStargetR/MStargetR}
#'   \item Report bugs at \url{https://github.com/MStargetR/MStargetR/issues}
#' }
#'
#' @name MStargetR-package
#' @aliases MStargetR
"_PACKAGE"
