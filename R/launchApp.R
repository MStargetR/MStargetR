#' Launch MStargetR Shiny Application
#'
#' Opens the MStargetR interactive Shiny application for targeted MRM
#' mass spectrometry data processing and quality control.
#'
#' @param port Integer specifying the port number. Default \code{NULL};
#'   Shiny selects an available port automatically.
#' @param launch.browser Logical indicating whether to open the app in
#'   a web browser. Default is \code{TRUE}.
#' @param host Character string specifying the host address. Default is
#'   \code{"127.0.0.1"} for local-only access. Use \code{"0.0.0.0"} to
#'   allow connections from other machines on the network.
#'
#' @details
#' The GUI requires several additional packages beyond the core MStargetR
#' dependencies: \pkg{shiny}, \pkg{bslib}, \pkg{DT}, \pkg{shinyWidgets},
#' and \pkg{htmltools}. If any are missing, the function will display an
#' informative message listing the packages to install.
#'
#' @section Network deployment:
#' When binding to a non-loopback address (anything other than
#' \code{"127.0.0.1"}, \code{"localhost"}, or \code{"::1"}) to expose the
#' app on a network, be aware that Shiny serves plain HTTP with no built-in
#' authentication. For production or multi-user deployments you should:
#' \itemize{
#'   \item Place the app behind a reverse proxy (e.g. nginx, Apache) that
#'     provides HTTPS/TLS encryption.
#'   \item Configure authentication on the reverse proxy or restrict
#'     access via VPN. Without this, anyone on the network can access
#'     the pipeline and upload files.
#'   \item The upload size limit is automatically reduced to 500 MB for any
#'     non-loopback host (versus 2 GB for loopback) as an additional
#'     safeguard.
#' }
#'
#' @return This function does not return a value. It launches a Shiny
#'   application and blocks the R session until the app is closed.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' launchMStargetR()
#' launchMStargetR(host = "0.0.0.0", port = 3838)
#' }
launchMStargetR <- function(port = NULL,
                            launch.browser = TRUE,
                            host = "127.0.0.1") {

  # Input validation
  if (!is.null(port)) {
    if (!is.numeric(port) || length(port) != 1 || port != as.integer(port) ||
        port < 1 || port > 65535) {
      stop("launchMStargetR: 'port' must be a single integer between 1 and 65535, or NULL. Got: ",
           deparse(port), call. = FALSE)
    }
  }

  if (!is.logical(launch.browser) || length(launch.browser) != 1 || is.na(launch.browser)) {
    stop("launchMStargetR: 'launch.browser' must be TRUE or FALSE. Got: ",
         deparse(launch.browser), call. = FALSE)
  }

  if (!is.character(host) || length(host) != 1 || is.na(host) ||
      nchar(host) == 0) {
    stop("launchMStargetR: 'host' must be a single non-empty character string. Got: ",
         paste(class(host), collapse = ", "), call. = FALSE)
  }
  # Reject hosts that obviously can't parse as an IPv4 dotted quad or as a
  # DNS label sequence. Keep the pattern permissive enough to accept
  # localhost, 127.0.0.1, 0.0.0.0, ::1, and normal hostnames, but refuse
  # things like "999.999.999.999" or "bad!!host" before Shiny returns a
  # cryptic socket error.
  is_valid_host <- function(h) {
    if (identical(h, "localhost") || identical(h, "::1")) return(TRUE)
    # IPv4 dotted quad with 0-255 octets
    ipv4_re <- "^(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)(\\.(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)){3}$"
    if (grepl(ipv4_re, h)) return(TRUE)
    # IPv6: bracket-enclosed (e.g. [::], [fe80::1]) or bare colon-containing
    # address (e.g. ::, fe80::1, 2001:db8::1). Accept any string that
    # contains at least one colon — Shiny/httpuv will report real socket
    # errors for malformed addresses, which is a better user experience than
    # a confusing "not a valid IPv4 or hostname" message.
    if (grepl(":", h, fixed = TRUE)) return(TRUE)
    # DNS label sequence (RFC 1123, simplified). Require at least one letter
    # anywhere so all-numeric strings like "999.999.999.999" that failed the
    # IPv4 octet check above are not accepted as DNS names.
    dns_re <- "^[A-Za-z0-9]([A-Za-z0-9-]{0,62}[A-Za-z0-9])?(\\.[A-Za-z0-9]([A-Za-z0-9-]{0,62}[A-Za-z0-9])?)*$"
    grepl(dns_re, h) && grepl("[A-Za-z]", h)
  }
  if (!is_valid_host(host)) {
    stop("launchMStargetR: 'host' does not look like a valid IPv4 address or hostname: ",
         shQuote(host), call. = FALSE)
  }

  # Check for required GUI packages.
  # The authoritative list lives in DESCRIPTION Suggests; we derive it here
  # so it cannot drift.  The GUI_PKGS constant names the subset of Suggests
  # that the Shiny app actually requires at runtime.
  GUI_PKGS <- c("shiny", "bslib", "DT", "shinyWidgets", "htmltools",
                "plotly", "readr", "ggplot2", "dplyr", "knitr")
  suggests_raw <- tryCatch(
    utils::packageDescription("MStargetR", fields = "Suggests"),
    error = function(e) NA_character_
  )
  if (!is.na(suggests_raw) && nzchar(suggests_raw)) {
    # Parse "pkg (>= x.y), pkg2, ..." into plain package names
    suggests_pkgs <- trimws(gsub("\\s*\\([^)]*\\)", "",
                                  strsplit(suggests_raw, ",")[[1]]))
    gui_deps <- intersect(GUI_PKGS, suggests_pkgs)
    # Retain any GUI_PKGS entry not yet in Suggests (safety net)
    gui_deps <- union(gui_deps, GUI_PKGS)
  } else {
    gui_deps <- GUI_PKGS
  }
  missing <- gui_deps[!vapply(gui_deps, requireNamespace,
                               quietly = TRUE, FUN.VALUE = logical(1))]

  if (length(missing) > 0) {
    stop(
      "The following packages are required for the MStargetR GUI but are not installed:\n",
      paste0("  - ", missing, collapse = "\n"), "\n\n",
      "Install them with:\n",
      "  install.packages(c(", paste0('"', missing, '"', collapse = ", "), "))",
      call. = FALSE
    )
  }

  appDir <- system.file("shiny", "MStargetR_app", package = "MStargetR")


  if (!nzchar(appDir)) {
    stop(
      "Could not find the MStargetR Shiny application directory.\n",
      "Please ensure the package is properly installed.",
      call. = FALSE
    )
  }

  # Allow large uploads (LC-MS files can be hundreds of MB).
  # Use 500 MB for network-exposed instances, 2 GB for local-only.
  # withr::local_options() restores the previous value (or removes the option
  # if it was unset) automatically on function exit, including on error.
  loopback_hosts <- c("127.0.0.1", "localhost", "::1")
  max_size <- if (!host %in% loopback_hosts) 500 * 1024^2 else 2 * 1024^3
  withr::local_options(list(shiny.maxRequestSize = max_size))

  shiny::runApp(
    appDir,
    port = port,
    launch.browser = launch.browser,
    host = host
  )
}
