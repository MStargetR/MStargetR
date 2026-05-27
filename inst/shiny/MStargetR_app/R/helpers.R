# MStargetR Shiny App - Shared Helper Functions
# ==============================================================================

# Null-coalescing operator (available in base R >= 4.4.0, polyfill for older).
# Assigned into the app-level environment so it is visible from server.R and
# ui.R regardless of how (and in what local env) helpers.R was sourced.
if (!exists("%||%", envir = .GlobalEnv, mode = "function", inherits = FALSE)) {
  assign("%||%", function(a, b) if (is.null(a)) b else a, envir = .GlobalEnv)
}


#' Safely call a function with error handling for Shiny context
#'
#' Wraps any function call in tryCatch, returning a list with success status,
#' the result or error message, and optional warnings.
#'
#' @param expr An expression to evaluate.
#' @param error_prefix Character string prepended to error messages.
#' @return A list with elements: success (logical), result (any), message (character).
safe_call <- function(expr, error_prefix = "Error") {
  warnings_collected <- character(0)
  result <- tryCatch(
    withCallingHandlers(
      expr,
      warning = function(w) {
        warnings_collected <<- c(warnings_collected, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      msg <- paste0(error_prefix, ": ", conditionMessage(e))
      return(structure(
        list(
          success = FALSE,
          result = NULL,
          message = msg,
          warnings = warnings_collected
        ),
        class = "safe_call_error"
      ))
    }
  )

  if (inherits(result, "safe_call_error")) {
    return(result)
  }

  list(
    success = TRUE,
    result = result,
    message = "Operation completed successfully.",
    warnings = warnings_collected
  )
}


#' Format file size to human-readable string
#'
#' @param size_bytes Numeric file size in bytes.
#' @return Character string (e.g. "1.5 MB").
format_file_size <- function(size_bytes) {
  vapply(size_bytes, function(sb) {
    if (is.na(sb) || sb < 0) return("Unknown")
    if (sb == 0) return("0 B")
    units <- c("B", "KB", "MB", "GB", "TB")
    exp <- min(floor(log(sb, 1024)), length(units) - 1)
    val <- sb / (1024^exp)
    paste(round(val, 1), units[exp + 1])
  }, character(1))
}


#' Create a status badge HTML element
#'
#' @param status One of "success", "warning", "danger", "info", "unknown".
#' @param label Text label to display beside the indicator.
#' @return An htmltools tag object.
create_status_badge <- function(status = "unknown", label = "") {
  color_map <- list(
    success = "#28a745",
    warning = "#ffc107",
    danger  = "#dc3545",
    info    = "#17a2b8",
    unknown = "#6c757d"
  )
  color <- color_map[[status]] %||% color_map[["unknown"]]

  htmltools::tags$span(
    class = "status-badge",
    htmltools::tags$span(
      class = "status-dot",
      style = paste0(
        "display:inline-block;width:10px;height:10px;border-radius:50%;",
        "background-color:", color, ";margin-right:6px;"
      )
    ),
    htmltools::tags$span(label)
  )
}


#' Validate an uploaded file
#'
#' Checks that a file upload input is present and has an allowed extension.
#'
#' @param upload The file input value from Shiny (data frame with name, size, etc.).
#' @param allowed_extensions Character vector of allowed extensions (e.g. c("csv", "tsv")).
#' @param max_size_mb Maximum file size in megabytes.
#' @return A list with valid (logical) and message (character).
validate_upload <- function(upload, allowed_extensions = NULL, max_size_mb = 50) {
  if (is.null(upload)) {
    return(list(valid = FALSE, message = "No file uploaded."))
  }

  ext <- tolower(tools::file_ext(upload$name))

  if (!is.null(allowed_extensions) && !(ext %in% tolower(allowed_extensions))) {
    return(list(
      valid = FALSE,
      message = paste0(
        "Unsupported file type '.", ext, "'. Allowed: ",
        paste0(".", allowed_extensions, collapse = ", ")
      )
    ))
  }

  # Enforce file size limit if specified
  if (is.finite(max_size_mb)) {
    size_mb <- upload$size / (1024 * 1024)
    if (any(size_mb > max_size_mb)) {
      return(list(
        valid = FALSE,
        message = paste0("File too large (", round(max(size_mb), 1),
                         " MB). Maximum allowed: ", max_size_mb, " MB.")
      ))
    }
  }

  list(valid = TRUE, message = "File validated.")
}


#' Check and increment session upload counter for rate limiting
#'
#' Tracks the number of uploads per session and rejects further uploads
#' once the threshold is exceeded.
#'
#' @param rv Reactive values object containing upload_count.
#' @param max_uploads Maximum uploads allowed per session (default 50).
#' @return A list with allowed (logical) and message (character).
check_upload_rate <- function(rv, max_uploads = 50L) {
  count <- rv$upload_count %||% 0L
  if (count >= max_uploads) {
    return(list(
      allowed = FALSE,
      message = paste0("Upload limit reached (", max_uploads,
                       " per session). Please restart the application.")
    ))
  }
  rv$upload_count <- count + 1L
  list(allowed = TRUE, message = "Upload permitted.")
}


#' Get the user preferences file path
#'
#' Preferences are stored under tools::R_user_dir (base R, no external deps).
#' Falls back to tempdir on error.
#'
#' @return Character path to the preferences RDS file.
get_prefs_path <- function() {
  prefs_dir <- tryCatch(
    tools::R_user_dir("MStargetR", "config"),
    error = function(e) file.path(tempdir(), ".MStargetR")
  )
  if (!dir.exists(prefs_dir)) {
    dir.create(prefs_dir, recursive = TRUE, showWarnings = FALSE)
  }
  file.path(prefs_dir, "preferences.rds")
}


#' Save user preferences
#'
#' @param prefs A named list of user preferences.
save_user_preferences <- function(prefs) {
  tryCatch({
    path <- get_prefs_path()
    saveRDS(prefs, path)
  }, error = function(e) {
    message("Could not save preferences: ", e$message)
  })
}


#' Get default user preferences
#'
#' @return A named list of default preference values.
default_user_preferences <- function() {
  list(
    user_name       = "",
    qc_label        = "LTR",
    sample_tags     = "sample,control,blank,qc",
    theme           = "light",
    docker_path     = "docker",
    recent_projects = list()
  )
}


#' Load user preferences
#'
#' @return A named list of user preferences, or defaults if none saved.
load_user_preferences <- function() {
  defaults <- default_user_preferences()

  tryCatch({
    path <- get_prefs_path()
    if (file.exists(path)) {
      saved <- readRDS(path)
      # Guard against corrupted or non-list / unexpected-class preferences.
      # typeof() check runs before any class methods fire so S4/R6
      # deserialisation side-effects cannot reach the merge loop.
      if (typeof(saved) != "list") {
        message("MStargetR: preferences file is corrupted (not a plain list). Using defaults.")
        return(defaults)
      }
      # Accept only string keys whose values are atomic (or a list for
      # recent_projects).  Unknown / complex objects are silently skipped.
      for (nm in names(saved)) {
        val <- saved[[nm]]
        if (nm == "recent_projects") {
          if (is.list(val)) defaults[[nm]] <- val
        } else if (is.atomic(val) && length(val) <= 1L) {
          defaults[[nm]] <- val
        }
      }
    }
    defaults
  }, error = function(e) {
    message("MStargetR: could not load preferences: ", e$message)
    defaults
  })
}


#' Add a project to the recent projects list
#'
#' @param prefs Current preferences list.
#' @param project_path Path to the project directory.
#' @param project_name Display name for the project.
#' @return Updated preferences list.
add_recent_project <- function(prefs, project_path, project_name = NULL) {
  if (is.null(project_name)) {
    project_name <- basename(project_path)
  }
  entry <- list(
    path = project_path,
    name = project_name,
    timestamp = as.character(Sys.time())
  )
  # Remove duplicate
  existing <- prefs$recent_projects
  existing <- Filter(function(x) x$path != project_path, existing)
  # Prepend and limit to 10
  prefs$recent_projects <- c(list(entry), existing)
  if (length(prefs$recent_projects) > 10) {
    prefs$recent_projects <- prefs$recent_projects[1:10]
  }
  prefs
}


#' Check if Docker is available on the system
#'
#' @return A list with available (logical) and version (character or NA).
check_docker_status <- function(docker_path = "docker") {
  # Version-aware system2 wrapper: 'timeout' param requires R >= 4.4.0
  safe_system2 <- function(command, args = character(), ...) {
    dots <- list(...)
    if (!is.null(dots$timeout) && getRversion() < "4.4.0") {
      dots$timeout <- NULL
    }
    do.call(system2, c(list(command = command, args = args), dots))
  }
  tryCatch({
    version_output <- safe_system2(docker_path, "--version",
                                   stdout = TRUE, stderr = FALSE, timeout = 5)
    if (length(version_output) > 0 && nchar(version_output[1]) > 0) {
      # Check if daemon is running
      info <- tryCatch(
        suppressWarnings(safe_system2(docker_path, "info",
                                      stdout = TRUE, stderr = FALSE,
                                      timeout = 10)),
        error = function(e) NULL
      )
      running <- is.character(info) && length(info) > 0 &&
        any(grepl("Server", info))
      list(
        installed = TRUE,
        running = running,
        version = version_output[1]
      )
    } else {
      list(installed = FALSE, running = FALSE, version = NA_character_)
    }
  }, error = function(e) {
    list(installed = FALSE, running = FALSE, version = NA_character_)
  })
}


#' Check if an R package is installed
#'
#' @param pkg Character name of the package.
#' @return Logical.
is_pkg_installed <- function(pkg) {
  requireNamespace(pkg, quietly = TRUE)
}


#' Copy an uploaded Shiny file to a temp path preserving the original filename
#'
#' Shiny stores uploads with opaque temp names (e.g. "0.csv").
#' Pipeline functions rely on basename() and file extension, so this helper
#' copies the temp file to a new temp directory using the original name.
#'
#' @param datapath Character vector of Shiny temp file path(s).
#' @param name Character vector of original filename(s) from the upload.
#' @return Character vector of new file paths with original names preserved.
preserve_upload_names <- function(datapath, name) {
  if (is.null(datapath) || is.null(name)) return(datapath)
  stopifnot(length(datapath) == length(name))
  # tempfile() generates a globally-unique path; avoids the second-granularity
  # collision where two concurrent sessions uploading within the same second
  # would share a directory and overwrite each other's files.
  tmp_dir <- tempfile(pattern = "mstargetr_upload_", tmpdir = tempdir())
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  # Sanitise filename: strip path components and replace unsafe chars.
  # Prevents path-traversal (e.g. "../evil.R") when host = "0.0.0.0".
  safe_name <- basename(gsub("[^A-Za-z0-9._-]", "_", name))
  new_paths <- file.path(tmp_dir, safe_name)
  file.copy(datapath, new_paths, overwrite = TRUE)
  new_paths
}


#' Read a tabular file (CSV or TSV) based on extension
#'
#' @param path File path (may be a Shiny temp path).
#' @param original_name Original filename for extension detection (optional).
#' @return A data frame, or NULL on failure.
read_tabular_file <- function(path, original_name = NULL) {
  if (is.null(path) || !file.exists(path)) return(NULL)
  # Prefer original filename for extension detection (Shiny temp paths may vary)
  ext <- if (!is.null(original_name)) {
    tolower(tools::file_ext(original_name))
  } else {
    tolower(tools::file_ext(path))
  }
  tryCatch({
    if (ext == "csv") {
      readr::read_csv(path, show_col_types = FALSE)
    } else if (ext %in% c("tsv", "txt")) {
      readr::read_tsv(path, show_col_types = FALSE)
    } else {
      # Try tab-delimited first, fall back to CSV
      df <- tryCatch(
        readr::read_tsv(path, show_col_types = FALSE),
        error = function(e) NULL
      )
      if (!is.null(df) && ncol(df) > 1) return(df)
      readr::read_csv(path, show_col_types = FALSE)
    }
  }, error = function(e) {
    NULL
  })
}


# ==============================================================================
# Shared RSD helpers (used by QC Checker and Result Explorer tabs)
# ==============================================================================

#' Extract per-metabolite RSD values from a qcCheckR result object
#'
#' Reads the `filters$rsd` table and returns the named numeric vector of
#' metabolite RSD values for a given pipeline stage. Both the QC Checker
#' histogram and the Result Explorer value-boxes call this helper so both
#' tabs report identical counts.
#'
#' @param qc_result A qcCheckR result list (with `$filters$rsd`).
#' @param stage One of `"concentration"` (pre batch-correction),
#'   `"concentration[statTarget]"` (post batch-correction), or `"peakArea"`.
#' @return A named numeric vector (metabolite -> RSD%), or `NULL` if the
#'   table / requested stage is not available.
get_qc_rsd_values <- function(qc_result,
                              stage = c("concentration",
                                        "concentration[statTarget]",
                                        "peakArea")) {
  stage <- match.arg(stage)
  tbl <- tryCatch(qc_result$filters$rsd, error = function(e) NULL)
  if (is.null(tbl) || !nrow(tbl)) return(NULL)
  row <- tbl[tbl$dataSource == stage & tbl$dataBatch == "allBatches", ,
             drop = FALSE]
  if (!nrow(row)) return(NULL)
  met_cols <- setdiff(names(row), c("dataSource", "dataBatch"))
  if (length(met_cols) == 0) return(NULL)
  vals <- suppressWarnings(as.numeric(unlist(row[1, met_cols],
                                             use.names = FALSE)))
  names(vals) <- met_cols
  vals
}

#' Core Result Explorer RSD computation (pure, testable)
#'
#' Replicates the qcCheckR-backed path of `results_rsd_values()` as a pure
#' function. Prefers post-batch-correction (`concentration[statTarget]`),
#' then pre-correction (`concentration`), then peak-area.
#'
#' @param qc_result A qcCheckR result list.
#' @param stages Ordered character vector of stages to try.
#' @return A named numeric vector of RSD values, or `NULL`.
mstargetr_results_rsd_core <- function(qc_result,
                                       stages = c("concentration[statTarget]",
                                                  "concentration",
                                                  "peakArea")) {
  for (src in stages) {
    vals <- get_qc_rsd_values(qc_result, stage = src)
    if (!is.null(vals)) return(vals)
  }
  NULL
}

#' Log a render error to the session and optionally to rv
#'
#' Centralises the 15+ per-plot tryCatch error handlers so failures surface
#' via showNotification AND are appended to rv$render_errors for aggregated
#' inspection.  Silent req() short-circuits are not notified.
#'
#' @param session Shiny session object (may be NULL outside session scope).
#' @param context Character label identifying the plot/output that failed.
#' @param e The condition object from a tryCatch error handler.
#' @param rv Reactive values object; if non-NULL, errors are appended to
#'   rv$render_errors as a named character vector.
log_shiny_error <- function(session, context, e, rv = NULL) {
  if (inherits(e, "shiny.silent.exception")) return(invisible(NULL))
  msg <- conditionMessage(e)
  message("MStargetR render error [", context, "]: ", msg)
  if (!is.null(rv)) {
    prev <- rv$render_errors %||% character(0)
    rv$render_errors <- c(prev,
      stats::setNames(paste0("[", context, "] ", msg), context))
  }
  if (!is.null(session)) {
    tryCatch(
      shiny::showNotification(
        paste0(context, ": ", msg),
        type = "warning", duration = 6
      ),
      error = function(e2) NULL
    )
  }
  invisible(NULL)
}


#' Swallow only Shiny silent exceptions, re-raise everything else
#'
#' Replacement for blanket `tryCatch(..., error = function(e) NULL)` in
#' presentation-only `render*` handlers. Real errors surface; `req()`
#' short-circuits do not.
#'
#' @param expr An expression to evaluate.
silent_only <- function(expr) {
  tryCatch(expr, error = function(e) {
    if (inherits(e, "shiny.silent.exception")) return(NULL)
    message("MStargetR render error: ", conditionMessage(e))
    NULL
  })
}


#' Return a blank plotly figure with a centred annotation message
#'
#' Shared helper used by renderPlotly blocks when data is absent, eliminating
#' duplicated inline layout() calls.
#'
#' @param msg Character string to display in the centre of the plot area.
#' @return A plotly figure.
empty_plotly <- function(msg = "No data available") {
  plotly::plot_ly() |>
    plotly::layout(
      annotations = list(list(
        x = 0.5, y = 0.5, text = msg,
        showarrow = FALSE, xref = "paper", yref = "paper",
        font = list(size = 13, color = "#64748b")
      ))
    )
}


# ==============================================================================
# Run Button Protection Helpers
# ==============================================================================

#' Disable all run buttons via JS custom message
#' @param session Shiny session object.
disable_run_buttons <- function(session) {
  session$sendCustomMessage("mst-toggle-run-buttons", list(disabled = TRUE))
}

#' Re-enable all run buttons via JS custom message
#' @param session Shiny session object.
enable_run_buttons <- function(session) {
  session$sendCustomMessage("mst-toggle-run-buttons", list(disabled = FALSE))
}


# ==============================================================================
# Centralised Parameter Validation (returns ALL issues at once)
# ==============================================================================

#' Validate File Conversion parameters
#' @return Character vector of issue messages (empty = valid).
validate_convert_params <- function(input_dir, output_dir, docker_status) {
  issues <- character(0)

  if (!nzchar(input_dir %||% "")) {
    issues <- c(issues, "Input directory is required.")
  } else if (!dir.exists(input_dir)) {
    issues <- c(issues, paste0("Input directory does not exist: ", input_dir))
  } else {
    vendor_ext <- c("wiff", "wiff2", "raw", "d", "baf", "lcd", "mbi", "qgd")
    files <- tryCatch(list.files(input_dir, recursive = TRUE), error = function(e) character(0))
    vendor_files <- files[grepl(
      paste0("\\.(", paste(vendor_ext, collapse = "|"), ")$"),
      files, ignore.case = TRUE
    )]
    if (length(vendor_files) == 0) {
      issues <- c(issues, "No vendor instrument files found in the input directory.")
    }
  }

  if (nzchar(output_dir %||% "") && !dir.exists(output_dir)) {
    issues <- c(issues, paste0("Output directory does not exist: ", output_dir))
  }

  if (!docker_status$installed) {
    issues <- c(issues, "Docker is not installed. msConvertR requires Docker with ProteoWizard.")
  } else if (!docker_status$running) {
    issues <- c(issues, "Docker is installed but not running. Please start Docker Desktop.")
  }

  issues
}

#' Validate Peak Integration parameters
#' @return Character vector of issue messages (empty = valid).
validate_peak_params <- function(user_name, project_dir, mrm_upload, qc_label) {
  issues <- character(0)

  # User name
  if (!nzchar(user_name %||% "")) {
    issues <- c(issues, "User Name is required.")
  } else if (grepl("[/\\\\]|\\.\\.", user_name)) {
    issues <- c(issues, "User Name contains invalid path characters (slashes or '..').")
  }

  # Project directory
  if (!nzchar(project_dir %||% "")) {
    issues <- c(issues, "Project Directory is required.")
  } else if (!dir.exists(project_dir)) {
    issues <- c(issues, paste0("Project directory does not exist: ", project_dir))
  } else {
    mzml <- list.files(project_dir, pattern = "\\.mzML$", recursive = TRUE)
    if (length(mzml) == 0) {
      issues <- c(issues, "No .mzML files found in the project directory. Run File Conversion first.")
    }
  }

  # MRM templates
  if (is.null(mrm_upload)) {
    issues <- c(issues, "At least one MRM template file is required.")
  } else {
    check <- validate_upload(mrm_upload, allowed_extensions = c("tsv", "csv", "txt"))
    if (!check$valid) issues <- c(issues, check$message)
  }

  # QC label
  if (!nzchar(qc_label %||% "")) {
    issues <- c(issues, "QC sample label is required.")
  } else if (grepl("[,\\s]", qc_label, perl = TRUE)) {
    issues <- c(issues, "QC sample label should be a single word (no commas or spaces).")
  }

  issues
}

#' Validate QC Check parameters
#' @return Character vector of issue messages (empty = valid).
validate_qc_params <- function(user_name, project_dir, qc_templates,
                               qc_label, sample_tags_str) {
  issues <- character(0)

  if (!nzchar(user_name %||% "")) {
    issues <- c(issues, "User Name is required.")
  } else if (grepl("[/\\\\]|\\.\\.", user_name)) {
    issues <- c(issues, "User Name contains invalid path characters.")
  }

  if (!nzchar(project_dir %||% "")) {
    issues <- c(issues, "Project Directory is required.")
  } else if (!dir.exists(project_dir)) {
    issues <- c(issues, paste0("Project directory does not exist: ", project_dir))
  }

  if (length(qc_templates) == 0) {
    issues <- c(issues,
      "At least one MRM template pair (SIL guide + concentration guide) is required. Use 'Add Method Version'.")
  }

  if (!nzchar(qc_label %||% "")) {
    issues <- c(issues, "QC sample label is required.")
  } else if (grepl("[,\\s]", qc_label, perl = TRUE)) {
    issues <- c(issues, "QC sample label should be a single word (no commas or spaces).")
  }

  # Sample tags
  tags_vec <- trimws(strsplit(sample_tags_str %||% "", ",")[[1]])
  tags_vec <- tags_vec[nzchar(tags_vec)]
  if (length(tags_vec) == 0) {
    issues <- c(issues, "Sample tags field is empty. Enter at least one tag.")
  }

  issues
}

#' Validate Batch Correction parameters
#' @return Character vector of issue messages (empty = valid).
validate_batch_params <- function(data, qc_label, method, project_dir) {
  issues <- character(0)

  if (is.null(data)) {
    issues <- c(issues, "No data available. Upload a file or run QC Check first.")
  } else {
    # Accept either canonical (sample_type) or MStargetR (sample_type_factor) columns
    # bc_preprocess_input maps sample_type_factor -> sample_type and
    # sample_plate_id -> batch before the pipeline runs
    has_sample_type <- "sample_type" %in% names(data) || "sample_type_factor" %in% names(data)
    if (!has_sample_type) {
      issues <- c(issues, "Data is missing required column: sample_type (or sample_type_factor)")
    }
    # Check QC label matches data for QC-based methods (QCRFSC and QC-RLSC),
    # using sample_type_factor if available. ComBat is QC-free.
    st_col <- if ("sample_type_factor" %in% names(data)) data$sample_type_factor else data$sample_type
    if (!is.null(st_col) && nzchar(qc_label %||% "") && method %in% c("QCRFSC", "QCRLSC")) {
      if (!any(tolower(st_col) == tolower(qc_label))) {
        issues <- c(issues, paste0(
          "No samples match QC label '", qc_label,
          "' in the data. ", method, " requires QC samples."))
      }
    }
  }

  if (!nzchar(qc_label %||% "")) {
    issues <- c(issues, "QC sample label is required.")
  }

  if (nzchar(project_dir %||% "") && !dir.exists(project_dir)) {
    issues <- c(issues, paste0("Project directory does not exist: ", project_dir))
  }

  issues
}

#' Coerce a numeric input to a safe value within a range
#'
#' Used to sanitise Shiny numeric inputs that feed directly into pipeline
#' parameters (e.g. `Frule`, `ntree`). NA, non-numeric, or out-of-range
#' values are replaced with `default`; values are clamped to `[min, max]`.
#'
#' @param value Raw input value (may be NULL/NA/non-numeric).
#' @param default Numeric fallback when the value is unusable.
#' @param min,max Numeric range to clamp into.
#' @param integer If TRUE, coerce result to integer.
#' @return A single numeric/integer within `[min, max]`.
safe_numeric <- function(value, default, min = -Inf, max = Inf,
                         integer = FALSE) {
  if (is.null(value) || length(value) == 0) {
    v <- default
  } else {
    v <- suppressWarnings(as.numeric(value[[1]]))
    if (is.na(v) || !is.finite(v)) v <- default
  }
  v <- max(min, min(max, v))
  if (integer) as.integer(round(v)) else v
}

#' Notify once per session about an out-of-range numeric input
#'
#' Called by server code after `safe_numeric()` to surface a single,
#' user-facing warning rather than silently substituting the default.
#'
#' @param session Shiny session (may be NULL).
#' @param rv Reactive values object used to track per-session notifications.
#' @param key Character key identifying the input (e.g. "qc_batch_Frule").
#' @param message Human-readable message.
notify_input_clamp_once <- function(session, rv, key, message) {
  if (is.null(rv)) return(invisible(NULL))
  seen <- rv$notified_clamps %||% character(0)
  if (key %in% seen) return(invisible(NULL))
  rv$notified_clamps <- c(seen, key)
  if (!is.null(session)) {
    tryCatch(
      shiny::showNotification(message, type = "warning", duration = 6),
      error = function(e) NULL
    )
  }
  invisible(NULL)
}

#' Show all validation issues as a single notification
#' @return TRUE if there were issues, FALSE otherwise.
show_validation_issues <- function(session, issues) {
  if (length(issues) == 0) return(FALSE)
  msg <- paste0(
    "Please fix the following before running:\n",
    paste0("\u2022 ", issues, collapse = "\n")
  )
  session$sendCustomMessage("mst-notify", list(
    message = msg, type = "danger", duration = 8000
  ))
  TRUE
}


#' Write a timestamped entry to the audit log
#'
#' Appends a record of pipeline operations to a persistent log file
#' stored in the user's data directory.
#'
#' @param operation Character string identifying the operation (e.g.
#'   "File Conversion", "Peak Integration", "QC Check", "Batch Correction").
#' @param details Character string with additional details about the operation.
#' @param status One of "START", "SUCCESS", "ERROR".
log_audit <- function(operation, details = "", status = "START") {
  log_dir <- tryCatch({
    if (requireNamespace("rappdirs", quietly = TRUE)) {
      rappdirs::user_log_dir("MStargetR", "MStargetR")
    } else {
      file.path(Sys.getenv("HOME", tempdir()), ".MStargetR", "logs")
    }
  }, error = function(e) {
    file.path(tempdir(), "MStargetR_logs")
  })

  if (!dir.exists(log_dir)) {
    dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
  }

  log_file <- file.path(log_dir, paste0("audit_", format(Sys.Date(), "%Y-%m"), ".log"))
  entry <- paste0(
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    " | ", status,
    " | ", operation,
    if (nzchar(details)) paste0(" | ", details) else "",
    "\n"
  )
  tryCatch(
    cat(entry, file = log_file, append = TRUE),
    error = function(e) message("log_audit write failed: ", e$message)
  )
}


# ==============================================================================
# Background pipeline execution (SH-010, SH-013)
# ------------------------------------------------------------------------------
# qcCheckR and batchCorrectR are long-running and used to block the Shiny main
# thread inside withProgress(), which made "Cancel" a placebo and froze other
# reactivity for the pipeline duration. These helpers run the pipeline in a
# callr::r_bg() subprocess and let the app poll for progress + completion.
#
# Usage pattern (server.R):
#   bg <- mst_spawn_pkg_fn("qcCheckR", args_list)
#   rv$process_handle     <- bg$handle
#   rv$process_log_file   <- bg$log_file
#   rv$process_log_offset <- 0L
#   rv$process_task       <- "qc_run"
#
#   # Polling observer (single global observer watches rv$process_handle).
#   tick <- mst_poll_pipeline(rv$process_handle, rv$process_log_file,
#                             rv$process_log_offset)
#   rv$process_log_offset <- tick$new_offset
#   if (tick$done) finalize(tick$result, tick$log_tail)
#
# The subprocess:
#   * Loads MStargetR
#   * Redirects stdout + messages to log_file so the app can tail them
#   * Returns the pipeline result (or raises an error) via callr
# ==============================================================================

#' Spawn a MStargetR pipeline function in a background R process.
#'
#' @param pkg_fn_name Character name of the exported MStargetR function to run
#'   (e.g. "qcCheckR", "batchCorrectR").
#' @param args Named list of arguments to pass to the function.
#' @return A list with:
#'   \item{handle}{A callr r_process handle with methods `is_alive()`,
#'     `get_result()`, `kill()`.}
#'   \item{log_file}{Path to the tempfile capturing stdout / messages from
#'     the subprocess.}
#' @keywords internal
mst_spawn_pkg_fn <- function(pkg_fn_name, args) {
  if (!requireNamespace("callr", quietly = TRUE)) {
    stop("The 'callr' package is required for async pipelines. ",
         "Install.packages('callr').", call. = FALSE)
  }
  log_file <- tempfile("mst_bg_", fileext = ".log")
  file.create(log_file)

  handle <- callr::r_bg(
    func = function(pkg_fn_name, args, log_file) {
      # Redirect stdout + messages into the log so the parent can tail them
      # as a progress stream. Warnings -> messages via withCallingHandlers.
      con <- file(log_file, open = "a")
      sink(con, type = "output")
      sink(con, type = "message")
      on.exit({
        try(sink(NULL, type = "message"), silent = TRUE)
        try(sink(NULL, type = "output"), silent = TRUE)
        try(close(con), silent = TRUE)
      }, add = TRUE)
      requireNamespace("MStargetR", quietly = TRUE)
      pkg_fn <- getExportedValue("MStargetR", pkg_fn_name)
      withCallingHandlers(
        do.call(pkg_fn, args),
        warning = function(w) {
          message("[warning] ", conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      )
    },
    args = list(pkg_fn_name = pkg_fn_name, args = args, log_file = log_file),
    supervise = TRUE
  )

  list(handle = handle, log_file = log_file)
}

#' Read everything appended to a log file since the given byte offset.
#'
#' @param log_file Path to the log file.
#' @param offset Byte offset to resume reading from.
#' @return A list with `text` (character) and `new_offset` (integer).
#' @keywords internal
mst_tail_log <- function(log_file, offset = 0L) {
  if (is.null(log_file) || !file.exists(log_file)) {
    return(list(text = "", new_offset = offset))
  }
  size <- file.size(log_file)
  if (is.na(size) || size <= offset) {
    return(list(text = "", new_offset = offset))
  }
  con <- file(log_file, open = "rb")
  on.exit(try(close(con), silent = TRUE), add = TRUE)
  tryCatch({
    seek(con, where = offset, origin = "start")
    raw <- readBin(con, "raw", n = as.integer(size - offset))
    list(
      text = rawToChar(raw),
      new_offset = as.integer(size)
    )
  }, error = function(e) list(text = "", new_offset = offset))
}

#' Poll a running background pipeline.
#'
#' Call from a reactive observer that re-schedules itself via
#' `shiny::invalidateLater()` while `done == FALSE`.
#'
#' @param handle A callr r_process handle returned from `mst_spawn_pkg_fn()`.
#' @param log_file Path to the subprocess log file.
#' @param offset Last-read byte offset into the log file.
#' @return A list with:
#'   \item{done}{TRUE if the subprocess has exited.}
#'   \item{log_text}{New log content since `offset`.}
#'   \item{new_offset}{Updated byte offset.}
#'   \item{result}{When done: a list with `success`, `value`, `message`.
#'     NULL otherwise.}
#' @keywords internal
mst_poll_pipeline <- function(handle, log_file, offset = 0L) {
  tail <- mst_tail_log(log_file, offset)
  alive <- tryCatch(handle$is_alive(), error = function(e) FALSE)
  if (alive) {
    return(list(
      done = FALSE,
      log_text = tail$text,
      new_offset = tail$new_offset,
      result = NULL
    ))
  }

  # Subprocess exited. Read any final bytes left in the log.
  final <- mst_tail_log(log_file, tail$new_offset)
  combined <- paste0(tail$text, final$text)

  result <- tryCatch(
    list(success = TRUE, value = handle$get_result(), message = NULL),
    error = function(e) list(
      success = FALSE,
      value = NULL,
      message = conditionMessage(e)
    )
  )

  list(
    done = TRUE,
    log_text = combined,
    new_offset = final$new_offset,
    result = result
  )
}

#' Spawn a detached background qs2 save.
#'
#' Used by the QC tab to fire `MStargetR::export_master_list_qs(master_list)`
#' in its own subprocess after `qcCheckR(..., write_rda = FALSE)` returns,
#' so users can view results in the GUI while the save continues.
#' Distinct from `mst_spawn_pkg_fn()` only in that the worker takes a
#' pre-built `master_list` rather than calling a pipeline entry point
#' with raw user inputs — same log/handle contract so the existing
#' `mst_poll_pipeline()` / `mst_cleanup_pipeline()` helpers work unchanged.
#'
#' @param master_list The completed qcCheckR result.
#' @return List with `handle` (callr r_process) and `log_file` (path).
#' @keywords internal
mst_spawn_qs_save <- function(master_list) {
  if (!requireNamespace("callr", quietly = TRUE)) {
    stop("The 'callr' package is required for the detached qs2 save. ",
         "Install.packages('callr').", call. = FALSE)
  }
  log_file <- tempfile("mst_qs_", fileext = ".log")
  file.create(log_file)

  handle <- callr::r_bg(
    func = function(master_list, log_file) {
      con <- file(log_file, open = "a")
      sink(con, type = "output")
      sink(con, type = "message")
      on.exit({
        try(sink(NULL, type = "message"), silent = TRUE)
        try(sink(NULL, type = "output"), silent = TRUE)
        try(close(con), silent = TRUE)
      }, add = TRUE)
      requireNamespace("MStargetR", quietly = TRUE)
      fn <- getExportedValue("MStargetR", "export_master_list_qs")
      withCallingHandlers(
        fn(master_list),
        warning = function(w) {
          message("[warning] ", conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      )
    },
    args = list(master_list = master_list, log_file = log_file),
    supervise = TRUE
  )

  list(handle = handle, log_file = log_file)
}

#' Best-effort cleanup for a finished or cancelled pipeline.
#'
#' @keywords internal
mst_cleanup_pipeline <- function(handle, log_file) {
  if (!is.null(handle)) {
    try(if (handle$is_alive()) handle$kill(), silent = TRUE)
  }
  if (!is.null(log_file) && file.exists(log_file)) {
    try(unlink(log_file), silent = TRUE)
  }
  invisible(NULL)
}
