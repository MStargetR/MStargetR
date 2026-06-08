# MStargetR Shiny Application - Server Logic
# ==============================================================================

function(input, output, session) {

  # -- Reactive Values ---------------------------------------------------------
  rv <- shiny::reactiveValues(
    prefs              = load_user_preferences(),
    qc_result          = NULL,
    bc_result          = NULL,
    bc_data            = NULL,
    convert_log        = "",
    peak_log           = "",
    peak_report_files  = NULL,
    qc_log             = "",
    qc_templates       = list(),
    batch_log          = "",
    batch_xlsx_sheets  = NULL,
    batch_source_desc  = NULL,
    running            = FALSE,
    running_task       = "",
    last_convert_click = 0L,
    upload_count       = 0L,
    # Async pipeline (SH-010, SH-013): qcCheckR / batchCorrectR run in a
    # callr::r_bg() subprocess so the Shiny thread stays responsive and
    # Cancel can actually kill the job.
    process_handle     = NULL,
    process_log_file   = NULL,
    process_log_offset = 0L,
    process_task       = NULL,
    process_extra      = list(),
    # Detached qs2 save: the qcCheckR pipeline returns BEFORE the
    # master_list .qs2 save (qcCheckR(..., write_rda = FALSE)). After
    # results are surfaced, a second background subprocess is spawned
    # just to call export_master_list_qs(), tracked in this independent
    # slot so it cannot stall a follow-up qc_run / batch_run.
    qs_handle          = NULL,
    qs_log_file        = NULL,
    qs_log_offset      = 0L,
    qs_project_dir     = NULL,
    qs_started_at      = NULL,
    qs_status          = NULL  # NULL | "running" | "ok" | "error"
  )

  # -- Session cleanup ---------------------------------------------------------
  session$onSessionEnded(function() {
    # Stop the Docker polling observer
    tryCatch(docker_observer$destroy(), error = function(e) NULL)
    # Kill any background pipeline that's still running so we don't orphan
    # an R subprocess holding onto the project directory.
    tryCatch(
      mst_cleanup_pipeline(
        shiny::isolate(rv$process_handle),
        shiny::isolate(rv$process_log_file)
      ),
      error = function(e) NULL
    )
    # Same for the detached qs2 writer. Killing mid-write leaves a
    # truncated/corrupt .qs2 on disk, but that's preferable to leaving
    # an orphaned R subprocess holding the project directory after the
    # browser tab closes.
    tryCatch(
      mst_cleanup_pipeline(
        shiny::isolate(rv$qs_handle),
        shiny::isolate(rv$qs_log_file)
      ),
      error = function(e) NULL
    )
  })

  # -- Health check endpoint ----------------------------------------------------
  # Serves a JSON response at /health for external monitoring tools.
  session$registerDataObj("health", NULL, function(data, req) {
    shiny::httpResponse(
      status = 200L,
      content_type = "application/json",
      content = paste0('{"status":"ok","timestamp":"',
                       format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
                       '"}')
    )
  })

  # -- Cancel run button --------------------------------------------------------
  # qc_run / batch_run run in a callr::r_bg() subprocess (SH-010, SH-013),
  # so cancel can actually terminate the worker. peak_run / convert_run /
  # transition_check remain synchronous for now — the dialog body advises
  # accordingly when those are in flight.
  shiny::observeEvent(input$cancel_run, {
    if (isTRUE(rv$running)) {
      can_kill <- !is.null(rv$process_handle) &&
        rv$process_task %in% c("qc_run", "batch_run")
      if (can_kill) {
        shiny::showModal(shiny::modalDialog(
          title = paste0("Cancel ", rv$running_task, "?"),
          htmltools::tags$p(
            paste0("This will terminate the background worker immediately. ",
                   "Any progress is lost and partial output files may remain ",
                   "in the project directory.")
          ),
          footer = htmltools::tagList(
            shiny::modalButton("Keep running"),
            shiny::actionButton("confirm_cancel", "Yes, cancel",
                                class = "btn btn-danger")
          ),
          easyClose = TRUE
        ))
      } else {
        shiny::showModal(shiny::modalDialog(
          title = "Cancel not supported",
          htmltools::tags$p(
            paste0("The ", rv$running_task, " pipeline runs on the main ",
                   "Shiny thread and cannot be interrupted mid-run.")
          ),
          htmltools::tags$p(
            "Close this dialog and wait for the run to finish, or close the ",
            "browser tab / R session to terminate it."
          ),
          footer = htmltools::tagList(
            shiny::modalButton("Close")
          ),
          easyClose = TRUE
        ))
      }
    }
  })

  # Real kill for the async pipelines. For sync pipelines, confirm_cancel is
  # never reachable (no "Yes, cancel" button is rendered).
  shiny::observeEvent(input$confirm_cancel, {
    shiny::removeModal()
    handle   <- rv$process_handle
    log_file <- rv$process_log_file
    task     <- rv$process_task
    if (is.null(handle)) return()

    mst_cleanup_pipeline(handle, log_file)

    # Route a cancelled-run message to the matching log pane so the user
    # sees why the run stopped.
    msg <- paste0("\n[cancelled] ", rv$running_task %||% "Pipeline",
                  " was cancelled by the user.\n")
    if (identical(task, "qc_run")) {
      rv$qc_log <- paste0(rv$qc_log %||% "", msg)
    } else if (identical(task, "batch_run")) {
      rv$batch_log <- paste0(rv$batch_log %||% "", msg)
    }
    log_audit(rv$running_task %||% "Pipeline", "user cancel", "CANCELLED")

    rv$process_handle     <- NULL
    rv$process_log_file   <- NULL
    rv$process_log_offset <- 0L
    rv$process_task       <- NULL
    rv$process_extra      <- list()
    rv$running            <- FALSE
    rv$running_task       <- ""
    enable_run_buttons(session)

    session$sendCustomMessage("mst-notify", list(
      message = "Run cancelled.", type = "warning", duration = 4000
    ))
  })

  output$global_cancel_ui <- shiny::renderUI({
    if (rv$running) {
      htmltools::tags$div(
        class = "mst-global-cancel",
        shiny::actionButton(
          "cancel_run",
          htmltools::tagList(shiny::icon("stop"), " Cancel ", rv$running_task),
          class = "btn btn-danger"
        )
      )
    }
  })

  # -- Background pipeline poller (SH-010, SH-013) -----------------------------
  # Observes rv$process_handle. While a qc_run / batch_run subprocess is alive
  # it re-arms every 500 ms, tails stdout into the corresponding *_log, and
  # when the process exits it harvests the result, fans the success/error
  # branches into the appropriate reactive slots, and unlocks the UI.
  shiny::observe({
    # The *only* reactive dependency of this observer is rv$process_handle —
    # the handler is kicked when a pipeline is spawned (set) and again
    # during cleanup (cleared). All other rv fields are read via isolate()
    # to prevent a self-invalidation loop that would bypass the 500 ms
    # invalidateLater pacing and spam the subprocess.
    handle <- rv$process_handle
    if (is.null(handle)) return()

    log_file       <- shiny::isolate(rv$process_log_file)
    task           <- shiny::isolate(rv$process_task)
    current_offset <- shiny::isolate(rv$process_log_offset) %||% 0L
    if (is.null(task)) return()

    tick <- mst_poll_pipeline(handle, log_file, current_offset)
    rv$process_log_offset <- tick$new_offset

    if (nzchar(tick$log_text)) {
      if (identical(task, "qc_run")) {
        rv$qc_log <- paste0(rv$qc_log %||% "", tick$log_text)
      } else if (identical(task, "batch_run")) {
        rv$batch_log <- paste0(rv$batch_log %||% "", tick$log_text)
      }
    }

    if (!tick$done) {
      shiny::invalidateLater(500, session)
      return()
    }

    # ---- Pipeline finished: dispatch by task -------------------------------
    result <- tick$result
    extra  <- shiny::isolate(rv$process_extra) %||% list()

    if (identical(task, "qc_run")) {
      if (isTRUE(result$success)) {
        rv$qc_result <- result$value
        rv$qc_log    <- paste0(rv$qc_log %||% "",
                                "\nqcCheckR completed successfully.\n")
        if (!is.null(extra$project_dir)) {
          rv$prefs <- add_recent_project(rv$prefs, extra$project_dir)
          save_user_preferences(rv$prefs)
        }
        log_audit("QC Check", status = "SUCCESS")
        session$sendCustomMessage("mst-notify", list(
          message = "QC pipeline complete!", type = "success"
        ))

        # Detached qs2 write: qcCheckR was invoked with write_rda = FALSE
        # so results are already in rv$qc_result above. Now fire the
        # qs2 save in its own subprocess so the user can interact with
        # the results without waiting on disk I/O. If a previous qs2
        # save is still in flight (rv$qs_handle alive), skip — qs_save()
        # at the end will overwrite the same date-stamped path anyway
        # and we don't want to fan out workers.
        prior_alive <- !is.null(rv$qs_handle) && tryCatch(
          rv$qs_handle$is_alive(), error = function(e) FALSE
        )
        if (prior_alive) {
          rv$qc_log <- paste0(
            rv$qc_log %||% "",
            "\n[note] Previous qs2 save is still running; skipping new ",
            "background save to avoid fan-out. Results are usable now.\n"
          )
        } else {
          # Drop any stale handle before spawning a new one.
          if (!is.null(rv$qs_handle)) {
            tryCatch(mst_cleanup_pipeline(rv$qs_handle, rv$qs_log_file),
                     error = function(e) NULL)
          }
          bg_qs <- tryCatch(
            mst_spawn_qs_save(result$value),
            error = function(e) {
              rv$qc_log <- paste0(
                rv$qc_log %||% "",
                "\n[warning] Could not spawn background qs2 writer: ",
                conditionMessage(e),
                ". The .qs2 file will not be saved this run.\n"
              )
              NULL
            }
          )
          if (!is.null(bg_qs)) {
            rv$qs_handle      <- bg_qs$handle
            rv$qs_log_file    <- bg_qs$log_file
            rv$qs_log_offset  <- 0L
            rv$qs_project_dir <- result$value$project_details$project_dir
            rv$qs_started_at  <- Sys.time()
            rv$qs_status      <- "running"
            rv$qc_log <- paste0(
              rv$qc_log %||% "",
              "\nqs2 save running in background; results are ready to view.\n"
            )
          }
        }
      } else {
        rv$qc_log <- paste0(rv$qc_log %||% "",
                             "\n[error] ", result$message, "\n")
        log_audit("QC Check", result$message %||% "", "ERROR")
        session$sendCustomMessage("mst-notify", list(
          message = result$message %||% "qcCheckR failed",
          type = "danger", duration = 6000
        ))
      }
    } else if (identical(task, "batch_run")) {
      if (isTRUE(result$success)) {
        rv$bc_result <- result$value
        rv$bc_data   <- extra$df
        # Save corrected data to project directory (batch_correction subfolder)
        # so it does NOT overwrite qcCheckR's statTarget output.
        if (identical(extra$data_source, "pipeline") && !is.null(rv$qc_result)) {
          tryCatch({
            proj_dir <- rv$qc_result$project_details$project_dir
            is_unc <- grepl("^\\\\\\\\", proj_dir %||% "")
            is_sym <- !is.na(tryCatch(Sys.readlink(proj_dir %||% ""),
                                       error = function(e) NA)) &&
                      nzchar(tryCatch(Sys.readlink(proj_dir %||% ""),
                                       error = function(e) ""))
            if (!is.null(proj_dir) && dir.exists(proj_dir) &&
                !is_unc && !is_sym &&
                file.access(proj_dir, mode = 2) == 0L) {
              bc_dir <- file.path(proj_dir, "all", "batch_correction")
              if (!dir.exists(bc_dir)) dir.create(bc_dir, recursive = TRUE)
              out_file <- file.path(bc_dir, paste0(
                Sys.Date(), "_batchCorrectR_corrected.csv"
              ))
              readr::write_csv(result$value$corrected_data, out_file)
              rv$batch_log <- paste0(
                rv$batch_log %||% "",
                "\nCorrected data saved to:\n  ", out_file, "\n"
              )
            }
          }, error = function(e) {
            rv$batch_log <- paste0(
              rv$batch_log %||% "",
              "\nNote: Could not auto-save corrected data: ", e$message, "\n"
            )
          })
        }
        src_msg <- tryCatch({
          src <- batch_source_desc_r()
          if (!is.null(src) && nzchar(src)) paste0(" (Source: ", src, ")") else ""
        }, error = function(e) "")
        log_audit("Batch Correction", status = "SUCCESS")
        session$sendCustomMessage("mst-notify", list(
          message = paste0("Batch correction complete!", src_msg),
          type = "success"
        ))
      } else {
        rv$batch_log <- paste0(rv$batch_log %||% "",
                                "\n[error] ", result$message, "\n")
        log_audit("Batch Correction", result$message %||% "", "ERROR")
        session$sendCustomMessage("mst-notify", list(
          message = result$message %||% "batchCorrectR failed",
          type = "danger", duration = 6000
        ))
      }
    }

    # ---- Common cleanup ----------------------------------------------------
    if (!is.null(log_file) && file.exists(log_file)) {
      try(unlink(log_file), silent = TRUE)
    }
    rv$process_handle     <- NULL
    rv$process_log_file   <- NULL
    rv$process_log_offset <- 0L
    rv$process_task       <- NULL
    rv$process_extra      <- list()
    rv$running            <- FALSE
    rv$running_task       <- ""
    enable_run_buttons(session)
  })

  # -- Detached qs2-save poller ------------------------------------------------
  # Independent of the main pipeline poller above. The qcCheckR result is
  # already in rv$qc_result by the time this fires; this observer only
  # tails the worker's log into rv$qc_log, flips rv$qs_status when the
  # subprocess exits, and clears the handle slot so the next QC run can
  # spawn a new qs2 writer. Does NOT toggle rv$running — the user must
  # be free to interact with results, run plots, run batch correction,
  # etc. while the save is in flight.
  shiny::observe({
    handle <- rv$qs_handle
    if (is.null(handle)) return()

    log_file <- shiny::isolate(rv$qs_log_file)
    offset   <- shiny::isolate(rv$qs_log_offset) %||% 0L

    tick <- mst_poll_pipeline(handle, log_file, offset)
    rv$qs_log_offset <- tick$new_offset

    if (nzchar(tick$log_text)) {
      rv$qc_log <- paste0(rv$qc_log %||% "",
                           "[qs2] ", tick$log_text)
    }

    if (!tick$done) {
      shiny::invalidateLater(750, session)
      return()
    }

    if (isTRUE(tick$result$success)) {
      rv$qs_status <- "ok"
      rv$qc_log <- paste0(
        rv$qc_log %||% "",
        "\nBackground qs2 save completed.\n"
      )
      session$sendCustomMessage("mst-notify", list(
        message = "qs2 file saved.", type = "success", duration = 3000
      ))
    } else {
      rv$qs_status <- "error"
      msg <- tick$result$message %||% "unknown error"
      rv$qc_log <- paste0(
        rv$qc_log %||% "",
        "\n[error] Background qs2 save failed: ", msg, "\n"
      )
      session$sendCustomMessage("mst-notify", list(
        message = paste0("Background qs2 save failed: ", msg),
        type = "warning", duration = 6000
      ))
    }

    if (!is.null(log_file) && file.exists(log_file)) {
      try(unlink(log_file), silent = TRUE)
    }
    rv$qs_handle     <- NULL
    rv$qs_log_file   <- NULL
    rv$qs_log_offset <- 0L
  })

  # Banner shown above the QC results tabs while the detached qs2 save
  # is in flight. Once the worker exits, the banner shows a brief
  # success or error state for the next run; clearing it falls out of
  # starting a new qc_run (which sets rv$qs_status back to "running" or
  # NULL).
  output$qc_qs_save_status <- shiny::renderUI({
    status <- rv$qs_status
    if (is.null(status)) return(NULL)
    if (identical(status, "running")) {
      started <- rv$qs_started_at
      elapsed_str <- if (!is.null(started)) {
        secs <- as.numeric(difftime(Sys.time(), started, units = "secs"))
        # Re-render every 2s so the elapsed counter stays roughly current
        # without thrashing reactivity.
        shiny::invalidateLater(2000, session)
        sprintf(" (%.0fs elapsed)", secs)
      } else ""
      htmltools::tags$div(
        class = "alert alert-info d-flex align-items-center gap-2 mb-3",
        shiny::icon("floppy-disk"),
        htmltools::tags$span(
          htmltools::tags$strong("Saving qs2 file in background"),
          elapsed_str,
          " - results below are ready to view; the .qs2 file is still ",
          "being written and will appear in the project's all/data/qs2 ",
          "folder when finished."
        )
      )
    } else if (identical(status, "ok")) {
      htmltools::tags$div(
        class = "alert alert-success d-flex align-items-center gap-2 mb-3",
        shiny::icon("circle-check"),
        htmltools::tags$span("qs2 file saved.")
      )
    } else if (identical(status, "error")) {
      htmltools::tags$div(
        class = "alert alert-warning d-flex align-items-center gap-2 mb-3",
        shiny::icon("triangle-exclamation"),
        htmltools::tags$span(
          "Background qs2 save failed - see console output above. ",
          "XLSX and HTML reports were unaffected."
        )
      )
    }
  })

  # -- Cached computed data (W10) -----------------------------------------------
  # Avoids repeated dplyr::bind_rows() calls across 8+ render functions
  corrected_data <- shiny::reactive({
    shiny::req(rv$qc_result$data$concentration$corrected)
    dplyr::bind_rows(rv$qc_result$data$concentration$corrected)
  })

  # -- Metadata columns constant (W10 support) ---------------------------------
  meta_cols <- c("file_name", "sample_name", "sample_type", "batch",
                 "run_order", "injection_order", "group", "class", "type",
                 "sample_plate_id", "sample_run_index", "sample_type_factor",
                 "sample_type_factor_rev", "sample_plate_order",
                 "sample_matrix", "sample_data_source", "sample_timestamp")

  # -- Cached Docker status (W12) -----------------------------------------------
  docker_status <- shiny::reactiveVal(NULL)
  docker_observer <- shiny::observe({
    docker_path <- rv$prefs$docker_path %||% "docker"
    docker_status(check_docker_status(docker_path))
    shiny::invalidateLater(30000)
  })

  # -- Utility result holders (W11) ---------------------------------------------
  transition_result_val <- shiny::reactiveVal(NULL)
  transition_table_val  <- shiny::reactiveVal(NULL)
  compare_result_val    <- shiny::reactiveVal(NULL)
  compare_table_val     <- shiny::reactiveVal(NULL)
  depcheck_result_val   <- shiny::reactiveVal(NULL)

  # -- Directory pickers (native OS file explorer) -----------------------------
  # Uses the operating system's native folder picker dialog.
  # Windows: PowerShell FolderBrowserDialog (works from background processes)
  # Mac/Linux: tcltk::tk_choose.dir()
  choose_directory <- function(caption = "Select a folder") {
    if (.Platform$OS.type == "windows") {
      # Write PowerShell script to a temp file to avoid command-line quoting
      # issues with $variables, @{hashtables}, and curly braces. Uses -STA for
      # proper Windows Forms threading and -ExecutionPolicy Bypass for the temp
      # script. The parent form is briefly activated to ensure the dialog appears
      # in the foreground instead of behind the browser window.
      ps_script <- sprintf(paste(
        "Add-Type -AssemblyName System.Windows.Forms",
        "[System.Windows.Forms.Application]::EnableVisualStyles()",
        "$form = New-Object System.Windows.Forms.Form",
        "$form.TopMost = $true",
        "$form.ShowInTaskbar = $false",
        "$form.WindowState = 'Minimized'",
        "$form.Width = 0",
        "$form.Height = 0",
        "$form.Show()",
        "$form.Activate()",
        "$form.Hide()",
        "$dlg = New-Object System.Windows.Forms.FolderBrowserDialog",
        "$dlg.Description = '%s'",
        "$dlg.ShowNewFolderButton = $true",
        "$result = $dlg.ShowDialog($form)",
        "$form.Dispose()",
        "if ($result -eq [System.Windows.Forms.DialogResult]::OK) {",
        "  $dlg.SelectedPath",
        "}",
        sep = "\n"
      ), caption)
      tmp_ps1 <- tempfile(fileext = ".ps1")
      writeLines(ps_script, tmp_ps1)
      on.exit(unlink(tmp_ps1), add = TRUE)
      path <- tryCatch({
        result <- system2("powershell",
                          args = c("-NoProfile", "-ExecutionPolicy", "Bypass",
                                   "-STA", "-File", shQuote(tmp_ps1)),
                          stdout = TRUE, stderr = FALSE)
        result <- trimws(paste(result, collapse = ""))
        if (nzchar(result)) result else NA_character_
      }, error = function(e) NA_character_)
    } else {
      path <- tryCatch({
        if (requireNamespace("tcltk", quietly = TRUE)) {
          as.character(tcltk::tk_choose.dir(caption = caption))
        } else {
          session$sendCustomMessage("mst-notify", list(
            message = paste0(
              "Folder picker requires the 'tcltk' package on ",
              Sys.info()[["sysname"]], ". Please type the path manually."
            ),
            type = "warning", duration = 5000
          ))
          NA_character_
        }
      }, error = function(e) NA_character_)
    }
    if (is.na(path) || !nzchar(path)) return(NULL)
    normalizePath(path, winslash = "/", mustWork = FALSE)
  }

  shiny::observeEvent(input$convert_input_dir_browse, {
    path <- choose_directory("Select input directory")
    if (!is.null(path)) shiny::updateTextInput(session, "convert_input_dir", value = path)
  })

  shiny::observeEvent(input$convert_output_dir_browse, {
    path <- choose_directory("Select output directory")
    if (!is.null(path)) shiny::updateTextInput(session, "convert_output_dir", value = path)
  })

  shiny::observeEvent(input$peak_project_dir_browse, {
    path <- choose_directory("Select project directory")
    if (!is.null(path)) shiny::updateTextInput(session, "peak_project_dir", value = path)
  })

  shiny::observeEvent(input$qc_project_dir_browse, {
    path <- choose_directory("Select project directory")
    if (!is.null(path)) shiny::updateTextInput(session, "qc_project_dir", value = path)
  })

  # -- Path validation feedback ------------------------------------------------
  output$convert_input_dir_status <- shiny::renderUI({
    path <- input$convert_input_dir
    if (is.null(path) || !nzchar(path)) return(NULL)
    if (dir.exists(path)) {
      create_status_badge("success", "Directory found")
    } else {
      create_status_badge("danger", "Directory not found")
    }
  })

  output$convert_output_dir_status <- shiny::renderUI({
    path <- input$convert_output_dir
    if (is.null(path) || !nzchar(path)) return(NULL)
    if (dir.exists(path)) {
      create_status_badge("success", "Directory found")
    } else {
      create_status_badge("warning", "Directory does not exist yet (will be created)")
    }
  })

  output$peak_project_dir_status <- shiny::renderUI({
    path <- input$peak_project_dir
    if (is.null(path) || !nzchar(path)) return(NULL)
    if (dir.exists(path)) {
      create_status_badge("success", "Directory found")
    } else {
      create_status_badge("danger", "Directory not found")
    }
  })

  output$qc_project_dir_status <- shiny::renderUI({
    path <- input$qc_project_dir
    if (is.null(path) || !nzchar(path)) return(NULL)
    if (dir.exists(path)) {
      create_status_badge("success", "Directory found")
    } else {
      create_status_badge("danger", "Directory not found")
    }
  })

  # -- Recent Projects quick-select --------------------------------------------
  shiny::observe({
    projects <- rv$prefs$recent_projects
    if (length(projects) == 0) {
      choices <- c("No recent projects" = "")
    } else {
      paths <- vapply(projects, function(p) p$path, character(1))
      names(paths) <- vapply(projects, function(p) {
        paste0(p$name, " (", p$path, ")")
      }, character(1))
      choices <- c("Select a recent project..." = "", paths)
    }
    shiny::updateSelectInput(session, "quick_project", choices = choices)
  })

  shiny::observeEvent(input$quick_project, ignoreInit = TRUE, {
    path <- input$quick_project
    if (nzchar(path)) {
      shiny::updateTextInput(session, "convert_input_dir", value = path)
      shiny::updateTextInput(session, "peak_project_dir", value = path)
      shiny::updateTextInput(session, "qc_project_dir", value = path)
    }
  })

  # -- Dashboard ---------------------------------------------------------------

  output$dash_docker_status <- shiny::renderUI({
    status <- docker_status()
    shiny::req(status)
    if (status$running) {
      create_status_badge("success", paste("Docker:", status$version))
    } else if (status$installed) {
      create_status_badge("warning", "Docker installed but not running")
    } else {
      create_status_badge("danger", "Docker not found")
    }
  })

  output$dash_package_status <- shiny::renderUI({
    core <- c("statTarget", "mzR", "ropls", "dplyr", "ggplot2", "plotly")
    installed <- sum(vapply(core, is_pkg_installed, logical(1)))
    if (all(vapply(core, is_pkg_installed, logical(1)))) {
      create_status_badge("success", paste0(installed, "/", length(core), " packages OK"))
    } else {
      create_status_badge("warning", paste0(installed, "/", length(core), " packages installed"))
    }
  })

  output$dash_recent_projects <- shiny::renderUI({
    projects <- rv$prefs$recent_projects
    if (length(projects) == 0) return(NULL)
    items <- lapply(projects, function(p) {
      htmltools::tags$div(
        class = "d-flex justify-content-between align-items-center py-2 border-bottom",
        htmltools::tags$span(
          htmltools::tags$strong(p$name),
          htmltools::tags$br(),
          htmltools::tags$small(class = "text-muted", p$path)
        ),
        htmltools::tags$small(class = "text-muted", p$timestamp)
      )
    })
    htmltools::tagList(
      htmltools::tags$h5(class = "section-heading mt-4", "Recent Projects"),
      items
    )
  })

  # Navigate to tab when workflow card is clicked
  shiny::observeEvent(input$nav_to, ignoreInit = TRUE, {
    bslib::nav_select("main_nav", input$nav_to, session = session)
  })

  # -- Settings ----------------------------------------------------------------

  shiny::observeEvent(rv$prefs, {
    shiny::updateTextInput(session, "settings_user_name", value = rv$prefs$user_name)
    shiny::updateTextInput(session, "settings_qc_label", value = rv$prefs$qc_label)
    shiny::updateTextInput(session, "settings_sample_tags", value = rv$prefs$sample_tags)
    shiny::updateRadioButtons(session, "settings_theme",
                              selected = rv$prefs$theme %||% "light")
  }, ignoreNULL = FALSE)

  shiny::observeEvent(input$settings_save, {
    rv$prefs$user_name   <- input$settings_user_name
    rv$prefs$qc_label    <- input$settings_qc_label
    rv$prefs$sample_tags <- input$settings_sample_tags

    # Validate docker path: only allow known docker/podman executables to
    # prevent arbitrary binary execution under multi-tenant deployments.
    raw_docker <- input$settings_docker_path %||% "docker"
    allowed_docker_bases <- c("docker", "docker.exe", "podman", "podman.exe")
    if (basename(raw_docker) %in% allowed_docker_bases) {
      rv$prefs$docker_path <- raw_docker
    } else {
      rv$prefs$docker_path <- Sys.which("docker")
      if (!nzchar(rv$prefs$docker_path)) rv$prefs$docker_path <- "docker"
      session$sendCustomMessage("mst-notify", list(
        message = paste0("Docker path '", raw_docker,
          "' is not permitted. Must be 'docker' or 'podman'. Reverted to default."),
        type = "warning", duration = 6000
      ))
    }

    # Persist theme preference and apply it in the browser (SH-020).
    rv$prefs$theme <- input$settings_theme %||% "light"
    save_user_preferences(rv$prefs)
    # Re-check docker with new path
    docker_status(check_docker_status(rv$prefs$docker_path))
    # Apply theme to the live session
    session$sendCustomMessage("mst-set-theme", list(theme = rv$prefs$theme))
    session$sendCustomMessage("mst-notify", list(
      message = "Preferences saved.", type = "success", duration = 3000
    ))
  })

  shiny::observeEvent(input$settings_reset, {
    rv$prefs <- default_user_preferences()
    save_user_preferences(rv$prefs)
    shiny::updateTextInput(session, "settings_user_name", value = "")
    shiny::updateTextInput(session, "settings_qc_label", value = "LTR")
    shiny::updateTextInput(session, "settings_sample_tags", value = "sample,control,blank,qc")
    shiny::updateTextInput(session, "settings_docker_path", value = "docker")
    shiny::updateRadioButtons(session, "settings_theme", selected = "light")
    session$sendCustomMessage("mst-set-theme", list(theme = "light"))
    session$sendCustomMessage("mst-notify", list(
      message = "Settings reset to defaults.", type = "info", duration = 3000
    ))
  })

  # Keep the Settings "Theme" radio in sync when the navbar toggle or
  # keyboard shortcut changes the active theme (reported via Shiny.setInputValue
  # from app.js). Without this observer the radio could show "Light" while
  # the page is actually in dark mode.
  shiny::observeEvent(input$current_theme, {
    shiny::updateRadioButtons(session, "settings_theme",
                              selected = input$current_theme)
    rv$prefs$theme <- input$current_theme
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  output$settings_docker_test <- shiny::renderUI({
    status <- docker_status()
    shiny::req(status)
    if (status$running) {
      create_status_badge("success", paste("Connected:", status$version))
    } else if (status$installed) {
      create_status_badge("warning", "Docker installed but daemon not running")
    } else {
      create_status_badge("danger", "Docker not found at configured path")
    }
  })

  # Sync settings docker path input on load (once only, after first flush).
  # onFlushed is a plain callback (not a reactive consumer), so rv$prefs must
  # be read inside isolate() — otherwise Shiny throws
  # "Can't access reactive value 'prefs' outside of reactive consumer."
  session$onFlushed(function() {
    docker_path <- shiny::isolate(rv$prefs$docker_path) %||% "docker"
    shiny::updateTextInput(session, "settings_docker_path",
                            value = docker_path)
  }, once = TRUE)

  # -- File Conversion ---------------------------------------------------------

  output$convert_docker_indicator <- shiny::renderUI({
    status <- docker_status()
    shiny::req(status)
    if (status$running) {
      create_status_badge("success", paste("Docker:", status$version))
    } else if (status$installed) {
      create_status_badge("warning", "Docker installed but not running")
    } else {
      create_status_badge("danger", "Docker not found")
    }
  })

  shiny::observeEvent(input$convert_run, {
    # -- Run protection guard --
    if (rv$running) {
      session$sendCustomMessage("mst-notify", list(
        message = paste0("Processing already in progress (", rv$running_task,
          "). Please wait for it to finish or cancel and restart."),
        type = "warning", duration = 5000))
      return()
    }

    # Ignore queued clicks from before the last run completed
    click_val <- input$convert_run
    if (click_val <= rv$last_convert_click) return()
    rv$last_convert_click <- click_val

    # -- Comprehensive parameter validation --
    issues <- validate_convert_params(
      input_dir     = input$convert_input_dir,
      output_dir    = input$convert_output_dir,
      docker_status = docker_status()
    )
    if (show_validation_issues(session, issues)) return()

    rv$running      <- TRUE
    rv$running_task <- "File Conversion"
    disable_run_buttons(session)
    log_audit("File Conversion", status = "START")
    on.exit({
      rv$running      <- FALSE
      rv$running_task <- ""
      enable_run_buttons(session)
    }, add = TRUE)

    log_lines <- c()
    ts <- function() format(Sys.time(), "%H:%M:%S")

    # Phase 1: Pre-flight checks (build full log)
    log_lines <- c(log_lines,
      paste0("[", ts(), "] === MStargetR File Conversion ==="),
      paste0("  Input:  ", input$convert_input_dir),
      paste0("  Output: ", if (nzchar(input$convert_output_dir %||% ""))
                             input$convert_output_dir else "(same as input)"),
      ""
    )

    # Scan for vendor files.
    # Lines are accumulated locally then merged so the error path does not use
    # <<- which would race against unconditional log_lines writes further down.
    scan_extra_lines <- tryCatch({
      files <- list.files(input$convert_input_dir, recursive = TRUE)
      vendor_ext <- c("wiff", "wiff2", "raw", "d", "baf", "lcd", "mbi", "qgd")
      vendor_files <- files[grepl(
        paste0("\\.(", paste(vendor_ext, collapse = "|"), ")$"),
        files, ignore.case = TRUE
      )]
      found_lines <- paste0("[", ts(), "] Found ", length(vendor_files), " vendor file(s)")
      for (vf in utils::head(vendor_files, 10)) {
        found_lines <- c(found_lines, paste0("  ", vf))
      }
      if (length(vendor_files) > 10) {
        found_lines <- c(found_lines,
          paste0("  ... and ", length(vendor_files) - 10, " more"))
      }
      c(found_lines, "")
    }, error = function(e) {
      c(paste0("[", ts(), "] Warning: Could not scan directory: ", e$message), "")
    })
    log_lines <- c(log_lines, scan_extra_lines)

    # Docker check
    local_docker <- check_docker_status(rv$prefs$docker_path %||% "docker")
    if (local_docker$running) {
      log_lines <- c(log_lines,
        paste0("[", ts(), "] Docker: Running (", local_docker$version, ")"), "")
    } else if (local_docker$installed) {
      log_lines <- c(log_lines,
        paste0("[", ts(), "] Docker: INSTALLED BUT NOT RUNNING"),
        "  Please start Docker Desktop and try again.", "")
    } else {
      log_lines <- c(log_lines,
        paste0("[", ts(), "] Docker: NOT FOUND"),
        "  msConvertR requires Docker with ProteoWizard.", "")
    }

    log_lines <- c(log_lines,
      paste0("[", ts(), "] Running msConvertR..."),
      "  (This may take several minutes per file)", "")

    # Phase 2: Run conversion and capture all messages
    shiny::withProgress(message = "Converting vendor files...", value = 0.2, {
      captured_messages <- character(0)
      result <- safe_call({
        withCallingHandlers(
          {
            shiny::incProgress(0.3, detail = "Running ProteoWizard conversion...")
            out_dir <- input$convert_output_dir
            if (is.null(out_dir) || !nzchar(out_dir)) out_dir <- input$convert_input_dir
            # Optional plate manifest (raw_file,plateID) for one-file-per-sample
            # vendors. Shiny stores the upload under an opaque temp name; pass the
            # datapath straight to msConvertR(), which reads it by column name.
            manifest_path <- NULL
            mf <- input$convert_manifest
            if (!is.null(mf) && nzchar(mf$datapath %||% "")) {
              manifest_path <- mf$datapath
            }
            MStargetR::msConvertR(
              input_directory = input$convert_input_dir,
              output_directory = out_dir,
              manifest = manifest_path
            )
          },
          message = function(m) {
            captured_messages <<- c(captured_messages, conditionMessage(m))
            invokeRestart("muffleMessage")
          }
        )
      }, error_prefix = "msConvertR")

      # Append captured backend messages
      if (length(captured_messages) > 0) {
        log_lines <- c(log_lines, "--- Backend Output ---",
                       captured_messages, "--- End Backend Output ---", "")
      }

      shiny::incProgress(0.4, detail = "Finalising...")

      if (result$success) {
        out_dir <- input$convert_output_dir
        if (is.null(out_dir) || !nzchar(out_dir)) out_dir <- input$convert_input_dir
        mzml_count <- length(list.files(out_dir, pattern = "\\.mzML$",
                                        recursive = TRUE))
        log_lines <- c(log_lines,
          paste0("[", ts(), "] COMPLETE"),
          paste0("  ", mzml_count, " mzML file(s) created"),
          paste0("  Location: ", out_dir))

        rv$prefs <- add_recent_project(rv$prefs, input$convert_input_dir)
        save_user_preferences(rv$prefs)
        log_audit("File Conversion", paste0(mzml_count, " mzML files"), "SUCCESS")
        session$sendCustomMessage("mst-notify", list(
          message = paste0("Conversion complete! ", mzml_count, " mzML files."),
          type = "success"
        ))
      } else {
        log_lines <- c(log_lines,
          paste0("[", ts(), "] ERROR: ", result$message))
        if (length(result$warnings) > 0) {
          log_lines <- c(log_lines, "  Warnings:",
                         paste0("    - ", result$warnings))
        }
        log_audit("File Conversion", result$message, "ERROR")
        session$sendCustomMessage("mst-notify", list(
          message = result$message, type = "danger", duration = 6000
        ))
      }
    })

    # Set the full log at once (triggers renderText)
    rv$convert_log <- paste(log_lines, collapse = "\n")
  })

  output$convert_console <- shiny::renderText({
    log <- rv$convert_log
    if (is.null(log) || !nzchar(log)) {
      "Console output will appear here when you run a conversion.\nSelect your input directory and click 'Run Conversion' to start."
    } else {
      log
    }
  })

  convert_input_dir_debounced <- shiny::debounce(
    shiny::reactive(input$convert_input_dir), 1000
  )
  output$convert_output_table <- DT::renderDT({
    dir_path <- convert_input_dir_debounced()
    # Gate on existence before debounce pulse to avoid unnecessary disk I/O
    shiny::req(nzchar(dir_path %||% ""), dir.exists(dir_path))
    mzml_files <- list.files(dir_path, pattern = "\\.mzML$", recursive = TRUE)
    if (length(mzml_files) == 0) return(NULL)
    data.frame(
      File = basename(mzml_files),
      Path = mzml_files,
      Size = format_file_size(file.size(file.path(dir_path, mzml_files)))
    )
  }, options = list(pageLength = 10, scrollX = TRUE))
  # Suspend disk I/O when the tab is hidden (default TRUE, but made explicit)
  shiny::outputOptions(output, "convert_output_table", suspendWhenHidden = TRUE)

  # -- Peak Integration --------------------------------------------------------

  output$peak_mrm_preview <- DT::renderDT({
    shiny::req(input$peak_mrm_templates)
    check <- validate_upload(input$peak_mrm_templates,
                             allowed_extensions = c("tsv", "csv", "txt"))
    if (!check$valid) {
      session$sendCustomMessage("mst-notify", list(
        message = check$message, type = "warning", duration = 5000
      ))
      return(NULL)
    }
    named_path <- preserve_upload_names(input$peak_mrm_templates$datapath[1],
                                        input$peak_mrm_templates$name[1])
    df <- read_tabular_file(named_path,
                            original_name = input$peak_mrm_templates$name[1])
    shiny::req(df)
    DT::datatable(df,
                  options = list(pageLength = 5, scrollX = TRUE, scrollY = "220px",
                                 dom = "tip", paging = TRUE),
                  filter = "none",
                  class = "compact stripe",
                  style = "bootstrap4")
  }, server = FALSE)

  # Auto transition check when MRM templates are uploaded
  peak_transition_dupes <- shiny::reactiveVal(NULL)

  shiny::observe({
    shiny::req(input$peak_mrm_templates)
    check <- validate_upload(input$peak_mrm_templates,
                             allowed_extensions = c("tsv", "csv", "txt"))
    if (!check$valid) { peak_transition_dupes(NULL); return() }

    # Check each uploaded template file.
    # Use append pattern (c(all_dupes, list(dupes))) instead of indexed
    # assignment so that NULL-result files (skipped via `next`) do not leave
    # holes in all_dupes — a sparse list passed to do.call(rbind, ...) would
    # silently include NULL elements and produce an error.
    all_dupes <- list()
    for (i in seq_along(input$peak_mrm_templates$name)) {
      named_path <- preserve_upload_names(
        input$peak_mrm_templates$datapath[i],
        input$peak_mrm_templates$name[i]
      )
      df <- read_tabular_file(named_path,
                              original_name = input$peak_mrm_templates$name[i])
      if (is.null(df)) next

      result <- safe_call(
        MStargetR::transition_checkR(df),
        error_prefix = "transition_checkR"
      )
      if (result$success && is.data.frame(result$result) && nrow(result$result) > 0) {
        dupes <- result$result
        dupes$Source <- input$peak_mrm_templates$name[i]
        all_dupes <- c(all_dupes, list(dupes))
      }
    }

    if (length(all_dupes) > 0) {
      peak_transition_dupes(do.call(rbind, all_dupes))
    } else {
      peak_transition_dupes(NULL)
    }
  })

  output$peak_transition_result <- shiny::renderUI({
    shiny::req(input$peak_mrm_templates)
    dupes <- peak_transition_dupes()
    if (is.null(dupes)) {
      create_status_badge("success", "All MRM transitions are unique")
    } else {
      create_status_badge("warning",
        paste(nrow(dupes), "duplicate transition(s) found -- review below"))
    }
  })

  output$peak_transition_has_dupes <- shiny::reactive({
    !is.null(peak_transition_dupes())
  })
  shiny::outputOptions(output, "peak_transition_has_dupes", suspendWhenHidden = FALSE)

  output$peak_transition_table <- DT::renderDT({
    shiny::req(peak_transition_dupes())
    DT::datatable(peak_transition_dupes(),
      options = list(pageLength = 5, scrollX = TRUE, scrollY = "180px",
                     dom = "tip"),
      class = "compact stripe",
      style = "bootstrap4"
    )
  }, server = FALSE)

  shiny::observeEvent(input$peak_run, {
    # -- Run protection guard --
    if (rv$running) {
      session$sendCustomMessage("mst-notify", list(
        message = paste0("Processing already in progress (", rv$running_task,
          "). Please wait for it to finish or cancel and restart."),
        type = "warning", duration = 5000))
      return()
    }

    # -- Comprehensive parameter validation --
    issues <- validate_peak_params(
      user_name  = input$peak_user_name,
      project_dir = input$peak_project_dir,
      mrm_upload = input$peak_mrm_templates,
      qc_label   = input$peak_qc_label
    )
    if (show_validation_issues(session, issues)) return()

    rv$running      <- TRUE
    rv$running_task <- "Peak Integration"
    disable_run_buttons(session)
    log_audit("Peak Integration", status = "START")
    on.exit({
      rv$running      <- FALSE
      rv$running_task <- ""
      enable_run_buttons(session)
    }, add = TRUE)

    shiny::withProgress(message = "Running PeakForgeR...", value = 0, {
      result <- safe_call({
        shiny::incProgress(0.1, detail = "Starting peak integration...")
        # Build template list from uploaded files, preserving original names
        template_paths <- preserve_upload_names(
          input$peak_mrm_templates$datapath,
          input$peak_mrm_templates$name
        )
        mrm_list <- as.list(template_paths)
        names(mrm_list) <- paste0("v", seq_along(mrm_list))

        MStargetR::PeakForgeR(
          user_name = input$peak_user_name,
          project_directory = input$peak_project_dir,
          mrm_template_list = mrm_list,
          QC_sample_label = input$peak_qc_label
        )
      }, error_prefix = "PeakForgeR")

      rv$peak_success <- FALSE
      if (result$success) {
        rv$peak_log <- "PeakForgeR completed successfully."
        rv$peak_success <- TRUE
        rv$prefs <- add_recent_project(rv$prefs, input$peak_project_dir)
        save_user_preferences(rv$prefs)
        log_audit("Peak Integration", status = "SUCCESS")
        session$sendCustomMessage("mst-notify", list(
          message = "Peak integration complete!", type = "success"
        ))
      } else {
        rv$peak_log <- result$message
        log_audit("Peak Integration", result$message, "ERROR")
        session$sendCustomMessage("mst-notify", list(
          message = result$message, type = "danger", duration = 6000
        ))
      }
    })
  })

  output$peak_console <- shiny::renderText({
    log <- rv$peak_log
    if (is.null(log) || !nzchar(log)) {
      "Console output will appear here when you run peak integration.\nSelect your project directory, upload MRM template(s), and click 'Run Peak Integration' to start."
    } else {
      log
    }
  })

  # Pure reactive: compute peak report file list without side effects (SH-008).
  peak_report_files_r <- shiny::reactive({
    shiny::req(rv$peak_log)
    if (!isTRUE(rv$peak_success)) return(NULL)
    proj <- shiny::isolate(input$peak_project_dir)
    shiny::req(proj, dir.exists(proj))
    files <- list.files(proj,
                        pattern = "_PeakForgeR_.*\\.csv$",
                        recursive = TRUE,
                        full.names = TRUE)
    if (length(files) == 0) NULL else files
  })

  output$peak_results_tabs <- shiny::renderUI({
    report_files <- peak_report_files_r()
    if (is.null(report_files)) {
      shiny::req(isTRUE(rv$peak_success))
      return(htmltools::tags$p(class = "text-muted",
        "No PeakForgeR report CSVs found in project directory."))
    }

    # Build tab panels
    tabs <- lapply(seq_along(report_files), function(i) {
      plate_label <- basename(report_files[i]) |>
        sub("^\\d{4}-\\d{2}-\\d{2}_PeakForgeR_", "", x = _) |>
        sub("\\.csv$", "", x = _)
      output_id <- paste0("peak_report_dt_", i)
      bslib::nav_panel(
        title = plate_label,
        htmltools::tags$div(
          class = "pt-2",
          htmltools::tags$p(class = "text-muted small mb-2",
            paste0("Showing first 20 rows of: ", basename(report_files[i]))),
          DT::DTOutput(output_id)
        )
      )
    })

    # Create tabset
    do.call(bslib::navset_pill, c(tabs, list(id = "peak_report_tabset")))
  })

  # Dynamically render a DT for each PeakForgeR report.
  # Track previously bound output IDs and NULL out stale ones (SH-009).
  peak_bound_ids_r <- shiny::reactiveVal(character(0))

  shiny::observe({
    report_files <- peak_report_files_r()
    prev_ids  <- peak_bound_ids_r()
    new_ids   <- if (!is.null(report_files)) {
      paste0("peak_report_dt_", seq_along(report_files))
    } else {
      character(0)
    }

    # Clear stale bindings from the previous run
    for (stale_id in setdiff(prev_ids, new_ids)) {
      output[[stale_id]] <- NULL
    }
    peak_bound_ids_r(new_ids)

    if (is.null(report_files)) return()

    lapply(seq_along(report_files), function(i) {
      output_id <- paste0("peak_report_dt_", i)
      output[[output_id]] <- DT::renderDT({
        df <- tryCatch(
          readr::read_csv(report_files[i], show_col_types = FALSE, n_max = 20),
          error = function(e) data.frame(Error = e$message)
        )
        DT::datatable(df,
          options = list(pageLength = 10, scrollX = TRUE, scrollY = "300px",
                         dom = "tip"),
          class = "compact stripe",
          style = "bootstrap4"
        )
      }, server = FALSE)
    })
  })

  # -- Quality Control ---------------------------------------------------------

  # Template list management
  shiny::observeEvent(input$qc_add_version, {
    shiny::req(input$qc_mrm_file, input$qc_conc_file)

    # SHINY-H7: rate-limit uploads per session before any file handling.
    rl <- check_upload_rate(rv)
    if (!rl$allowed) {
      shiny::showNotification("Upload rate limit exceeded — try again shortly.",
                              type = "error", duration = 6)
      return(invisible(NULL))
    }

    # Enforce explicit per-file size cap independent of shiny.maxRequestSize,
    # so a large TSV cannot OOM the session when read at render time.
    mrm_check <- validate_upload(input$qc_mrm_file,
                                 allowed_extensions = c("tsv", "csv", "txt"),
                                 max_size_mb = 10)
    if (!mrm_check$valid) {
      session$sendCustomMessage("mst-notify", list(
        message = paste("SIL Guide:", mrm_check$message),
        type = "warning", duration = 5000
      ))
      return()
    }

    conc_check <- validate_upload(input$qc_conc_file,
                                  allowed_extensions = c("tsv", "csv", "txt"),
                                  max_size_mb = 10)
    if (!conc_check$valid) {
      session$sendCustomMessage("mst-notify", list(
        message = paste("Concentration Guide:", conc_check$message),
        type = "warning", duration = 5000
      ))
      return()
    }

    idx <- length(rv$qc_templates) + 1
    # Preserve original filenames for pipeline functions that rely on basename()
    mrm_path  <- preserve_upload_names(input$qc_mrm_file$datapath,
                                        input$qc_mrm_file$name)
    conc_path <- preserve_upload_names(input$qc_conc_file$datapath,
                                        input$qc_conc_file$name)
    rv$qc_templates[[paste0("v", idx)]] <- list(
      SIL_guide  = mrm_path,
      conc_guide = conc_path,
      mrm_name   = input$qc_mrm_file$name,
      conc_name  = input$qc_conc_file$name
    )
    session$sendCustomMessage("mst-notify", list(
      message = paste0("Template pair v", idx, " added."), type = "info"
    ))
  })

  output$qc_template_builder <- shiny::renderUI({
    templates <- rv$qc_templates
    if (length(templates) == 0) {
      return(htmltools::tags$p(class = "text-muted", "No templates added yet."))
    }
    items <- lapply(names(templates), function(nm) {
      t <- templates[[nm]]
      htmltools::tags$div(
        class = "d-flex align-items-center py-1 border-bottom",
        create_status_badge("success", ""),
        htmltools::tags$small(
          htmltools::tags$strong(nm), ": ",
          t$mrm_name, " + ", t$conc_name
        )
      )
    })
    htmltools::tagList(items)
  })

  # Cache the heavy file I/O + MStargetR calls keyed on rv$qc_templates so
  # renderUI only re-runs when templates actually change, not on every other
  # upstream invalidation (e.g. slider changes, tab switches).
  qc_compare_cache_r <- shiny::reactive({
    templates <- rv$qc_templates
    if (length(templates) == 0) return(list())
    lapply(names(templates), function(nm) {
      t <- templates[[nm]]
      mrm_df  <- tryCatch(read_tabular_file(t$SIL_guide,  original_name = t$mrm_name),
                          error = function(e) NULL)
      conc_df <- tryCatch(read_tabular_file(t$conc_guide, original_name = t$conc_name),
                          error = function(e) NULL)
      cmp_result <- if (!is.null(mrm_df) && !is.null(conc_df))
        safe_call(MStargetR::compare_mrm_template_with_guide(mrm_df, conc_df),
                  error_prefix = "compare_mrm_template_with_guide")
      else NULL
      tc_result  <- if (!is.null(mrm_df))
        safe_call(MStargetR::transition_checkR(mrm_df),
                  error_prefix = "transition_checkR")
      else NULL
      list(nm = nm, cmp = cmp_result, tc = tc_result)
    })
  })

  # Auto compare MRM template with concentration guide when templates are added
  output$qc_compare_result <- shiny::renderUI({
    cache <- qc_compare_cache_r()
    if (length(cache) == 0) return(NULL)

    results_ui <- lapply(cache, function(item) {
      nm  <- item$nm
      cmp <- item$cmp
      tc  <- item$tc
      compare_ui    <- NULL
      transition_ui <- NULL

      # --- Compare MRM template with concentration guide (cached result) ---
      if (!is.null(cmp)) {
        if (cmp$success) {
          if (is.character(cmp$result) && length(cmp$result) > 0) {
            compare_ui <- htmltools::tags$div(
              class = "mt-1",
              create_status_badge("warning",
                paste0(nm, ": ", length(cmp$result),
                       " unmatched SIL standard(s) in concentration guide")),
              htmltools::tags$details(
                class = "mt-1",
                htmltools::tags$summary(
                  class = "text-warning",
                  style = "cursor:pointer; font-size:0.8rem;",
                  "Show unmatched names"
                ),
                htmltools::tags$ul(class = "mb-0 small",
                  lapply(cmp$result, function(x) htmltools::tags$li(x))
                )
              )
            )
          } else {
            compare_ui <- create_status_badge("success",
              paste0(nm, ": All SIL standards match concentration guide"))
          }
        } else {
          compare_ui <- create_status_badge("danger",
            paste0(nm, " compare: ", cmp$message))
        }
      }

      # --- Transition check on MRM template (cached result) ---
      if (!is.null(tc) && tc$success) {
        if (is.data.frame(tc$result) && nrow(tc$result) > 0) {
          dupe_names <- if ("Precursor Name" %in% names(tc$result)) {
            unique(tc$result$`Precursor Name`)
          } else character(0)
          transition_ui <- htmltools::tags$div(
            class = "mt-1",
            create_status_badge("warning",
              paste0(nm, ": ", nrow(tc$result), " duplicate transition(s)")),
            if (length(dupe_names) > 0) {
              htmltools::tags$details(
                class = "mt-1",
                htmltools::tags$summary(
                  class = "text-warning",
                  style = "cursor:pointer; font-size:0.8rem;",
                  "Show duplicates"
                ),
                htmltools::tags$ul(class = "mb-0 small",
                  lapply(dupe_names, function(x) htmltools::tags$li(x))
                )
              )
            }
          )
        } else {
          transition_ui <- create_status_badge("success",
            paste0(nm, ": All transitions unique"))
        }
      }

      htmltools::tagList(compare_ui, transition_ui)
    })

    if (length(results_ui) > 0) {
      htmltools::tags$div(
        class = "mt-2 mb-2 p-2 border rounded bg-light",
        htmltools::tags$small(class = "fw-semibold text-muted d-block mb-1",
          "Auto-validation Results"),
        htmltools::tagList(results_ui)
      )
    }
  })

  # Populate the Sample Tags field with the ANPC default tag set. Mirrors the
  # fallback in qcCheckR_set_project_details() so the GUI stays in sync with
  # what `qcCheckR(user_name = "ANPC", sample_tags = NULL)` does in R.
  shiny::observeEvent(input$qc_use_anpc_defaults, {
    shiny::updateTextInput(session, "qc_sample_tags",
      value = "pqc,qc,vltr,sltr,ltr,blank,istds,cond,sample")
    session$sendCustomMessage("mst-notify", list(
      message = "Sample tags set to ANPC defaults.",
      type = "info", duration = 3000
    ))
  })

  # SH-010: qcCheckR runs in a callr::r_bg() subprocess so the Shiny thread
  # stays responsive, Cancel can truly kill the worker, and messages stream
  # to the console pane as they happen. Completion is handled by the global
  # `process_poller` observer below.
  shiny::observeEvent(input$qc_run, {
    # -- Run protection guard --
    if (rv$running) {
      session$sendCustomMessage("mst-notify", list(
        message = paste0("Processing already in progress (", rv$running_task,
          "). Please wait for it to finish or cancel and restart."),
        type = "warning", duration = 5000))
      return()
    }

    # -- Comprehensive parameter validation --
    issues <- validate_qc_params(
      user_name       = input$qc_user_name,
      project_dir     = input$qc_project_dir,
      qc_templates    = rv$qc_templates,
      qc_label        = input$qc_qc_label,
      sample_tags_str = input$qc_sample_tags
    )
    if (show_validation_issues(session, issues)) return()

    # Prepare sample tags
    tags_vec <- trimws(strsplit(input$qc_sample_tags, ",")[[1]])
    tags_vec <- tags_vec[nzchar(tags_vec)]
    qc_label <- trimws(input$qc_qc_label)
    if (nzchar(qc_label) && !tolower(qc_label) %in% tolower(tags_vec)) {
      tags_vec <- c(tags_vec, qc_label)
      session$sendCustomMessage("mst-notify", list(
        message = paste0("Auto-added '", qc_label, "' to sample tags list."),
        type = "info", duration = 3000
      ))
    }
    template_list <- lapply(rv$qc_templates, function(t) {
      list(SIL_guide = t$SIL_guide, conc_guide = t$conc_guide)
    })

    # SHINY-H5: sanitise numeric inputs before they reach the pipeline so
    # NA / negative / out-of-range values do not propagate as silent errors.
    qc_ntree <- safe_numeric(input$qc_batch_ntree, default = 500,
                             min = 1, max = 10000, integer = TRUE)
    if (!identical(qc_ntree, as.integer(input$qc_batch_ntree %||% 500))) {
      notify_input_clamp_once(session, rv, "qc_batch_ntree",
        paste0("QC batch ntree adjusted to ", qc_ntree,
               " (allowed range 1-10000)."))
    }
    qc_frule_pct <- safe_numeric(input$qc_batch_Frule, default = 80,
                                 min = 0, max = 100)
    if (!identical(qc_frule_pct, as.numeric(input$qc_batch_Frule %||% 80))) {
      notify_input_clamp_once(session, rv, "qc_batch_Frule",
        paste0("QC Frule adjusted to ", qc_frule_pct,
               "% (allowed range 0-100)."))
    }

    args <- list(
      user_name         = input$qc_user_name,
      project_directory = input$qc_project_dir,
      mrm_template_list = template_list,
      QC_sample_label   = input$qc_qc_label,
      sample_tags       = tags_vec,
      mv_threshold      = input$qc_mv_threshold,
      lod_threshold     = safe_numeric(input$qc_lod_threshold, default = 5000,
                                       min = 0),
      batch_method      = input$qc_batch_method,
      batch_ntree       = qc_ntree,
      batch_coCV        = input$qc_batch_coCV,
      batch_Frule       = qc_frule_pct / 100,
      batch_imputeM     = input$qc_batch_imputeM,
      combat_par.prior  = input$qc_combat_par_prior %||% TRUE,
      combat_mean.only  = input$qc_combat_mean_only %||% FALSE,
      combat_ref.batch  = if (nzchar(input$qc_combat_ref_batch %||% "")) {
        input$qc_combat_ref_batch
      } else NULL,
      qcrlsc_method     = input$qc_qcrlsc_method %||% "subtract",
      qcrlsc_intra      = input$qc_qcrlsc_intra  %||% FALSE,
      qcrlsc_opti       = input$qc_qcrlsc_opti   %||% TRUE,
      qcrlsc_log10      = input$qc_qcrlsc_log10  %||% TRUE,
      qcrlsc_outl       = input$qc_qcrlsc_outl   %||% TRUE,
      qcrlsc_shift      = input$qc_qcrlsc_shift  %||% TRUE,
      # batch_column drives ComBat and QC-RLSC; read the input belonging to the
      # selected method (each lives in its own conditionalPanel).
      batch_column      = {
        bc <- if ((input$qc_batch_method %||% "QCRFSC") == "QCRLSC") {
          input$qc_qcrlsc_batch_column %||% ""
        } else {
          input$qc_batch_column %||% ""
        }
        if (nzchar(bc)) bc else NULL
      },
      # The qs2 save is the slowest tail-end step in the pipeline. Skip
      # it here so qcCheckR returns the in-memory result as soon as the
      # XLSX/HTML exports finish, then the qc_run completion handler
      # (above) spawns a separate background subprocess that calls
      # export_master_list_qs(master_list) so the user can view results
      # immediately while the save runs in the background.
      write_rda         = FALSE
    )

    bg <- tryCatch(
      mst_spawn_pkg_fn("qcCheckR", args),
      error = function(e) {
        session$sendCustomMessage("mst-notify", list(
          message = paste0("Could not start background worker: ",
                           conditionMessage(e)),
          type = "danger", duration = 6000
        ))
        NULL
      }
    )
    if (is.null(bg)) return()

    log_audit("QC Check", status = "START")
    rv$qc_log             <- "Starting qcCheckR in background worker...\n"
    rv$running            <- TRUE
    rv$running_task       <- "QC Check"
    rv$process_handle     <- bg$handle
    rv$process_log_file   <- bg$log_file
    rv$process_log_offset <- 0L
    rv$process_task       <- "qc_run"
    rv$process_extra      <- list(project_dir = input$qc_project_dir)
    # Clear any stale post-save banner from a previous run. The detached
    # qs2 writer sets this to "running" once it spawns, post-completion.
    rv$qs_status         <- NULL
    disable_run_buttons(session)
  })

  output$qc_console <- shiny::renderText({
    log <- rv$qc_log
    if (is.null(log) || !nzchar(log)) {
      "Console output will appear here when you run the QC pipeline.\nFill in all required fields and click 'Run QC Check' to start."
    } else {
      log
    }
  })

  output$qc_summary_table <- DT::renderDT({
    shiny::validate(shiny::need(rv$qc_result, "Run the QC pipeline first to see the summary."))
    tryCatch({
      if (!is.null(rv$qc_result$summary_tables$projectOverview)) {
        rv$qc_result$summary_tables$projectOverview
      } else {
        data.frame(Info = "No summary data available.")
      }
    }, error = function(e) data.frame(Info = e$message))
  }, options = list(pageLength = 25, scrollX = TRUE))

  output$qc_pca_plot <- plotly::renderPlotly({
    shiny::validate(shiny::need(rv$qc_result, "Run the QC pipeline first to see the PCA plot."))
    tryCatch({
      # PCA plots are stored under $pca$plot keyed by fill variable
      pca_plot <- rv$qc_result$pca$plot$sample_type_factor
      if (is.null(pca_plot)) {
        pca_plot <- rv$qc_result$pca$plot$sample_plate_id
      }
      if (is.null(pca_plot)) {
        # Try first available plot
        plots <- rv$qc_result$pca$plot
        if (length(plots) > 0) pca_plot <- plots[[1]]
      }
      shiny::req(pca_plot)
      plotly::ggplotly(pca_plot)
    }, error = function(e) {
      plotly::plot_ly() |>
        plotly::layout(
          title = "PCA plot unavailable",
          annotations = list(list(
            text = paste("Error:", e$message), showarrow = FALSE,
            xref = "paper", yref = "paper", x = 0.5, y = 0.5
          ))
        )
    })
  })

  # Update control chart metabolite selector when QC result is available
  shiny::observe({
    shiny::req(rv$qc_result)
    mets <- names(rv$qc_result$control_charts)
    if (length(mets) > 0) {
      shiny::updateSelectInput(session, "qc_controlchart_metabolite",
                                choices = mets, selected = mets[1])
    }
  })

  # Run order plots (PCA scores run order from qcCheckR)
  output$qc_runorder_plot <- plotly::renderPlotly({
    shiny::req(rv$qc_result, input$qc_runorder_pc)
    silent_only({
      pc <- input$qc_runorder_pc
      p <- rv$qc_result$pca$scoresRunOrder[[pc]]
      if (is.null(p)) return(empty_plotly("No run order data available"))
      plotly::ggplotly(p)
    })
  })

  # Control chart plots (per metabolite from qcCheckR)
  output$qc_controlchart_plot <- plotly::renderPlotly({
    shiny::req(rv$qc_result, input$qc_controlchart_metabolite)
    silent_only({
      met <- input$qc_controlchart_metabolite
      p <- rv$qc_result$control_charts[[met]]
      if (is.null(p)) return(empty_plotly("No control chart data available"))
      plotly::ggplotly(p)
    })
  })

  # Dedicated reactive for QC RSD data frame — depends only on qc_result,
  # not on the threshold slider, so slider changes only redraw the threshold
  # annotation layer without re-computing all RSDs (SH-011).
  qc_rsd_df_r <- shiny::reactive({
    shiny::req(rv$qc_result)
    rsd_df <- NULL
    rsd_vec <- get_qc_rsd_values(rv$qc_result, stage = "concentration")
    if (!is.null(rsd_vec) && length(rsd_vec) > 0) {
      rsd_df <- data.frame(
        metabolite = names(rsd_vec),
        rsd = as.numeric(rsd_vec),
        stringsAsFactors = FALSE
      )
      rsd_df <- rsd_df[!is.na(rsd_df$rsd), ]
    }
    if ((is.null(rsd_df) || nrow(rsd_df) == 0) &&
        !is.null(rv$qc_result$data$concentration$corrected)) {
      df <- corrected_data()
      num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
      num_cols <- setdiff(num_cols, meta_cols)
      rsds <- vapply(num_cols, function(m) {
        v <- df[[m]][!is.na(df[[m]])]
        if (length(v) < 2 || mean(v) == 0) return(NA_real_)
        sd(v) / mean(v) * 100
      }, numeric(1))
      rsd_df <- data.frame(metabolite = names(rsds), rsd = as.numeric(rsds),
                            stringsAsFactors = FALSE)
      rsd_df <- rsd_df[!is.na(rsd_df$rsd), ]
    }
    rsd_df
  })

  # Cached reactive for pre-imputation peak area data used by qc_missing_plot.
  # Keyed on rv$qc_result so multiple renders sharing this data do not each
  # call dplyr::bind_rows() over the full plate list independently.
  peak_area_sorted_data_r <- shiny::reactive({
    shiny::req(rv$qc_result)
    if (!is.null(rv$qc_result$data$peakArea$sorted)) {
      dplyr::bind_rows(rv$qc_result$data$peakArea$sorted)
    } else {
      NULL
    }
  })

  # Cached reactive for imputed concentration data (fallback path in missing plot).
  imputed_concentration_data_r <- shiny::reactive({
    shiny::req(rv$qc_result)
    if (!is.null(rv$qc_result$data$concentration$imputed)) {
      dplyr::bind_rows(rv$qc_result$data$concentration$imputed)
    } else {
      NULL
    }
  })

  # RSD distribution histogram
  output$qc_rsd_histogram <- plotly::renderPlotly({
    shiny::req(rv$qc_result)
    silent_only({
      rsd_df <- qc_rsd_df_r()

      # SHINY-H6: render a labelled empty plot instead of a blank canvas
      if (is.null(rsd_df) || nrow(rsd_df) == 0) {
        return(
          plotly::plot_ly() |>
            plotly::layout(annotations = list(list(
              x = 0.5, y = 0.5, text = "No data available",
              showarrow = FALSE, xref = "paper", yref = "paper"
            )))
        )
      }

      # RSD-3: read the same threshold reactive the Result Explorer uses
      # so both tabs share the fail cut-off. Threshold is consumed here as a
      # separate layer so slider drag doesn't re-run the RSD computation.
      fail_thr <- rsd_fail_threshold()

      plotly::plot_ly(
        data = rsd_df, x = ~rsd, type = "histogram",
        nbinsx = 30,
        marker = list(color = "#377EB8", line = list(color = "white", width = 0.5)),
        hovertemplate = "RSD: %{x:.1f}%<br>Count: %{y}<extra></extra>"
      ) |>
        plotly::layout(
          xaxis = list(title = "RSD %"),
          yaxis = list(title = "Number of Metabolites"),
          shapes = list(
            list(type = "line", x0 = fail_thr, x1 = fail_thr, y0 = 0, y1 = 1,
                 yref = "paper",
                 line = list(color = "red", dash = "dash", width = 1.5))
          ),
          annotations = list(
            list(x = fail_thr + 1, y = 1, yref = "paper",
                 text = paste0(fail_thr, "% threshold"),
                 showarrow = FALSE, xanchor = "left",
                 font = list(color = "red", size = 11))
          )
        )
    })
  })

  # Missing value pattern plot
  # Uses pre-imputation sorted data to show original missing value pattern
  output$qc_missing_plot <- plotly::renderPlotly({
    shiny::req(rv$qc_result)
    silent_only({
      # Use cached bind_rows results to avoid repeating O(plates * rows * cols)
      # work on every invalidation (e.g. when other reactive values change).
      df <- peak_area_sorted_data_r()
      if (is.null(df) || nrow(df) == 0) {
        df <- imputed_concentration_data_r()
      }
      shiny::req(df, nrow(df) > 0)
      num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
      num_cols <- setdiff(num_cols, meta_cols)
      shiny::req(length(num_cols) > 0)

      pct_missing <- vapply(num_cols, function(m) {
        sum(is.na(df[[m]])) / nrow(df) * 100
      }, numeric(1))

      mv_df <- data.frame(metabolite = names(pct_missing),
                            pct = as.numeric(pct_missing),
                            stringsAsFactors = FALSE)
      # Only show metabolites that actually have missing values
      mv_df <- mv_df[mv_df$pct > 0, ]

      if (nrow(mv_df) == 0) {
        return(
          plotly::plot_ly() |>
            plotly::layout(
              annotations = list(list(
                text = "No missing values detected in the dataset",
                showarrow = FALSE, xref = "paper", yref = "paper",
                x = 0.5, y = 0.5, font = list(size = 14, color = "#64748b")
              ))
            )
        )
      }

      mv_df <- mv_df[order(mv_df$pct, decreasing = TRUE), ]
      mv_df <- utils::head(mv_df, 40)
      mv_df$metabolite <- factor(mv_df$metabolite, levels = rev(mv_df$metabolite))

      colours <- ifelse(mv_df$pct > 50, "#E41A1C",
                 ifelse(mv_df$pct > 20, "#FF7F00", "#377EB8"))

      plotly::plot_ly(
        data = mv_df, y = ~metabolite, x = ~pct,
        type = "bar", orientation = "h",
        marker = list(color = colours),
        hovertemplate = "%{y}: %{x:.1f}% missing<extra></extra>"
      ) |>
        plotly::layout(
          xaxis = list(title = "Missing Values (%)", range = c(0, 100)),
          yaxis = list(title = "", tickfont = list(size = 9)),
          margin = list(l = 150)
        )
    })
  })

  # Sample type pie chart
  output$qc_sample_type_pie <- plotly::renderPlotly({
    shiny::req(rv$qc_result)
    silent_only({
      df <- NULL
      if (!is.null(rv$qc_result$data$concentration$corrected)) {
        df <- corrected_data()
      }
      shiny::req(df, "sample_type_factor" %in% names(df) || "sample_type" %in% names(df))
      type_col <- if ("sample_type_factor" %in% names(df)) "sample_type_factor" else "sample_type"
      counts <- as.data.frame(table(df[[type_col]]), stringsAsFactors = FALSE)
      names(counts) <- c("type", "count")
      plotly::plot_ly(
        data = counts, labels = ~type, values = ~count,
        type = "pie",
        textinfo = "label+percent",
        hovertemplate = "%{label}: %{value} samples (%{percent})<extra></extra>"
      ) |>
        plotly::layout(title = list(text = "Sample Types", font = list(size = 14)))
    })
  })

  # Plate distribution bar chart
  output$qc_plate_bar <- plotly::renderPlotly({
    shiny::req(rv$qc_result)
    silent_only({
      df <- NULL
      if (!is.null(rv$qc_result$data$concentration$corrected)) {
        df <- corrected_data()
      }
      shiny::req(df, "sample_plate_id" %in% names(df))
      counts <- as.data.frame(table(df$sample_plate_id), stringsAsFactors = FALSE)
      names(counts) <- c("plate", "count")
      plotly::plot_ly(
        data = counts, x = ~plate, y = ~count,
        type = "bar",
        marker = list(color = "#377EB8"),
        hovertemplate = "Plate %{x}: %{y} samples<extra></extra>"
      ) |>
        plotly::layout(
          xaxis = list(title = "Plate"),
          yaxis = list(title = "Number of Samples"),
          title = list(text = "Samples per Plate", font = list(size = 14))
        )
    })
  })

  output$qc_filtered_table <- DT::renderDT({
    shiny::req(rv$qc_result)
    tryCatch({
      # Build a summary of what passed vs failed QC filtering
      passed_metabolites <- character(0)
      failed_metabolites <- character(0)
      failed_samples_list <- character(0)
      passed_samples <- character(0)

      # Failed samples from missing value filter
      if (!is.null(rv$qc_result$filters$failed_samples)) {
        failed_samples_list <- rv$qc_result$filters$failed_samples
      }

      # Failed lipids from lipid filter
      if (!is.null(rv$qc_result$filters$failed_lipids)) {
        failed_metabolites <- rv$qc_result$filters$failed_lipids
      }

      # Get all metabolite names from corrected data
      if (!is.null(rv$qc_result$data$concentration$corrected)) {
        df <- corrected_data()
        num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
        all_metabolites <- setdiff(num_cols, meta_cols)
        passed_metabolites <- setdiff(all_metabolites, failed_metabolites)

        all_samples <- unique(df$sample_name)
        passed_samples <- setdiff(all_samples, failed_samples_list)
      }

      # Build summary table
      max_len <- max(length(passed_metabolites), length(passed_samples),
                     length(failed_metabolites), length(failed_samples_list), 1)

      pad_vec <- function(v, n) {
        length(v) <- n
        v
      }

      summary_df <- data.frame(
        Passed_Metabolites = pad_vec(passed_metabolites, max_len),
        Failed_Metabolites = pad_vec(failed_metabolites, max_len),
        Passed_Samples = pad_vec(passed_samples, max_len),
        Failed_Samples = pad_vec(failed_samples_list, max_len),
        stringsAsFactors = FALSE
      )

      # Add counts as column names
      names(summary_df) <- c(
        paste0("Passed Metabolites (", length(passed_metabolites), ")"),
        paste0("Failed Metabolites (", length(failed_metabolites), ")"),
        paste0("Passed Samples (", length(passed_samples), ")"),
        paste0("Failed Samples (", length(failed_samples_list), ")")
      )

      summary_df
    }, error = function(e) data.frame(Info = e$message))
  }, options = list(pageLength = 25, scrollX = TRUE))

  # -- Batch Correction --------------------------------------------------------

  # Detect xlsx upload and populate sheet selector
  shiny::observeEvent(input$batch_data_upload, {
    shiny::req(input$batch_data_upload)
    # SHINY-H7: rate-limit uploads per session before any file handling.
    rl <- check_upload_rate(rv)
    if (!rl$allowed) {
      shiny::showNotification("Upload rate limit exceeded — try again shortly.",
                              type = "error", duration = 6)
      return(invisible(NULL))
    }
    # Show sheet selector only for single xlsx upload
    if (nrow(input$batch_data_upload) == 1) {
      ext <- tolower(tools::file_ext(input$batch_data_upload$name))
      if (ext == "xlsx") {
        tryCatch({
          sheets <- openxlsx::getSheetNames(input$batch_data_upload$datapath)
          rv$batch_xlsx_sheets <- sheets
        }, error = function(e) {
          rv$batch_xlsx_sheets <- NULL
          session$sendCustomMessage("mst-notify", list(
            message = paste0("Could not read xlsx: ", e$message),
            type = "danger", duration = 5000
          ))
        })
      } else {
        rv$batch_xlsx_sheets <- NULL
      }
    } else {
      rv$batch_xlsx_sheets <- NULL
    }
  })

  # Render sheet selector for xlsx files
  output$batch_xlsx_sheet_selector <- shiny::renderUI({
    sheets <- rv$batch_xlsx_sheets
    if (is.null(sheets) || length(sheets) == 0) return(NULL)
    htmltools::tagList(
      htmltools::tags$hr(),
      htmltools::tags$div(
        class = "alert alert-info py-2 px-3",
        style = "font-size: 0.82rem;",
        shiny::icon("file-excel"),
        paste0(" Excel file detected with ", length(sheets), " sheet(s)")
      ),
      shiny::selectInput(
        "batch_xlsx_sheet", "Select Sheet",
        choices = stats::setNames(sheets, paste0(seq_along(sheets), ". ", sheets)),
        selected = sheets[1]
      ),
      shiny::uiOutput("batch_xlsx_sheet_preview")
    )
  })

  # Preview the selected xlsx sheet (first few rows)
  output$batch_xlsx_sheet_preview <- shiny::renderUI({
    shiny::req(input$batch_data_upload, input$batch_xlsx_sheet)
    tryCatch({
      df <- openxlsx::read.xlsx(input$batch_data_upload$datapath,
                                sheet = input$batch_xlsx_sheet,
                                rows = 1:6)
      if (is.null(df) || ncol(df) == 0) {
        return(htmltools::tags$p(class = "help-text text-danger", "Sheet appears empty."))
      }
      htmltools::tags$div(
        htmltools::tags$p(
          class = "help-text",
          paste0(ncol(df), " columns, preview of first 5 rows:")
        ),
        htmltools::tags$div(
          style = "max-height: 120px; overflow: auto; font-size: 0.75rem;",
          htmltools::HTML(
            knitr::kable(utils::head(df, 5), format = "html", escape = TRUE,
                         table.attr = 'class="table table-sm table-bordered"')
          )
        )
      )
    }, error = function(e) {
      htmltools::tags$p(class = "help-text text-danger",
                         paste0("Cannot preview: ", e$message))
    })
  })

  # Helper: read a single uploaded file
  bc_read_upload <- function(datapath, name, sheet) {
    ext <- tolower(tools::file_ext(name))
    if (ext == "xlsx") {
      df <- openxlsx::read.xlsx(datapath, sheet = sheet %||% 1)
    } else {
      named_path <- preserve_upload_names(datapath, name)
      df <- read_tabular_file(named_path, original_name = name)
    }
    if (!is.null(df) && ncol(df) > 1) tibble::as_tibble(df) else NULL
  }

  # Load data from upload or pipeline.
  # Returns a list(df, desc); an observer below writes `desc` into rv so the
  # reactive remains pure (SHINY-H1).
  batch_loaded_data_raw <- shiny::reactive({
    if (input$batch_data_source == "upload") {
      shiny::req(input$batch_data_upload)
      upload <- input$batch_data_upload
      n_files <- nrow(upload)

      if (n_files == 1) {
        # Single file upload
        check <- validate_upload(upload,
                                 allowed_extensions = c("csv", "tsv", "txt", "xlsx"))
        if (!check$valid) return(NULL)
        df <- bc_read_upload(upload$datapath, upload$name,
                             input$batch_xlsx_sheet)
        shiny::req(df)
        ext <- tolower(tools::file_ext(upload$name))
        desc <- if (ext == "xlsx") {
          paste0("Uploaded file: ", upload$name,
                 " [sheet: ", input$batch_xlsx_sheet %||% 1, "]")
        } else {
          paste0("Uploaded file: ", upload$name)
        }
        return(list(df = df, desc = desc))
      }

      # Multiple files: read each, keep common columns, row-bind
      frames <- Filter(Negate(is.null), lapply(seq_len(n_files), function(i) {
        tryCatch(
          bc_read_upload(upload$datapath[i], upload$name[i], sheet = 1),
          error = function(e) NULL
        )
      }))
      shiny::req(length(frames) >= 2)

      # Find columns common to all uploaded files
      common_cols <- Reduce(intersect, lapply(frames, names))
      shiny::req(length(common_cols) > 1)

      # Keep only common columns and row-bind
      frames <- lapply(frames, function(df_i) df_i[, common_cols, drop = FALSE])
      df <- dplyr::bind_rows(frames)
      desc <- paste0(
        n_files, " files joined (", nrow(df), " rows, ",
        length(common_cols), " common columns): ",
        paste(upload$name, collapse = ", ")
      )
      return(list(df = df, desc = desc))
    } else {
      shiny::req(rv$qc_result)
      tryCatch({
        corrected <- corrected_data()
        shiny::req(nrow(corrected) > 0)
        proj_name <- rv$qc_result$project_details$project_name %||% "unknown"
        proj_dir  <- rv$qc_result$project_details$project_dir %||% "unknown"
        desc <- paste0(
          "qcCheckR output: ", proj_name,
          " (", basename(proj_dir), "/all/data/concentration/corrected)"
        )
        list(df = corrected, desc = desc)
      }, error = function(e) NULL)
    }
  })

  # Public reactive returns just the data frame (preserves prior behaviour
  # of `batch_loaded_data()` callers).
  # bindCache() keys the result on upload datapaths + source selector so that
  # downstream req() pulses do not trigger repeated file reads.
  batch_loaded_data <- shiny::reactive({
    bundle <- batch_loaded_data_raw()
    if (is.null(bundle)) return(NULL)
    bundle$df
  }) |> shiny::bindCache(
    input$batch_data_source,
    if (!is.null(input$batch_data_upload)) input$batch_data_upload$datapath else NULL,
    if (!is.null(input$batch_xlsx_sheet)) input$batch_xlsx_sheet else NULL
  )

  # Pure reactive for the source description — no rv write (SH-012).
  batch_source_desc_r <- shiny::reactive({
    bundle <- tryCatch(batch_loaded_data_raw(), error = function(e) NULL)
    if (!is.null(bundle)) bundle$desc else NULL
  })

  # Show which qcCheckR file will be corrected
  output$batch_pipeline_source_info <- shiny::renderUI({
    shiny::req(rv$qc_result)
    proj_name <- rv$qc_result$project_details$project_name %||% "unknown"
    proj_dir  <- rv$qc_result$project_details$project_dir %||% ""
    n_plates  <- length(rv$qc_result$data$concentration$corrected)
    n_samples <- tryCatch(nrow(corrected_data()), error = function(e) 0)
    htmltools::tags$div(
      class = "small mt-2 mb-2",
      style = "color: var(--mst-text-secondary);",
      htmltools::tags$div(
        shiny::icon("folder-open", class = "text-muted me-1"),
        htmltools::tags$strong("Project: "), proj_name
      ),
      htmltools::tags$div(
        shiny::icon("table", class = "text-muted me-1"),
        htmltools::tags$strong("Plates: "), n_plates,
        " | ",
        htmltools::tags$strong("Samples: "), n_samples
      )
    )
  })

  # Populate the "Batch Column" dropdown from the loaded data. Default-select
  # the conventional sample_plate_id (or batch) so existing workflows keep
  # working without user intervention; otherwise fall back to the first
  # discrete-looking column. When no data is loaded the dropdown is empty.
  shiny::observe({
    df <- tryCatch(batch_loaded_data(), error = function(e) NULL)
    if (is.null(df) || nrow(df) == 0 || ncol(df) == 0) {
      shiny::updateSelectInput(session, "batch_batch_column",
                               choices = character(0))
      return()
    }
    cols <- colnames(df)
    # A reasonable default: prefer sample_plate_id, then batch, then the first
    # non-numeric column with > 1 unique value (i.e. looks like a grouping).
    default_col <- if ("sample_plate_id" %in% cols) "sample_plate_id"
                   else if ("batch" %in% cols) "batch"
                   else {
                     candidates <- cols[vapply(df, function(x) {
                       !is.numeric(x) && length(unique(x)) > 1
                     }, logical(1))]
                     if (length(candidates) > 0) candidates[1] else cols[1]
                   }
    current <- shiny::isolate(input$batch_batch_column)
    selected <- if (!is.null(current) && nzchar(current) && current %in% cols)
                  current else default_col
    shiny::updateSelectInput(session, "batch_batch_column",
                             choices = cols, selected = selected)
  })

  # Populate the ComBat reference-batch dropdown from the unique values of
  # whichever column the user picked above. The "(none — use grand mean)"
  # option corresponds to combat_ref.batch = NULL on the R side (see args
  # construction inside the batchCorrectR run observer below).
  shiny::observe({
    df <- tryCatch(batch_loaded_data(), error = function(e) NULL)
    batch_col <- input$batch_batch_column
    batches <- character(0)
    if (!is.null(df) && nrow(df) > 0 &&
        !is.null(batch_col) && nzchar(batch_col) &&
        batch_col %in% colnames(df)) {
      batches <- sort(unique(stats::na.omit(as.character(df[[batch_col]]))))
      batches <- batches[nzchar(batches)]
    }
    choices <- c("(none — use grand mean)" = "")
    if (length(batches) > 0) {
      choices <- c(choices, stats::setNames(batches, batches))
    }
    current <- shiny::isolate(input$combat_ref_batch)
    selected <- if (!is.null(current) && current %in% choices) current else ""
    shiny::updateSelectInput(session, "combat_ref_batch",
                             choices = choices, selected = selected)
  })

  # Console output for batch correction
  output$batch_console <- shiny::renderText({
    log <- rv$batch_log
    if (is.null(log) || !nzchar(log)) {
      "Console output will appear here when you run batch correction.\nSelect a data source and click 'Run Batch Correction' to start."
    } else {
      # Prepend data source info
      src <- batch_source_desc_r()
      if (!is.null(src) && nzchar(src)) {
        paste0("Data source: ", src, "\n", paste(rep("-", 60), collapse = ""), "\n", log)
      } else {
        log
      }
    }
  })

  # SH-013: batchCorrectR runs in a callr::r_bg() subprocess. Messages are
  # streamed via sink() in the subprocess; the global process_poller below
  # collects the final result and writes the corrected-data CSV. See the
  # sibling qc_run observer + mst_spawn_pkg_fn() helper for the pattern.
  shiny::observeEvent(input$batch_run, {
    # -- Run protection guard --
    if (rv$running) {
      session$sendCustomMessage("mst-notify", list(
        message = paste0("Processing already in progress (", rv$running_task,
          "). Please wait for it to finish or cancel and restart."),
        type = "warning", duration = 5000))
      return()
    }

    # -- Comprehensive parameter validation --
    df <- tryCatch(batch_loaded_data(), error = function(e) NULL)
    issues <- validate_batch_params(
      data        = df,
      qc_label    = input$batch_qc_label,
      method      = input$batch_method,
      project_dir = input$batch_project_dir
    )
    if (show_validation_issues(session, issues)) return()

    # SHINY-H5: sanitise numeric inputs (NA, negative, out-of-range).
    ntree_val <- safe_numeric(input$batch_ntree, default = 500,
                              min = 1, max = 10000, integer = TRUE)
    if (!identical(ntree_val, as.integer(input$batch_ntree %||% 500))) {
      notify_input_clamp_once(session, rv, "batch_ntree",
        paste0("Batch ntree adjusted to ", ntree_val,
               " (allowed range 1-10000)."))
    }
    frule_pct <- safe_numeric(input$batch_Frule, default = 80,
                              min = 0, max = 100)
    if (!identical(frule_pct, as.numeric(input$batch_Frule %||% 80))) {
      notify_input_clamp_once(session, rv, "batch_Frule",
        paste0("Batch Frule adjusted to ", frule_pct,
               "% (allowed range 0-100)."))
    }

    # Parse comma-separated sample_tags; blank entry disables filtering.
    # The QC label is auto-added (case-insensitive) so users cannot
    # accidentally drop all their QC rows by omitting it.
    sample_tags_vec <- NULL
    raw_tags <- input$batch_sample_tags %||% ""
    if (nzchar(raw_tags)) {
      sample_tags_vec <- trimws(strsplit(raw_tags, ",")[[1]])
      sample_tags_vec <- sample_tags_vec[nzchar(sample_tags_vec)]
      qc_label_trim <- trimws(input$batch_qc_label %||% "")
      if (nzchar(qc_label_trim) &&
          !tolower(qc_label_trim) %in% tolower(sample_tags_vec)) {
        sample_tags_vec <- c(sample_tags_vec, qc_label_trim)
      }
      if (length(sample_tags_vec) == 0) sample_tags_vec <- NULL
    }

    args <- list(
      data        = df,
      qc_label    = input$batch_qc_label,
      method      = input$batch_method,
      ntree       = ntree_val,
      coCV        = input$batch_coCV,
      Frule       = frule_pct / 100,
      imputeM     = "minHalf",
      sample_tags = sample_tags_vec,
      batch_column = if (nzchar(input$batch_batch_column %||% "")) {
        input$batch_batch_column
      } else NULL,
      project_dir = if (nzchar(input$batch_project_dir %||% "")) {
        input$batch_project_dir
      } else NULL,
      plot        = TRUE,
      report      = TRUE,
      combat_par.prior = input$combat_par_prior %||% TRUE,
      combat_mean.only = input$combat_mean_only %||% FALSE,
      combat_ref.batch = if (nzchar(input$combat_ref_batch %||% "")) {
        input$combat_ref_batch
      } else NULL,
      qcrlsc_method = input$batch_qcrlsc_method %||% "subtract",
      qcrlsc_intra  = input$batch_qcrlsc_intra  %||% FALSE,
      qcrlsc_opti   = input$batch_qcrlsc_opti   %||% TRUE,
      qcrlsc_log10  = input$batch_qcrlsc_log10  %||% TRUE,
      qcrlsc_outl   = input$batch_qcrlsc_outl   %||% TRUE,
      qcrlsc_shift  = input$batch_qcrlsc_shift  %||% TRUE
    )

    bg <- tryCatch(
      mst_spawn_pkg_fn("batchCorrectR", args),
      error = function(e) {
        session$sendCustomMessage("mst-notify", list(
          message = paste0("Could not start background worker: ",
                           conditionMessage(e)),
          type = "danger", duration = 6000
        ))
        NULL
      }
    )
    if (is.null(bg)) return()

    log_audit("Batch Correction", status = "START")
    rv$batch_log         <- "Starting batchCorrectR in background worker...\n"
    rv$running           <- TRUE
    rv$running_task      <- "Batch Correction"
    rv$process_handle    <- bg$handle
    rv$process_log_file  <- bg$log_file
    rv$process_log_offset <- 0L
    rv$process_task      <- "batch_run"
    rv$process_extra     <- list(
      df = df,
      data_source = input$batch_data_source
    )
    disable_run_buttons(session)
  })

  # Helper: resolve column names for run order and sample type
  # qcCheckR uses sample_run_index/sample_type_factor; batchCorrectR uses run_order/sample_type
  bc_resolve_cols <- function(df) {
    run_col <- if ("run_order" %in% names(df)) "run_order"
               else if ("sample_run_index" %in% names(df)) "sample_run_index"
               else if ("injection_order" %in% names(df)) "injection_order"
               else NULL
    type_col <- if ("sample_type_factor" %in% names(df)) "sample_type_factor"
                else if ("sample_type" %in% names(df)) "sample_type"
                else NULL
    list(run = run_col, type = type_col)
  }

  # Metabolite selector for signal drift plots
  output$batch_met_selector <- shiny::renderUI({
    shiny::req(rv$bc_result)
    met_cols <- rv$bc_result$correction_summary$metabolite
    shiny::req(length(met_cols) > 0)

    # Default to metabolite with highest RSD before correction
    rsd_vals <- rv$bc_result$qc_rsd_before[met_cols]
    rsd_vals <- rsd_vals[!is.na(rsd_vals)]
    default_met <- if (length(rsd_vals) > 0) names(rsd_vals)[which.max(rsd_vals)] else met_cols[1]

    shiny::selectInput(
      "batch_selected_metabolite", "Select Metabolite",
      choices = met_cols, selected = default_met
    )
  })

  # Helper: build a signal drift plot coloured by sample type.
  # Delegates to MStargetR:::bc_drift_plot so the GUI and R users sharing
  # advanced_plots = TRUE see the byte-identical figure.
  bc_drift_plot <- function(df, met, title_prefix) {
    cols <- bc_resolve_cols(df)
    shiny::req(cols$run, cols$type, met %in% names(df))
    qc_label <- input$batch_qc_label %||% "qc"
    plot_pair <- MStargetR:::bc_drift_plot(df, met, title_prefix,
                                           qc_label = qc_label)
    if (is.null(plot_pair)) {
      return(plotly::plot_ly() |>
               plotly::layout(annotations = list(list(
                 text = "Drift plot unavailable for this metabolite",
                 showarrow = FALSE, xref = "paper", yref = "paper",
                 x = 0.5, y = 0.5))))
    }
    plot_pair$interactive
  }

  # Signal drift: Before correction
  output$batch_plot_before <- plotly::renderPlotly({
    shiny::req(rv$bc_result, rv$bc_data, input$batch_selected_metabolite)
    tryCatch({
      bc_drift_plot(rv$bc_data, input$batch_selected_metabolite, "Before")
    }, error = function(e) {
      plotly::plot_ly() |>
        plotly::layout(annotations = list(list(
          text = paste("Plot error:", e$message), showarrow = FALSE,
          xref = "paper", yref = "paper", x = 0.5, y = 0.5
        )))
    })
  })

  # Signal drift: After correction
  output$batch_plot_after <- plotly::renderPlotly({
    shiny::req(rv$bc_result, input$batch_selected_metabolite)
    tryCatch({
      bc_drift_plot(rv$bc_result$corrected_data, input$batch_selected_metabolite, "After")
    }, error = function(e) {
      plotly::plot_ly() |>
        plotly::layout(annotations = list(list(
          text = paste("Plot error:", e$message), showarrow = FALSE,
          xref = "paper", yref = "paper", x = 0.5, y = 0.5
        )))
    })
  })

  # Dynamic checkbox for sample type inclusion in PCA

  output$batch_pca_sample_toggle <- shiny::renderUI({
    shiny::req(rv$bc_data)
    cols <- bc_resolve_cols(rv$bc_data)
    shiny::req(cols$type)
    types <- sort(unique(as.character(rv$bc_data[[cols$type]])))
    shiny::req(length(types) > 0)
    shiny::checkboxGroupInput(
      "batch_pca_include_types",
      label = "Include sample types in PCA:",
      choices = types,
      selected = types,
      inline = TRUE
    )
  })

  # Cached PCA: recompute only when corrected data or selected types change
  bc_pca_cache <- shiny::reactive({
    shiny::req(rv$bc_result)
    build_pca <- function(df_in, met_cols) {
      incl <- input$batch_pca_include_types
      cols <- bc_resolve_cols(df_in)
      if (!is.null(cols$type) && !is.null(incl))
        df_in <- df_in[as.character(df_in[[cols$type]]) %in% incl, , drop = FALSE]
      if (nrow(df_in) < 3 || length(met_cols) < 2) return(NULL)
      mat <- as.matrix(df_in[, met_cols, drop = FALSE])
      for (j in seq_len(ncol(mat))) {
        nas <- is.na(mat[, j])
        if (any(nas)) mat[nas, j] <- stats::median(mat[, j], na.rm = TRUE)
      }
      mat <- mat[, apply(mat, 2, stats::var, na.rm = TRUE) > 0, drop = FALSE]
      if (ncol(mat) < 2) return(NULL)
      list(pca = stats::prcomp(mat, center = TRUE, scale. = TRUE), df = df_in)
    }
    mets <- rv$bc_result$correction_summary$metabolite
    list(
      before = tryCatch(
        build_pca(rv$bc_data, intersect(mets, names(rv$bc_data))),
        error = function(e) NULL),
      after  = tryCatch(
        build_pca(rv$bc_result$corrected_data,
                  intersect(mets, names(rv$bc_result$corrected_data))),
        error = function(e) NULL)
    )
  })

  # Helper: PCA score plot coloured by sample type (uses cached prcomp when available)
  bc_pca_plot <- function(df, title_prefix, cached_entry = NULL) {
    cols <- bc_resolve_cols(df)
    shiny::req(cols$type)

    # Filter to selected sample types
    if (!is.null(input$batch_pca_include_types)) {
      df <- df[as.character(df[[cols$type]]) %in% input$batch_pca_include_types, , drop = FALSE]
    }
    shiny::req(nrow(df) >= 3)

    met_cols <- rv$bc_result$correction_summary$metabolite
    met_cols <- intersect(met_cols, names(df))
    shiny::req(length(met_cols) >= 2)

    qc_label <- tolower(input$batch_qc_label %||% "qc")

    if (!is.null(cached_entry) && !is.null(cached_entry$pca)) {
      pca <- cached_entry$pca
      df  <- cached_entry$df
    } else {
      # Extract numeric matrix, impute NAs with column median for PCA
      mat <- as.matrix(df[, met_cols, drop = FALSE])
      for (j in seq_len(ncol(mat))) {
        nas <- is.na(mat[, j])
        if (any(nas)) mat[nas, j] <- stats::median(mat[, j], na.rm = TRUE)
      }
      # Remove zero-variance columns
      col_vars <- apply(mat, 2, stats::var, na.rm = TRUE)
      mat <- mat[, col_vars > 0, drop = FALSE]
      shiny::req(ncol(mat) >= 2)
      pca <- stats::prcomp(mat, center = TRUE, scale. = TRUE)
    }
    var_expl <- round(100 * pca$sdev^2 / sum(pca$sdev^2), 1)

    pca_df <- data.frame(
      PC1 = pca$x[, 1], PC2 = pca$x[, 2],
      sample_type_label = as.character(df[[cols$type]]),
      sample = if ("sample_name" %in% names(df)) df$sample_name else seq_len(nrow(df)),
      stringsAsFactors = FALSE
    )

    # Build colour palette: QC in red, others get distinct colours
    type_levels <- sort(unique(pca_df$sample_type_label))
    type_palette <- rep("#377EB8", length(type_levels))
    names(type_palette) <- type_levels

    non_qc <- type_levels[tolower(type_levels) != qc_label]
    if (length(non_qc) > 0) {
      non_qc_cols <- if (length(non_qc) <= 8) {
        grDevices::hcl.colors(max(length(non_qc), 3), palette = "Dark 3")[seq_len(length(non_qc))]
      } else {
        grDevices::hcl.colors(length(non_qc), palette = "Set 2")
      }
      type_palette[non_qc] <- non_qc_cols
    }
    qc_types <- type_levels[tolower(type_levels) == qc_label]
    if (length(qc_types) > 0) type_palette[qc_types] <- "#E41A1C"

    pca_df$sample_type_label <- factor(pca_df$sample_type_label, levels = type_levels)

    p <- ggplot2::ggplot(
      pca_df,
      ggplot2::aes(x = PC1, y = PC2, colour = sample_type_label, text = sample)
    ) +
      ggplot2::geom_point(size = 2, alpha = 0.7) +
      ggplot2::scale_colour_manual(values = type_palette, name = "Sample Type") +
      ggplot2::labs(
        title = title_prefix,
        x = paste0("PC1 (", var_expl[1], "%)"),
        y = paste0("PC2 (", var_expl[2], "%)")
      ) +
      ggplot2::theme_bw()

    plotly::ggplotly(p, tooltip = c("text", "x", "y"))
  }

  # PCA: Before correction
  output$batch_pca_before <- plotly::renderPlotly({
    shiny::req(rv$bc_result, rv$bc_data)
    tryCatch({
      bc_pca_plot(rv$bc_data, "Before Correction",
                  cached_entry = tryCatch(bc_pca_cache()$before, error = function(e) NULL))
    }, error = function(e) {
      plotly::plot_ly() |>
        plotly::layout(annotations = list(list(
          text = paste("PCA error:", e$message), showarrow = FALSE,
          xref = "paper", yref = "paper", x = 0.5, y = 0.5
        )))
    })
  })

  # PCA: After correction
  output$batch_pca_after <- plotly::renderPlotly({
    shiny::req(rv$bc_result)
    tryCatch({
      bc_pca_plot(rv$bc_result$corrected_data, "After Correction",
                  cached_entry = tryCatch(bc_pca_cache()$after, error = function(e) NULL))
    }, error = function(e) {
      plotly::plot_ly() |>
        plotly::layout(annotations = list(list(
          text = paste("PCA error:", e$message), showarrow = FALSE,
          xref = "paper", yref = "paper", x = 0.5, y = 0.5
        )))
    })
  })

  # Helper: extract metabolite class from name by tokenizing on first delimiter
  # e.g. "PC 36:2" -> "PC", "LPC_18:1" -> "LPC", "CE(18:2)" -> "CE",
  #       "TG 52:3" -> "TG", "Sphingomyelin d18:1/16:0" -> "Sphingomyelin"
  bc_extract_class <- function(met_names) {
    # Split on space, underscore, opening paren, or transition from letter to digit
    tokens <- sub("[\\s_\\(].*$", "", met_names, perl = TRUE)
    # Also handle letter-to-digit boundary: "CE18:2" -> "CE"
    tokens <- sub("\\d.*$", "", tokens, perl = TRUE)
    # Clean up empty tokens
    tokens[!nzchar(tokens)] <- "Unknown"
    tokens
  }

  # Class overview: median RSD before vs after by metabolite class
  output$batch_rsd_class_plot <- plotly::renderPlotly({
    shiny::req(rv$bc_result)
    tryCatch({
      summ <- rv$bc_result$correction_summary
      shiny::req("metabolite" %in% names(summ), "rsd_before" %in% names(summ),
                 "rsd_after" %in% names(summ))

      summ$class <- bc_extract_class(summ$metabolite)

      class_summ_b <- stats::aggregate(rsd_before ~ class, data = summ,
        FUN = function(x) stats::median(x, na.rm = TRUE))
      class_summ_a <- stats::aggregate(rsd_after ~ class, data = summ,
        FUN = function(x) stats::median(x, na.rm = TRUE))
      class_summ <- merge(class_summ_b, class_summ_a, by = "class")
      class_summ$n <- as.integer(table(summ$class)[class_summ$class])
      class_summ <- class_summ[order(class_summ$rsd_before, decreasing = TRUE), ]
      class_summ$class <- factor(class_summ$class, levels = rev(class_summ$class))

      p <- ggplot2::ggplot(class_summ) +
        ggplot2::geom_segment(
          ggplot2::aes(x = rsd_before, xend = rsd_after,
                       y = class, yend = class),
          colour = "grey60", linewidth = 0.6
        ) +
        ggplot2::geom_point(
          ggplot2::aes(x = rsd_before, y = class,
                       text = paste0(class, " (n=", n, ")\nBefore: ", round(rsd_before, 1), "%")),
          colour = "#E41A1C", size = 3
        ) +
        ggplot2::geom_point(
          ggplot2::aes(x = rsd_after, y = class,
                       text = paste0(class, " (n=", n, ")\nAfter: ", round(rsd_after, 1), "%")),
          colour = "#377EB8", size = 3
        ) +
        ggplot2::labs(
          title = "Median QC %RSD by Class: Before (red) vs After (blue)",
          x = "Median QC %RSD", y = NULL
        ) +
        ggplot2::theme_bw() +
        ggplot2::theme(axis.text.y = ggplot2::element_text(size = 9))

      plotly::ggplotly(p, tooltip = "text")
    }, error = function(e) {
      plotly::plot_ly() |>
        plotly::layout(annotations = list(list(
          text = paste("Class plot error:", e$message), showarrow = FALSE,
          xref = "paper", yref = "paper", x = 0.5, y = 0.5
        )))
    })
  })

  # Metabolite selector for RSD comparison
  output$batch_rsd_met_selector <- shiny::renderUI({
    shiny::req(rv$bc_result)
    met_cols <- rv$bc_result$correction_summary$metabolite
    shiny::req(length(met_cols) > 0)

    rsd_vals <- rv$bc_result$qc_rsd_before[met_cols]
    rsd_vals <- rsd_vals[!is.na(rsd_vals)]
    default_met <- if (length(rsd_vals) > 0) names(rsd_vals)[which.max(rsd_vals)] else met_cols[1]

    shiny::selectInput(
      "batch_rsd_metabolite", "Select Metabolite",
      choices = met_cols, selected = default_met
    )
  })

  # QC %RSD bar chart for selected metabolite
  output$batch_rsd_plot <- plotly::renderPlotly({
    shiny::req(rv$bc_result, input$batch_rsd_metabolite)
    tryCatch({
      summ <- rv$bc_result$correction_summary
      shiny::req("metabolite" %in% names(summ), "rsd_before" %in% names(summ),
                 "rsd_after" %in% names(summ))

      row <- summ[summ$metabolite == input$batch_rsd_metabolite, ]
      shiny::req(nrow(row) == 1)

      bar_df <- data.frame(
        Stage = factor(c("Before", "After"), levels = c("Before", "After")),
        RSD = c(row$rsd_before, row$rsd_after)
      )

      p <- ggplot2::ggplot(bar_df, ggplot2::aes(x = Stage, y = RSD, fill = Stage)) +
        ggplot2::geom_col(width = 0.5) +
        ggplot2::scale_fill_manual(
          values = c("Before" = "#E41A1C", "After" = "#377EB8"),
          guide = "none"
        ) +
        ggplot2::geom_text(
          ggplot2::aes(label = paste0(round(RSD, 1), "%")),
          vjust = -0.5, size = 4
        ) +
        ggplot2::labs(
          title = paste0("QC %RSD: ", input$batch_rsd_metabolite),
          x = NULL, y = "QC %RSD"
        ) +
        ggplot2::theme_bw()

      plotly::ggplotly(p) |> plotly::config(displayModeBar = FALSE)
    }, error = function(e) {
      plotly::plot_ly() |>
        plotly::layout(annotations = list(list(
          text = paste("RSD plot error:", e$message), showarrow = FALSE,
          xref = "paper", yref = "paper", x = 0.5, y = 0.5
        )))
    })
  })

  # RSD summary table for selected metabolite
  output$batch_rsd_table <- DT::renderDT({
    shiny::req(rv$bc_result, input$batch_rsd_metabolite)
    summ <- rv$bc_result$correction_summary
    row <- summ[summ$metabolite == input$batch_rsd_metabolite, ]
    dt <- DT::datatable(
      row,
      options = list(dom = "t", scrollX = TRUE)
    )
    round_cols <- intersect(c("rsd_before", "rsd_after", "rsd_change"),
                             colnames(rv$bc_result$correction_summary))
    if (length(round_cols) > 0) dt <- DT::formatRound(dt, columns = round_cols, digits = 2)
    dt
  })

  # Corrected data table
  output$batch_corrected_table <- DT::renderDT({
    shiny::req(rv$bc_result)
    DT::datatable(
      utils::head(rv$bc_result$corrected_data, 100),
      options = list(pageLength = 15, scrollX = TRUE),
      caption = paste(
        "Showing first 100 of",
        nrow(rv$bc_result$corrected_data), "rows"
      )
    )
  })

  # Download corrected data
  output$batch_download <- shiny::downloadHandler(
    filename = function() {
      paste0("batchCorrectR_corrected_", Sys.Date(), ".csv")
    },
    content = function(file) {
      shiny::req(rv$bc_result)
      tryCatch(
        readr::write_csv(rv$bc_result$corrected_data, file),
        error = function(e) {
          message("batch_download write failed: ", e$message)
          readr::write_csv(
            data.frame(Error = e$message, stringsAsFactors = FALSE),
            file
          )
        }
      )
    }
  )

  # -- Utilities ---------------------------------------------------------------

  # Transition checker
  shiny::observeEvent(input$util_transition_run, {
    # -- Run protection guard --
    if (rv$running) {
      session$sendCustomMessage("mst-notify", list(
        message = paste0("Processing already in progress (", rv$running_task,
          "). Please wait for it to finish or cancel and restart."),
        type = "warning", duration = 5000))
      return()
    }
    rv$running      <- TRUE
    rv$running_task <- "Transition Check"
    disable_run_buttons(session)
    on.exit({
      rv$running      <- FALSE
      rv$running_task <- ""
      enable_run_buttons(session)
    }, add = TRUE)

    shiny::req(input$util_transition_file)

    check <- validate_upload(input$util_transition_file,
                             allowed_extensions = c("tsv", "csv", "txt"))
    if (!check$valid) {
      session$sendCustomMessage("mst-notify", list(
        message = check$message, type = "warning", duration = 5000
      ))
      return()
    }

    named_path <- preserve_upload_names(input$util_transition_file$datapath,
                                        input$util_transition_file$name)
    df <- read_tabular_file(named_path,
                            original_name = input$util_transition_file$name)
    shiny::req(df)

    session$sendCustomMessage("mst-notify", list(
      message = "Running transition check...", type = "info", duration = 2000
    ))

    # Capture messages (transition_checkR uses message() for success feedback)
    # Use an environment accumulator to avoid <<- walking past observer frame
    msg_env <- new.env(parent = emptyenv())
    msg_env$msgs <- character(0)
    result <- safe_call({
      withCallingHandlers(
        MStargetR::transition_checkR(df),
        message = function(m) {
          msg_env$msgs <- c(msg_env$msgs, conditionMessage(m))
          invokeRestart("muffleMessage")
        }
      )
    }, error_prefix = "transition_checkR")
    msgs <- msg_env$msgs

    if (result$success) {
      res <- result$result
      if (is.data.frame(res) && nrow(res) > 0) {
        # Duplicates found -- show warning badge and populate table
        transition_result_val(
          create_status_badge("warning",
                              paste(nrow(res), "duplicate transitions found"))
        )
        transition_table_val(res)
        session$sendCustomMessage("mst-notify", list(
          message = paste(nrow(res), "duplicate transitions detected -- see table below."),
          type = "warning", duration = 6000
        ))
      } else {
        # Success -- res is NULL, feedback is in captured messages
        success_msg <- if (length(msgs) > 0) {
          paste(trimws(msgs), collapse = " ")
        } else {
          "All MRM transitions are unique."
        }
        transition_result_val(create_status_badge("success", success_msg))
        transition_table_val(NULL)
        session$sendCustomMessage("mst-notify", list(
          message = success_msg, type = "success", duration = 4000
        ))
      }
    } else {
      transition_result_val(create_status_badge("danger", result$message))
      transition_table_val(NULL)
      session$sendCustomMessage("mst-notify", list(
        message = result$message, type = "danger", duration = 6000
      ))
    }
  })

  output$util_transition_result <- shiny::renderUI({
    transition_result_val()
  })

  output$util_transition_table <- DT::renderDT({
    shiny::req(transition_table_val())
    DT::datatable(
      transition_table_val(),
      options  = list(pageLength = 15, scrollX = TRUE),
      rownames = FALSE,
      caption  = "Duplicate Q1/Q3 transitions that need correction"
    )
  })

  # Template comparator
  shiny::observeEvent(input$util_compare_run, {
    # -- Run protection guard --
    if (rv$running) {
      session$sendCustomMessage("mst-notify", list(
        message = paste0("Processing already in progress (", rv$running_task,
          "). Please wait for it to finish or cancel and restart."),
        type = "warning", duration = 5000))
      return()
    }
    rv$running      <- TRUE
    rv$running_task <- "Template Comparison"
    disable_run_buttons(session)
    on.exit({
      rv$running      <- FALSE
      rv$running_task <- ""
      enable_run_buttons(session)
    }, add = TRUE)

    shiny::req(input$util_compare_mrm, input$util_compare_conc)

    mrm_check <- validate_upload(input$util_compare_mrm,
                                 allowed_extensions = c("tsv", "csv", "txt"))
    if (!mrm_check$valid) {
      session$sendCustomMessage("mst-notify", list(
        message = paste("MRM Template:", mrm_check$message),
        type = "warning", duration = 5000
      ))
      return()
    }

    conc_check <- validate_upload(input$util_compare_conc,
                                  allowed_extensions = c("tsv", "csv", "txt"))
    if (!conc_check$valid) {
      session$sendCustomMessage("mst-notify", list(
        message = paste("Concentration Guide:", conc_check$message),
        type = "warning", duration = 5000
      ))
      return()
    }

    mrm_path  <- preserve_upload_names(input$util_compare_mrm$datapath,
                                       input$util_compare_mrm$name)
    conc_path <- preserve_upload_names(input$util_compare_conc$datapath,
                                        input$util_compare_conc$name)
    mrm  <- read_tabular_file(mrm_path,
                              original_name = input$util_compare_mrm$name)
    conc <- read_tabular_file(conc_path,
                              original_name = input$util_compare_conc$name)
    shiny::req(mrm, conc)

    session$sendCustomMessage("mst-notify", list(
      message = "Comparing templates...", type = "info", duration = 2000
    ))

    # Capture messages (compare_mrm_template_with_guide uses message() for
    # success feedback and returns NULL on success)
    msgs <- character(0)
    result <- safe_call({
      withCallingHandlers(
        MStargetR::compare_mrm_template_with_guide(mrm, conc),
        message = function(m) {
          msgs[[length(msgs) + 1L]] <<- conditionMessage(m)
          invokeRestart("muffleMessage")
        }
      )
    }, error_prefix = "compare_mrm_template_with_guide")

    if (result$success) {
      res <- result$result
      if (is.character(res) && length(res) > 0) {
        # Unmatched internal standards found
        compare_result_val(
          create_status_badge("warning",
                              paste(length(res),
                                    "unmatched internal standard(s) found"))
        )
        compare_table_val(
          data.frame(Unmatched_SIL = res, stringsAsFactors = FALSE)
        )
        session$sendCustomMessage("mst-notify", list(
          message = paste(length(res),
                          "internal standards have no match in the concentration guide."),
          type = "warning", duration = 6000
        ))
      } else {
        # Success -- res is NULL, feedback is in captured messages
        success_msg <- if (length(msgs) > 0) {
          paste(trimws(msgs), collapse = " ")
        } else {
          "Templates are compatible."
        }
        compare_result_val(create_status_badge("success", success_msg))
        compare_table_val(NULL)
        session$sendCustomMessage("mst-notify", list(
          message = success_msg, type = "success", duration = 4000
        ))
      }
    } else {
      compare_result_val(create_status_badge("danger", result$message))
      compare_table_val(NULL)
      session$sendCustomMessage("mst-notify", list(
        message = result$message, type = "danger", duration = 6000
      ))
    }
  })

  output$util_compare_result <- shiny::renderUI({
    compare_result_val()
  })

  output$util_compare_table <- DT::renderDT({
    shiny::req(compare_table_val())
    DT::datatable(
      compare_table_val(),
      options  = list(pageLength = 15, scrollX = TRUE),
      rownames = FALSE,
      caption  = "Internal standards with no match in the concentration guide"
    )
  })

  # Dependency checker
  shiny::observeEvent(input$util_depcheck_run, {
    all_deps <- c(
      "statTarget", "mzR", "ropls", "dplyr", "tidyr", "tibble",
      "readr", "ggplot2", "plotly", "stringr", "purrr", "data.table",
      "openxlsx", "viridis", "future", "future.apply", "janitor",
      "BiocManager", "shiny", "bslib", "DT", "shinyWidgets", "htmltools"
    )
    installed <- vapply(all_deps, is_pkg_installed, logical(1))
    versions <- vapply(seq_along(all_deps), function(i) {
      if (installed[[i]]) {
        as.character(utils::packageVersion(all_deps[[i]]))
      } else {
        "---"
      }
    }, character(1))

    depcheck_result_val(data.frame(
      Package   = all_deps,
      Installed = ifelse(installed, "Yes", "No"),
      Version   = versions,
      stringsAsFactors = FALSE
    ))
  })

  output$util_depcheck_table <- DT::renderDT({
    shiny::req(depcheck_result_val())
    depcheck_result_val()
  }, options = list(pageLength = 25, dom = "t"))

  # -- Results Explorer --------------------------------------------------------

  # Reactive: get the active results data frame based on user selection
  # -- File upload for Results Explorer ------------------------------------------

  # Reactive: store uploaded + loaded data
  results_upload_data <- shiny::reactiveVal(NULL)

  # Observer: when an Excel file is uploaded, populate sheet selector

  shiny::observe({
    file_info <- input$results_upload_file
    shiny::req(file_info)
    ext <- tolower(tools::file_ext(file_info$name))
    if (ext %in% c("xlsx", "xls")) {
      shiny::req(file.size(file_info$datapath) > 0)
      sheets <- tryCatch(
        openxlsx::getSheetNames(file_info$datapath),
        error = function(e) {
          shiny::showNotification(
            paste("Could not read sheets from uploaded file:", e$message),
            type = "error", duration = 8)
          character(0)
        }
      )
      if (length(sheets) > 0) {
        shiny::updateSelectInput(session, "results_upload_sheet",
                                  choices = sheets, selected = sheets[1])
      } else {
        shiny::updateSelectInput(session, "results_upload_sheet",
                                  choices = c("No sheets found" = ""),
                                  selected = "")
      }
    } else {
      shiny::updateSelectInput(session, "results_upload_sheet",
                                choices = c("N/A (CSV file)" = ""),
                                selected = "")
    }
  })

  # Observer: load button reads the file into results_upload_data
  shiny::observeEvent(input$results_upload_load, {
    file_info <- input$results_upload_file
    shiny::req(file_info)
    # SHINY-H7: rate-limit uploads per session before reading the file.
    rl <- check_upload_rate(rv)
    if (!rl$allowed) {
      shiny::showNotification("Upload rate limit exceeded — try again shortly.",
                              type = "error", duration = 6)
      return(invisible(NULL))
    }
    # SH-014: validate extension and size before parsing to prevent zip-bomb / DoS.
    upload_check <- validate_upload(file_info,
                                    allowed_extensions = c("xlsx", "xls", "csv", "tsv"),
                                    max_size_mb = 100)
    if (!upload_check$valid) {
      shiny::showNotification(upload_check$message, type = "error", duration = 8)
      return(invisible(NULL))
    }
    ext <- tolower(tools::file_ext(file_info$name))
    df <- tryCatch({
      if (ext %in% c("xlsx", "xls")) {
        sheet <- input$results_upload_sheet
        if (is.null(sheet) || !nzchar(sheet)) sheet <- 1
        shiny::req(sheet %in% openxlsx::getSheetNames(file_info$datapath))
        openxlsx::read.xlsx(file_info$datapath, sheet = sheet)
      } else if (ext == "csv") {
        readr::read_csv(file_info$datapath, show_col_types = FALSE)
      } else if (ext == "tsv") {
        readr::read_tsv(file_info$datapath, show_col_types = FALSE)
      } else {
        NULL
      }
    }, error = function(e) {
      shiny::showNotification(paste("Error reading file:", e$message),
                               type = "error", duration = 8)
      NULL
    })
    if (!is.null(df) && nrow(df) > 0) {
      results_upload_data(df)
      shiny::showNotification(
        paste0("Loaded ", nrow(df), " rows \u00d7 ", ncol(df), " columns from ",
               file_info$name),
        type = "message", duration = 5)
    }
  })

  # Upload status indicator
  output$results_upload_status <- shiny::renderUI({
    df <- results_upload_data()
    if (is.null(df)) {
      htmltools::tags$small(class = "text-muted",
        "Upload a qcCheckR report (.xlsx) and select a data sheet, or a batch-corrected CSV."
      )
    } else {
      num_cols <- sum(vapply(df, is.numeric, logical(1)))
      htmltools::tags$small(class = "text-success",
        shiny::icon("check-circle", class = "me-1"),
        paste0("Loaded: ", nrow(df), " rows, ", num_cols, " numeric columns")
      )
    }
  })

  # -- Results data reactive (with upload support) --------------------------------

  results_data <- shiny::reactive({
    shiny::req(input$results_source)
    src <- input$results_source
    df  <- NULL
    if (src == "upload") {
      df <- results_upload_data()
    } else if (src == "batch") {
      if (!is.null(rv$bc_result)) {
        df <- rv$bc_result$corrected_data
      } else {
        shiny::showNotification(
          "Batch correction results not available; showing uncorrected QC data instead.",
          type = "warning", duration = 8)
        df <- tryCatch(corrected_data(), error = function(e) NULL)
      }
    } else if (!is.null(rv$qc_result)) {
      df <- tryCatch(corrected_data(), error = function(e) NULL)
    }
    df
  })

  # Reactive: detect metabolite columns (numeric, non-metadata)
  results_metabolite_cols <- shiny::reactive({
    df <- results_data()
    shiny::req(df)
    num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
    num_cols <- setdiff(num_cols, meta_cols)
    num_cols[!grepl("^sample_", num_cols)]
  })

  # Reactive: lipid class lookup (metabolite name -> class name)
  results_lipid_class_map <- shiny::reactive({
    map_vec <- character(0)

    # Source 1: MRM guides from qcCheckR/PeakForgeR pipeline
    guides <- tryCatch(rv$qc_result$templates$mrm_guides, error = function(e) NULL)
    if (!is.null(guides)) {
      versions <- setdiff(names(guides), "by_plate")
      for (v in versions) {
        g <- guides[[v]]
        mrm <- g$SIL_guide
        if (is.null(mrm)) mrm <- g$mrm_guide
        if (!is.null(mrm) && "Molecule List Name" %in% names(mrm) &&
            "Precursor Name" %in% names(mrm)) {
          rows <- mrm[!grepl("SIL", mrm[["Precursor Name"]], ignore.case = TRUE), ]
          new_map <- stats::setNames(rows[["Molecule List Name"]], rows[["Precursor Name"]])
          new_map <- new_map[!names(new_map) %in% names(map_vec)]
          map_vec <- c(map_vec, new_map)
        }
      }
    }

    # Source 2: Fallback — parse class from metabolite column names
    # Common lipidomics patterns: "PC(34:1)", "LPC 16:0", "SM_d18:1/16:0",
    # "CE(18:1)", "TG_52:2", "Cer(d18:1/16:0)"
    if (length(map_vec) == 0) {
      mets <- tryCatch(results_metabolite_cols(), error = function(e) character(0))
      if (length(mets) > 0) {
        # Extract prefix before ( or first digit preceded by space/underscore/hyphen
        parsed <- sub("^([A-Za-z]+)[_\\s(\\-].*$", "\\1", mets, perl = TRUE)
        # Only keep mappings where we actually extracted a shorter prefix (not the whole name)
        # Exclude common metadata prefixes that aren't lipid classes
        non_class <- c("sample", "file", "batch", "run", "injection", "group",
                       "class", "type", "plate", "matrix", "data", "meta")
        has_class <- nchar(parsed) < nchar(mets) & nchar(parsed) >= 2 &
                     !tolower(parsed) %in% non_class
        if (any(has_class)) {
          map_vec <- stats::setNames(parsed[has_class], mets[has_class])
        }
      }
    }

    map_vec
  })

  # Reactive: per-metabolite RSD values from pipeline
  results_rsd_values <- shiny::reactive({
    mets <- results_metabolite_cols()
    shiny::req(length(mets) > 0)

    # When viewing batch-corrected results, use batchCorrectR's post-correction RSDs
    if (identical(input$results_source, "batch") && !is.null(rv$bc_result)) {
      rsd_after <- rv$bc_result$qc_rsd_after
      if (!is.null(rsd_after) && length(rsd_after) > 0) {
        common <- intersect(mets, names(rsd_after))
        if (length(common) > 0) {
          vals <- rsd_after[common]
          return(vals)
        }
      }
    }

    # qcCheckR pipeline: use the shared helper so QC Checker tab and
    # Result Explorer tab always read the same `filters$rsd` row and
    # produce identical numbers (SHINY-H3 / SHINY-H4 / RSD-3).
    if (!is.null(rv$qc_result)) {
      core_vals <- mstargetr_results_rsd_core(rv$qc_result)
      if (!is.null(core_vals)) {
        common <- intersect(mets, names(core_vals))
        if (length(common) > 0) {
          vals <- core_vals[common]
          # Ensure output vector includes every requested metabolite,
          # filling absent ones with NA so downstream length matches `mets`.
          out <- stats::setNames(rep(NA_real_, length(mets)), mets)
          out[common] <- vals
          return(out)
        }
      }
    }
    # Fallback: compute from QC samples in data
    df <- results_data()
    if (is.null(df)) return(stats::setNames(rep(NA_real_, length(mets)), mets))
    qc_df <- NULL
    if ("sample_type_factor" %in% names(df)) {
      qc_label <- rv$prefs$qc_label %||% input$batch_qc_label %||% "qc"
      qc_df <- df[tolower(as.character(df$sample_type_factor)) == tolower(qc_label), , drop = FALSE]
      if (nrow(qc_df) < 2) qc_df <- df[tolower(as.character(df$sample_type_factor)) != "sample", , drop = FALSE]
    } else if ("sample_type" %in% names(df)) {
      qc_df <- df[tolower(df$sample_type) == "qc", , drop = FALSE]
    }
    # RSD-1: fail loudly instead of silently falling back to ALL samples
    # (which produced the Result Explorer vs QC Checker count mismatch).
    if (is.null(qc_df) || nrow(qc_df) < 2) {
      shiny::showNotification(
        "Cannot compute RSD: fewer than 2 QC samples detected.",
        type = "warning", duration = 10
      )
      return(stats::setNames(rep(NA_real_, length(mets)), mets))
    }
    vapply(mets, function(m) {
      v <- qc_df[[m]]; v <- v[!is.na(v)]
      if (length(v) < 2 || mean(v) == 0) return(NA_real_)
      sd(v) / abs(mean(v)) * 100
    }, numeric(1))
  })

  # RSD threshold reactives — debounced so rapid slider drags do not trigger
  # expensive downstream recomputation on every tick (250 ms settling time).
  rsd_warn_threshold <- shiny::reactive({ input$results_rsd_warn %||% 20 }) |>
    shiny::debounce(250)
  rsd_fail_threshold <- shiny::reactive({ input$results_rsd_fail %||% 30 }) |>
    shiny::debounce(250)

  # Reactive: per-metabolite QC status (pass/warning/fail)
  results_metabolite_status <- shiny::reactive({
    rsds <- results_rsd_values()
    failed <- tryCatch(rv$qc_result$filters$failed_lipids, error = function(e) character(0))
    if (is.null(failed)) failed <- character(0)
    status <- dplyr::case_when(
      names(rsds) %in% failed        ~ "fail",
      is.na(rsds)                    ~ "fail",
      rsds > rsd_fail_threshold()    ~ "fail",
      rsds > rsd_warn_threshold()    ~ "warning",
      TRUE                           ~ "pass"
    )
    names(status) <- names(rsds)
    status
  })

  # Reactive: filtered metabolites (lipid class + QC status)
  results_filtered_metabolites <- shiny::reactive({
    mets <- results_metabolite_cols()
    shiny::req(length(mets) > 0)
    # Apply lipid class filter
    lc <- input$results_lipid_class
    if (!is.null(lc) && lc != "all") {
      class_map <- tryCatch(results_lipid_class_map(), error = function(e) character(0))
      if (length(class_map) > 0) {
        mets <- mets[mets %in% names(class_map[class_map == lc])]
      }
    }
    # Apply QC status filter
    qf <- input$results_qc_filter
    if (!is.null(qf) && qf != "all") {
      status <- tryCatch(results_metabolite_status(), error = function(e) character(0))
      if (length(status) > 0) {
        mets <- mets[mets %in% names(status[status == qf])]
      }
    }
    mets
  })

  # Observer: update lipid class selector choices
  shiny::observe({
    class_map <- tryCatch(results_lipid_class_map(), error = function(e) character(0))
    if (length(class_map) > 0) {
      classes <- sort(unique(class_map))
      choices <- c("All Classes" = "all", stats::setNames(classes, classes))
    } else {
      choices <- c("All Classes" = "all")
    }
    shiny::updateSelectInput(session, "results_lipid_class", choices = choices, selected = "all")
  })

  # Observer: update deep-dive metabolite selector
  shiny::observe({
    mets <- tryCatch(results_filtered_metabolites(), error = function(e) character(0))
    if (length(mets) > 0) {
      shiny::updateSelectizeInput(session, "results_deep_metabolite",
                                  choices = mets, selected = mets[1],
                                  server = TRUE)
    }
  })

  # -- Filter badge + Clear button ----------------------------------------------

  output$results_filter_badge <- shiny::renderUI({
    total <- tryCatch(length(results_metabolite_cols()), error = function(e) 0)
    shown <- tryCatch(length(results_filtered_metabolites()), error = function(e) 0)
    if (total == 0) return(NULL)
    is_filtered <- shown < total ||
                   (!is.null(input$results_lipid_class) && input$results_lipid_class != "all") ||
                   (!is.null(input$results_qc_filter) && input$results_qc_filter != "all")
    htmltools::tags$div(
      class = "results-sidebar-section",
      htmltools::tags$div(
        class = "d-flex align-items-center justify-content-between mb-2",
        htmltools::tags$span(
          class = paste0("badge ", if (is_filtered) "bg-primary" else "bg-secondary"),
          paste0(shown, " / ", total, " metabolites")
        ),
        if (is_filtered) {
          shiny::actionButton(
            "results_clear_filters", "Clear",
            class = "btn btn-sm btn-outline-secondary py-0 px-2",
            icon = shiny::icon("xmark")
          )
        }
      )
    )
  })

  shiny::observeEvent(input$results_clear_filters, {
    shiny::updateSelectInput(session, "results_lipid_class", selected = "all")
    shiny::updateSelectInput(session, "results_qc_filter", selected = "all")
    shiny::updateSliderInput(session, "results_rsd_warn", value = 20)
    shiny::updateSliderInput(session, "results_rsd_fail", value = 30)
    # Also reset explorer deep-dive and heatmap scope so "Clear" is complete
    mets_now <- tryCatch(results_filtered_metabolites(), error = function(e) character(0))
    if (length(mets_now) > 0) {
      shiny::updateSelectizeInput(session, "results_deep_metabolite",
                                  selected = mets_now[1])
    }
    shiny::updateSelectInput(session, "results_heatmap_scope", selected = "filtered")
  })

  # -- Summary value boxes -----------------------------------------------------

  output$results_n_samples <- shiny::renderUI({
    df <- results_data()
    n  <- if (!is.null(df)) nrow(df) else 0
    failed <- tryCatch(rv$qc_result$filters$failed_samples, error = function(e) character(0))
    failed <- failed %||% character(0)
    # Only count failed samples that exist in the current data
    if (!is.null(df) && "sample_name" %in% names(df)) {
      failed <- intersect(failed, df$sample_name)
    }
    n_failed <- length(failed)
    sub_text <- if (n_failed > 0) {
      htmltools::tags$small(class = "text-danger",
                            paste0(n - n_failed, " passed / ", n_failed, " failed"))
    }
    htmltools::tagList(
      htmltools::tags$div(class = "value-box-value", n),
      sub_text
    )
  })

  output$results_n_metabolites <- shiny::renderUI({
    mets <- tryCatch(results_metabolite_cols(), error = function(e) character(0))
    status <- tryCatch(results_metabolite_status(), error = function(e) character(0))
    n <- length(mets)
    # RSD-2: match qcCheckR's "RSD < fail threshold" semantics — count
    # both pass (below warn) and warning (between warn and fail) rows as
    # "passed" so QC Checker and Result Explorer report identical counts.
    n_pass <- sum(status %in% c("pass", "warning"), na.rm = TRUE)
    n_fail <- sum(status == "fail", na.rm = TRUE)
    sub_text <- if (length(status) > 0) {
      htmltools::tags$small(
        class = if (n_fail > 0) "text-danger" else "text-success",
        sprintf("%d passed (RSD < %g%%) / %d failed",
                n_pass, rsd_fail_threshold(), n_fail)
      )
    }
    htmltools::tagList(
      htmltools::tags$div(class = "value-box-value", n),
      sub_text
    )
  })

  output$results_median_rsd <- shiny::renderUI({
    rsds <- tryCatch(results_rsd_values(), error = function(e) numeric(0))
    val <- "\u2014"
    col_class <- ""
    if (length(rsds) > 0) {
      med <- median(rsds, na.rm = TRUE)
      if (!is.na(med)) {
        val <- paste0(round(med, 1), "%")
        col_class <- if (med < 15) "text-success" else if (med < 25) "text-warning" else "text-danger"
      }
    }
    htmltools::tags$div(class = paste("value-box-value", col_class), val)
  })

  output$results_pct_missing <- shiny::renderUI({
    df   <- results_data()
    mets <- tryCatch(results_metabolite_cols(), error = function(e) character(0))
    val  <- "\u2014"
    col_class <- ""
    if (!is.null(df) && length(mets) > 0) {
      mat <- as.matrix(df[, mets, drop = FALSE])
      pct <- sum(is.na(mat)) / length(mat) * 100
      val <- paste0(round(pct, 1), "%")
      col_class <- if (pct < 5) "text-success" else if (pct < 10) "text-warning" else "text-danger"
    }
    htmltools::tags$div(class = paste("value-box-value", col_class), val)
  })

  output$results_batch_status <- shiny::renderUI({
    if (!is.null(rv$bc_result)) {
      summ <- rv$bc_result$correction_summary
      improved <- if (!is.null(summ) && "improvement" %in% names(summ)) {
        med_imp <- median(summ$improvement, na.rm = TRUE)
        if (!is.na(med_imp)) paste0("Median ", round(med_imp, 1), "% improvement") else ""
      } else ""
      htmltools::tagList(
        htmltools::tags$div(class = "value-box-value text-success", "Corrected"),
        if (nzchar(improved)) htmltools::tags$small(class = "text-muted", improved)
      )
    } else {
      htmltools::tags$div(class = "value-box-value text-muted", "Not applied")
    }
  })

  output$results_n_classes <- shiny::renderUI({
    class_map <- tryCatch(results_lipid_class_map(), error = function(e) character(0))
    n <- if (length(class_map) > 0) length(unique(class_map)) else 0
    htmltools::tags$div(class = "value-box-value", n)
  })

  # -- Shared plotly theme for Results page ------------------------------------
  results_plotly_font <- list(family = "Inter, system-ui, sans-serif")
  results_plotly_layout <- function(p, ...) {
    plotly::layout(p,
      font          = results_plotly_font,
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor  = "rgba(0,0,0,0)",
      margin = list(t = 40, r = 20, b = 60, l = 70),
      ...
    ) |> plotly::config(
      displaylogo = FALSE,
      modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d")
    )
  }

  results_axis_style <- list(
    gridcolor     = "rgba(226,232,240,0.6)",
    gridwidth     = 1,
    linecolor     = "rgba(148,163,184,0.4)",
    linewidth     = 1,
    zerolinecolor = "rgba(148,163,184,0.3)",
    tickfont      = list(size = 11, color = "#64748b")
  )

  results_title_style <- function(txt) {
    list(text = txt, font = list(size = 14, color = "#334155"), x = 0, xanchor = "left")
  }

  results_palette <- c(
    "#0d9488", "#1e40af", "#7c3aed", "#db2777",
    "#ea580c", "#059669", "#0284c7", "#6366f1"
  )

  # -- Tab 1: Quality Dashboard ------------------------------------------------

  # RSD Distribution Histogram
  output$results_rsd_histogram <- plotly::renderPlotly({
    filt_mets <- results_filtered_metabolites()
    all_rsds <- results_rsd_values()
    shiny::req(length(filt_mets) > 0, length(all_rsds) > 0)
    rsds <- all_rsds[intersect(filt_mets, names(all_rsds))]
    rsds <- rsds[!is.na(rsds)]
    shiny::req(length(rsds) > 0)

    rsd_df <- data.frame(rsd = rsds, stringsAsFactors = FALSE)

    # Bin RSD values and color each bin by threshold
    bins <- seq(0, max(rsd_df$rsd, na.rm = TRUE) + 2, length.out = 31)
    bin_mids <- (utils::head(bins, -1) + utils::tail(bins, -1)) / 2
    bin_colors <- ifelse(bin_mids > rsd_fail_threshold(), "#e11d48",
                  ifelse(bin_mids > rsd_warn_threshold(), "#d97706", "#059669"))

    plotly::plot_ly(data = rsd_df, x = ~rsd, type = "histogram",
      marker = list(color = bin_colors, line = list(color = "white", width = 0.5)),
      xbins = list(start = bins[1], end = utils::tail(bins, 1),
                   size = bins[2] - bins[1]),
      hovertemplate = "RSD: %{x:.1f}%<br>Count: %{y}<extra></extra>"
    ) |>
      results_plotly_layout(
        title = results_title_style("RSD Distribution (All Metabolites)"),
        xaxis = c(results_axis_style, list(
          title = list(text = "RSD %", font = list(size = 12, color = "#334155"))
        )),
        yaxis = c(results_axis_style, list(
          title = list(text = "Count", font = list(size = 12, color = "#334155"))
        )),
        shapes = list(
          list(type = "line", x0 = rsd_warn_threshold(), x1 = rsd_warn_threshold(),
               y0 = 0, y1 = 1, yref = "paper",
               line = list(color = "#d97706", dash = "dot", width = 2)),
          list(type = "line", x0 = rsd_fail_threshold(), x1 = rsd_fail_threshold(),
               y0 = 0, y1 = 1, yref = "paper",
               line = list(color = "#e11d48", dash = "dash", width = 2))
        ),
        annotations = list(
          list(x = rsd_warn_threshold(), y = 1, yref = "paper",
               text = paste0(rsd_warn_threshold(), "% Warning"),
               showarrow = FALSE, yanchor = "bottom",
               font = list(size = 10, color = "#d97706")),
          list(x = rsd_fail_threshold(), y = 1, yref = "paper",
               text = paste0(rsd_fail_threshold(), "% Fail"),
               showarrow = FALSE, yanchor = "bottom",
               font = list(size = 10, color = "#e11d48"))
        )
      )
  })

  # Pass/Fail Donut Chart
  output$results_passfail_donut <- plotly::renderPlotly({
    filt_mets <- results_filtered_metabolites()
    all_status <- results_metabolite_status()
    shiny::req(length(filt_mets) > 0, length(all_status) > 0)
    status <- all_status[intersect(filt_mets, names(all_status))]
    shiny::req(length(status) > 0)

    counts <- c(
      Pass    = sum(status == "pass", na.rm = TRUE),
      Warning = sum(status == "warning", na.rm = TRUE),
      Fail    = sum(status == "fail", na.rm = TRUE)
    )
    counts <- counts[counts > 0]

    colors <- c(Pass = "#059669", Warning = "#d97706", Fail = "#e11d48")

    plotly::plot_ly(
      labels = names(counts), values = counts,
      type = "pie", hole = 0.55,
      marker = list(colors = colors[names(counts)],
                    line = list(color = "white", width = 2)),
      textinfo = "label+value",
      textfont = list(size = 12),
      hovertemplate = "<b>%{label}</b><br>%{value} metabolites (%{percent})<extra></extra>"
    ) |>
      results_plotly_layout(
        title = results_title_style("Metabolite QC Status"),
        showlegend = FALSE,
        annotations = list(
          list(text = paste0(sum(counts)), x = 0.5, y = 0.5,
               font = list(size = 22, color = "#334155", family = "Inter, system-ui, sans-serif"),
               showarrow = FALSE)
        )
      )
  })

  # Lipid Class Summary Bar Chart
  output$results_class_summary <- plotly::renderPlotly({
    filt_mets <- results_filtered_metabolites()
    all_status <- results_metabolite_status()
    class_map <- tryCatch(results_lipid_class_map(), error = function(e) character(0))
    shiny::req(length(filt_mets) > 0, length(all_status) > 0)
    status <- all_status[intersect(filt_mets, names(all_status))]
    shiny::req(length(status) > 0)

    # Map metabolites to classes
    mets <- names(status)
    classes <- if (length(class_map) > 0) {
      ifelse(mets %in% names(class_map), class_map[mets], "Unknown")
    } else {
      rep("Unknown", length(mets))
    }

    summ_df <- data.frame(
      metabolite = mets, class = classes, status = status,
      stringsAsFactors = FALSE
    )

    # Aggregate: count per class per status
    agg <- stats::aggregate(metabolite ~ class + status, data = summ_df, FUN = length)
    names(agg)[3] <- "count"

    # Order classes by total count
    class_totals <- stats::aggregate(count ~ class, data = agg, FUN = sum)
    class_totals <- class_totals[order(class_totals$count, decreasing = TRUE), ]
    agg$class <- factor(agg$class, levels = rev(class_totals$class))

    status_colors <- c(pass = "#059669", warning = "#d97706", fail = "#e11d48")
    status_labels <- c(
      pass    = paste0("Pass (<", rsd_warn_threshold(), "%)"),
      warning = paste0("Warning (", rsd_warn_threshold(), "-", rsd_fail_threshold(), "%)"),
      fail    = paste0("Fail (>", rsd_fail_threshold(), "%)")
    )

    p <- plotly::plot_ly()
    for (s in c("pass", "warning", "fail")) {
      s_data <- agg[agg$status == s, , drop = FALSE]
      if (nrow(s_data) > 0) {
        p <- plotly::add_trace(p,
          y = s_data$class, x = s_data$count,
          type = "bar", orientation = "h",
          name = status_labels[s],
          marker = list(color = status_colors[s],
                        line = list(color = "white", width = 0.5)),
          hovertemplate = paste0("<b>%{y}</b><br>", status_labels[s],
                                ": %{x}<extra></extra>")
        )
      }
    }

    p |> results_plotly_layout(
      title  = results_title_style("Quality by Class"),
      barmode = "stack",
      xaxis  = c(results_axis_style, list(
        title = list(text = "Number of Metabolites", font = list(size = 12, color = "#334155"))
      )),
      yaxis  = c(results_axis_style, list(
        title = "", tickfont = list(size = 11, color = "#334155")
      )),
      legend = list(orientation = "h", x = 0, y = -0.12,
                    font = list(size = 11, color = "#64748b")),
      margin = list(t = 50, r = 30, b = 60, l = 160)
    )
  })

  # -- Tab 2: RSD Explorer -----------------------------------------------------

  # Conditional scatter plot UI (only when batch correction exists)
  output$results_rsd_scatter_ui <- shiny::renderUI({
    if (!is.null(rv$bc_result)) {
      htmltools::tags$div(
        class = "mb-4",
        plotly::plotlyOutput("results_rsd_scatter", height = "450px")
      )
    }
  })

  # Before/After RSD scatter
  output$results_rsd_scatter <- plotly::renderPlotly({
    shiny::req(rv$bc_result)
    before <- rv$bc_result$qc_rsd_before
    after  <- rv$bc_result$qc_rsd_after
    shiny::req(length(before) > 0, length(after) > 0)

    common <- intersect(names(before), names(after))
    shiny::req(length(common) > 0)

    class_map <- tryCatch(results_lipid_class_map(), error = function(e) character(0))
    classes <- if (length(class_map) > 0) {
      ifelse(common %in% names(class_map), class_map[common], "Unknown")
    } else {
      rep("Unknown", length(common))
    }
    unique_classes <- unique(classes)
    pal <- results_palette[seq_len(min(length(unique_classes), length(results_palette)))]
    class_colors <- stats::setNames(pal[seq_along(unique_classes)], unique_classes)

    scat_df <- data.frame(
      metabolite = common,
      before = as.numeric(before[common]),
      after  = as.numeric(after[common]),
      class  = classes,
      stringsAsFactors = FALSE
    )
    scat_df <- scat_df[!is.na(scat_df$before) & !is.na(scat_df$after), ]

    p <- plotly::plot_ly()
    for (cl in unique_classes) {
      d <- scat_df[scat_df$class == cl, ]
      p <- plotly::add_trace(p,
        x = d$before, y = d$after,
        type = "scatter", mode = "markers",
        name = cl, text = d$metabolite,
        marker = list(color = class_colors[cl], size = 7, opacity = 0.7,
                      line = list(color = "white", width = 0.5)),
        hovertemplate = paste0("<b>%{text}</b><br>Before: %{x:.1f}%<br>",
                              "After: %{y:.1f}%<extra></extra>")
      )
    }

    max_val <- max(c(scat_df$before, scat_df$after), na.rm = TRUE) * 1.1
    p |> results_plotly_layout(
      title = results_title_style("Batch Correction Effect (Before vs After RSD)"),
      xaxis = c(results_axis_style, list(
        title = list(text = "RSD Before (%)", font = list(size = 12, color = "#334155")),
        range = c(0, max_val)
      )),
      yaxis = c(results_axis_style, list(
        title = list(text = "RSD After (%)", font = list(size = 12, color = "#334155")),
        range = c(0, max_val)
      )),
      shapes = list(
        list(type = "line", x0 = 0, x1 = max_val, y0 = 0, y1 = max_val,
             line = list(color = "rgba(100,116,139,0.3)", dash = "dash", width = 1.5))
      ),
      legend = list(orientation = "h", x = 0, y = -0.15,
                    font = list(size = 10, color = "#64748b"))
    )
  })

  # RSD Bar Chart (filtered, with optional before/after)
  output$results_rsd_bar <- plotly::renderPlotly({
    mets <- results_filtered_metabolites()
    shiny::req(length(mets) > 0)

    rsds <- results_rsd_values()
    rsd_df <- data.frame(
      metabolite = mets,
      rsd = as.numeric(rsds[mets]),
      stringsAsFactors = FALSE
    )
    rsd_df <- rsd_df[!is.na(rsd_df$rsd), ]
    rsd_df <- rsd_df[order(rsd_df$rsd, decreasing = TRUE), ]
    rsd_df <- utils::head(rsd_df, 50)
    rsd_df$metabolite <- factor(rsd_df$metabolite, levels = rev(rsd_df$metabolite))

    rsd_df$colour <- ifelse(rsd_df$rsd > rsd_fail_threshold(), "#e11d48",
                     ifelse(rsd_df$rsd > rsd_warn_threshold(), "#d97706", "#059669"))

    p <- plotly::plot_ly(
      data = rsd_df, y = ~metabolite, x = ~rsd,
      type = "bar", orientation = "h",
      name = "Current RSD",
      marker = list(color = rsd_df$colour,
                    line = list(color = "rgba(255,255,255,0.3)", width = 0.5)),
      text = ~paste0(round(rsd, 1), "%"),
      textposition = "outside",
      textfont = list(size = 10, color = "#64748b"),
      hovertemplate = "<b>%{y}</b><br>RSD: %{x:.1f}%<extra></extra>"
    )

    # Add before-correction bars if batch correction exists
    if (!is.null(rv$bc_result)) {
      before <- rv$bc_result$qc_rsd_before
      if (length(before) > 0) {
        before_vals <- as.numeric(before[match(as.character(rsd_df$metabolite), names(before))])
        if (any(!is.na(before_vals))) {
          p <- plotly::add_trace(p,
            y = rsd_df$metabolite, x = before_vals,
            type = "bar", orientation = "h",
            name = "Before Correction",
            marker = list(color = "rgba(148,163,184,0.35)",
                          line = list(color = "rgba(255,255,255,0.3)", width = 0.5)),
            hovertemplate = "<b>%{y}</b><br>Before: %{x:.1f}%<extra></extra>"
          )
        }
      }
    }

    max_rsd <- max(rsd_df$rsd, na.rm = TRUE)
    p |> results_plotly_layout(
      title   = results_title_style("RSD by Metabolite"),
      barmode = "group",
      xaxis   = c(results_axis_style, list(
        title = list(text = "RSD %", font = list(size = 12, color = "#334155")),
        range = c(0, max_rsd * 1.25)
      )),
      yaxis   = c(results_axis_style, list(
        title = "", tickfont = list(size = 10, color = "#334155")
      )),
      margin  = list(t = 50, r = 60, b = 60, l = 180),
      shapes  = list(
        list(type = "line", x0 = rsd_warn_threshold(), x1 = rsd_warn_threshold(),
             y0 = -0.5, y1 = nrow(rsd_df) - 0.5,
             line = list(color = "#d97706", dash = "dot", width = 1.5)),
        list(type = "line", x0 = rsd_fail_threshold(), x1 = rsd_fail_threshold(),
             y0 = -0.5, y1 = nrow(rsd_df) - 0.5,
             line = list(color = "#e11d48", dash = "dash", width = 1.5))
      ),
      annotations = list(
        list(x = rsd_warn_threshold(), y = nrow(rsd_df) - 0.5,
             text = paste0(rsd_warn_threshold(), "%"),
             showarrow = FALSE, yanchor = "bottom",
             font = list(size = 9, color = "#d97706")),
        list(x = rsd_fail_threshold(), y = nrow(rsd_df) - 0.5,
             text = paste0(rsd_fail_threshold(), "%"),
             showarrow = FALSE, yanchor = "bottom",
             font = list(size = 9, color = "#e11d48"))
      ),
      legend = list(orientation = "h", x = 0, y = -0.08,
                    font = list(size = 10, color = "#64748b"))
    )
  })

  # Concentration vs RSD scatter
  output$results_conc_vs_rsd <- plotly::renderPlotly({
    df <- results_data()
    shiny::req(df)
    mets <- results_filtered_metabolites()
    shiny::req(length(mets) > 0)
    rsds <- results_rsd_values()
    shiny::req(length(rsds) > 0)

    common <- intersect(mets, names(rsds))
    shiny::req(length(common) > 0)

    mean_conc <- colMeans(df[, common, drop = FALSE], na.rm = TRUE)
    rsd_vals  <- as.numeric(rsds[common])

    class_map <- tryCatch(results_lipid_class_map(), error = function(e) character(0))
    classes <- if (length(class_map) > 0) {
      ifelse(common %in% names(class_map), class_map[common], "Unknown")
    } else rep("Unknown", length(common))

    scat_df <- data.frame(
      metabolite = common, conc = mean_conc, rsd = rsd_vals,
      class = classes, stringsAsFactors = FALSE
    )
    scat_df <- scat_df[!is.na(scat_df$conc) & !is.na(scat_df$rsd) & scat_df$conc > 0, ]
    if (nrow(scat_df) == 0) return(NULL)

    unique_classes <- unique(scat_df$class)
    pal <- results_palette[seq_len(min(length(unique_classes), length(results_palette)))]
    class_colors <- stats::setNames(pal[seq_along(unique_classes)], unique_classes)

    p <- plotly::plot_ly()
    for (cl in unique_classes) {
      d <- scat_df[scat_df$class == cl, ]
      p <- plotly::add_trace(p,
        x = d$conc, y = d$rsd,
        type = "scatter", mode = "markers",
        name = cl, text = d$metabolite,
        marker = list(color = class_colors[cl], size = 7, opacity = 0.7,
                      line = list(color = "white", width = 0.5)),
        hovertemplate = paste0("<b>%{text}</b><br>Mean Conc: %{x:.2f}<br>",
                              "RSD: %{y:.1f}%<extra></extra>")
      )
    }

    p |> results_plotly_layout(
      title = results_title_style("Mean Concentration vs RSD"),
      xaxis = c(results_axis_style, list(
        title = list(text = "Mean Concentration", font = list(size = 12, color = "#334155")),
        type = "log"
      )),
      yaxis = c(results_axis_style, list(
        title = list(text = "RSD (%)", font = list(size = 12, color = "#334155"))
      )),
      shapes = list(
        list(type = "line", x0 = 0, x1 = 1, xref = "paper",
             y0 = rsd_warn_threshold(), y1 = rsd_warn_threshold(),
             line = list(color = "#d97706", dash = "dot", width = 1.5)),
        list(type = "line", x0 = 0, x1 = 1, xref = "paper",
             y0 = rsd_fail_threshold(), y1 = rsd_fail_threshold(),
             line = list(color = "#e11d48", dash = "dash", width = 1.5))
      ),
      legend = list(orientation = "h", x = 0, y = -0.15,
                    font = list(size = 10, color = "#64748b"))
    )
  })

  # -- Tab 3: Metabolite Deep Dive ---------------------------------------------

  # Metabolite info card
  output$results_metabolite_info <- shiny::renderUI({
    met <- input$results_deep_metabolite
    shiny::req(met, nzchar(met))

    rsds <- tryCatch(results_rsd_values(), error = function(e) numeric(0))
    status <- tryCatch(results_metabolite_status(), error = function(e) character(0))
    class_map <- tryCatch(results_lipid_class_map(), error = function(e) character(0))

    rsd_val <- if (met %in% names(rsds)) paste0(round(rsds[met], 1), "%") else "\u2014"
    stat_val <- if (met %in% names(status)) status[met] else "unknown"
    class_val <- if (met %in% names(class_map)) class_map[met] else "Unknown"
    stat_badge <- switch(stat_val,
      pass    = htmltools::tags$span(class = "badge bg-success", "Pass"),
      warning = htmltools::tags$span(class = "badge bg-warning text-dark", "Warning"),
      fail    = htmltools::tags$span(class = "badge bg-danger", "Fail"),
      htmltools::tags$span(class = "badge bg-secondary", "Unknown")
    )

    # Missing % for this metabolite
    df <- results_data()
    miss_pct <- if (!is.null(df) && met %in% names(df)) {
      paste0(round(sum(is.na(df[[met]])) / nrow(df) * 100, 1), "%")
    } else "\u2014"

    htmltools::tags$div(
      class = "card mb-3",
      htmltools::tags$div(
        class = "card-body py-2",
        htmltools::tags$div(
          class = "d-flex flex-wrap gap-4 align-items-center",
          htmltools::tags$div(
            htmltools::tags$small(class = "text-muted text-uppercase", "Class"),
            htmltools::tags$div(class = "fw-semibold", class_val)
          ),
          htmltools::tags$div(
            htmltools::tags$small(class = "text-muted text-uppercase", "QC RSD"),
            htmltools::tags$div(class = "fw-semibold", rsd_val)
          ),
          htmltools::tags$div(
            htmltools::tags$small(class = "text-muted text-uppercase", "Status"),
            htmltools::tags$div(stat_badge)
          ),
          htmltools::tags$div(
            htmltools::tags$small(class = "text-muted text-uppercase", "Missing"),
            htmltools::tags$div(class = "fw-semibold", miss_pct)
          )
        )
      )
    )
  })

  # Boxplot for selected metabolite (now uses deep_metabolite)
  output$results_boxplot <- plotly::renderPlotly({
    df  <- results_data()
    met <- input$results_deep_metabolite
    shiny::req(df, met, nzchar(met), met %in% names(df))

    type_col <- if ("sample_type_factor" %in% names(df)) "sample_type_factor"
                else if ("sample_type" %in% names(df)) "sample_type"
                else NULL

    if (!is.null(type_col)) {
      groups <- unique(as.character(df[[type_col]]))
      n_grp  <- length(groups)
      pal    <- results_palette[seq_len(min(n_grp, length(results_palette)))]

      p <- plotly::plot_ly()
      for (i in seq_along(groups)) {
        grp_data <- df[as.character(df[[type_col]]) == groups[i], , drop = FALSE]
        p <- plotly::add_trace(p,
          y = grp_data[[met]], type = "box", name = groups[i],
          boxpoints = "suspectedoutliers", jitter = 0.3, pointpos = -1.5,
          marker = list(color = pal[i], outliercolor = pal[i],
                        size = 4, opacity = 0.5,
                        line = list(color = pal[i], width = 1)),
          line = list(color = pal[i], width = 1.5),
          fillcolor = paste0(pal[i], "22"),
          hovertemplate = paste0("<b>", groups[i], "</b><br>",
                                met, ": %{y:.3f}<extra></extra>")
        )
      }
      p |> results_plotly_layout(
        title     = results_title_style(paste0("Distribution: ", met)),
        yaxis     = c(results_axis_style, list(
          title = list(text = met, font = list(size = 12, color = "#334155")))),
        xaxis     = c(results_axis_style, list(
          title = list(text = "Sample Type", font = list(size = 12, color = "#334155")))),
        legend    = list(orientation = "h", x = 0, y = -0.15,
                         font = list(size = 11, color = "#64748b")),
        showlegend = TRUE
      )
    } else {
      plotly::plot_ly(y = df[[met]], type = "box",
        boxpoints = "suspectedoutliers", jitter = 0.3,
        marker = list(color = results_palette[1], size = 4, opacity = 0.5),
        line = list(color = results_palette[1], width = 1.5),
        fillcolor = paste0(results_palette[1], "22"),
        hovertemplate = paste0(met, ": %{y:.3f}<extra></extra>")
      ) |> results_plotly_layout(
        title = results_title_style(paste0("Distribution: ", met)),
        yaxis = c(results_axis_style, list(
          title = list(text = met, font = list(size = 12, color = "#334155"))))
      )
    }
  })

  # Cached LOESS: keyed on metabolite + data so re-render on axis/type change is free
  results_runorder_loess <- shiny::reactive({
    df  <- results_data()
    met <- input$results_deep_metabolite
    shiny::req(df, met, nzchar(met), met %in% names(df))
    type_col <- if ("sample_type_factor" %in% names(df)) "sample_type_factor"
                else if ("sample_type"   %in% names(df)) "sample_type"
                else NULL
    if (is.null(type_col)) return(NULL)
    x_vals <- if ("sample_run_index"  %in% names(df)) df$sample_run_index
              else if ("run_order"    %in% names(df)) df$run_order
              else if ("injection_order" %in% names(df)) df$injection_order
              else seq_len(nrow(df))
    sample_idx <- tolower(as.character(df[[type_col]])) == "sample"
    if (sum(sample_idx) <= 4) return(NULL)
    tryCatch(
      list(x = x_vals[sample_idx],
           fit = stats::loess(df[[met]][sample_idx] ~ x_vals[sample_idx], span = 0.3)),
      error = function(e) NULL)
  })

  # Run-order scatter (now uses deep_metabolite)
  output$results_runorder <- plotly::renderPlotly({
    df  <- results_data()
    met <- input$results_deep_metabolite
    shiny::req(df, met, nzchar(met), met %in% names(df))

    if ("sample_run_index" %in% names(df)) {
      x_vals <- df$sample_run_index; x_lab <- "Run Order"
    } else if ("run_order" %in% names(df)) {
      x_vals <- df$run_order; x_lab <- "Run Order"
    } else if ("injection_order" %in% names(df)) {
      x_vals <- df$injection_order; x_lab <- "Injection Order"
    } else {
      x_vals <- seq_len(nrow(df)); x_lab <- "Sample Index"
    }

    type_col <- if ("sample_type_factor" %in% names(df)) "sample_type_factor"
                else if ("sample_type" %in% names(df)) "sample_type"
                else NULL

    hover_name <- if ("sample_name" %in% names(df)) df$sample_name else seq_len(nrow(df))

    if (!is.null(type_col)) {
      groups <- unique(as.character(df[[type_col]]))
      n_grp  <- length(groups)
      pal    <- results_palette[seq_len(min(n_grp, length(results_palette)))]

      p <- plotly::plot_ly()
      for (i in seq_along(groups)) {
        idx <- as.character(df[[type_col]]) == groups[i]
        is_sample <- tolower(groups[i]) == "sample"
        p <- plotly::add_trace(p,
          x = x_vals[idx], y = df[[met]][idx],
          type = "scatter", mode = "markers",
          name = groups[i], text = hover_name[idx],
          marker = list(
            color = pal[i],
            size = if (is_sample) 5 else 8,
            opacity = if (is_sample) 0.4 else 0.85,
            symbol = if (is_sample) "circle" else "diamond",
            line = list(color = "white", width = if (is_sample) 0 else 1)),
          hovertemplate = paste0("<b>%{text}</b><br>", x_lab, ": %{x}<br>",
                                met, ": %{y:.3f}<br>Type: ", groups[i], "<extra></extra>")
        )
      }
      loess_cache <- tryCatch(results_runorder_loess(), error = function(e) NULL)
      if (!is.null(loess_cache)) {
        ord <- order(loess_cache$x)
        p <- plotly::add_trace(p,
          x = loess_cache$x[ord], y = stats::predict(loess_cache$fit)[ord],
          type = "scatter", mode = "lines", name = "Trend",
          line = list(color = "rgba(100,116,139,0.5)", width = 2, dash = "dot"),
          hoverinfo = "skip", showlegend = TRUE)
      }

      p |> results_plotly_layout(
        title  = results_title_style(paste0("Run Order: ", met)),
        xaxis  = c(results_axis_style, list(
          title = list(text = x_lab, font = list(size = 12, color = "#334155")))),
        yaxis  = c(results_axis_style, list(
          title = list(text = met, font = list(size = 12, color = "#334155")))),
        legend = list(orientation = "h", x = 0, y = -0.15,
                      font = list(size = 11, color = "#64748b"),
                      itemsizing = "constant")
      )
    } else {
      plotly::plot_ly(
        x = x_vals, y = df[[met]], type = "scatter", mode = "markers",
        text = hover_name,
        marker = list(color = results_palette[1], size = 6, opacity = 0.6),
        hovertemplate = paste0("<b>%{text}</b><br>", x_lab, ": %{x}<br>",
                              met, ": %{y:.3f}<extra></extra>")
      ) |> results_plotly_layout(
        title = results_title_style(paste0("Run Order: ", met)),
        xaxis = c(results_axis_style, list(
          title = list(text = x_lab, font = list(size = 12, color = "#334155")))),
        yaxis = c(results_axis_style, list(
          title = list(text = met, font = list(size = 12, color = "#334155"))))
      )
    }
  })

  # Conditional before/after run-order UI
  output$results_before_after_ui <- shiny::renderUI({
    shiny::req(rv$bc_result, input$results_deep_metabolite)
    met <- input$results_deep_metabolite
    if (!nzchar(met)) return(NULL)
    htmltools::tags$div(
      class = "row mb-3",
      htmltools::tags$div(class = "col-12",
        htmltools::tags$hr(),
        htmltools::tags$h6(class = "text-muted", "Before / After Batch Correction")
      ),
      htmltools::tags$div(class = "col-md-6",
        plotly::plotlyOutput("results_deep_before", height = "350px")),
      htmltools::tags$div(class = "col-md-6",
        plotly::plotlyOutput("results_deep_after", height = "350px"))
    )
  })

  # Helper: build run-order scatter for a given data frame + metabolite.
  # Delegates to MStargetR:::re_plot_runorder so the GUI and the figures
  # written by resultsExplorerR(advanced_plots = TRUE) stay in sync.
  build_runorder_scatter <- function(df, met, title_prefix) {
    if (is.null(df) || !met %in% names(df)) return(NULL)
    plot_pair <- MStargetR:::re_plot_runorder(
      df, metabolite = met, title_prefix = title_prefix
    )
    if (is.null(plot_pair)) return(NULL)
    plot_pair$interactive
  }

  output$results_deep_before <- plotly::renderPlotly({
    met <- input$results_deep_metabolite
    shiny::req(rv$bc_result, met, nzchar(met))
    # Use the input data that was fed to batch correction
    df <- rv$bc_data
    if (is.null(df)) {
      df <- tryCatch({
        dplyr::bind_rows(rv$qc_result$data$concentration$imputed)
      }, error = function(e) NULL)
    }
    shiny::req(df, met %in% names(df))
    build_runorder_scatter(df, met, "Before Correction")
  })

  output$results_deep_after <- plotly::renderPlotly({
    met <- input$results_deep_metabolite
    shiny::req(rv$bc_result, met, nzchar(met))
    df <- rv$bc_result$corrected_data
    shiny::req(df, met %in% names(df))
    build_runorder_scatter(df, met, "After Correction")
  })

  # -- Tab 4: Heatmap ----------------------------------------------------------

  # Cached per-metabolite variances keyed only on results_data() (not on heatmap inputs)
  results_metabolite_vars <- shiny::reactive({
    df   <- results_data()
    mets <- results_metabolite_cols()
    shiny::req(df, length(mets) > 0)
    vapply(mets, function(m) var(df[[m]], na.rm = TRUE), numeric(1))
  })

  # Cached scaled heatmap matrix keyed on data + scope + sample filter
  results_heatmap_mat <- shiny::reactive({
    df <- results_data()
    shiny::req(df)
    scope <- input$results_heatmap_scope %||% "filtered"
    mets  <- if (scope == "filtered") results_filtered_metabolites()
             else results_metabolite_cols()
    shiny::req(length(mets) > 0)
    if (scope == "top30" || length(mets) > 100) {
      all_vars <- results_metabolite_vars()
      common   <- intersect(mets, names(all_vars))
      top_n    <- min(if (scope == "top30") 30L else 100L, length(common))
      mets     <- names(sort(all_vars[common], decreasing = TRUE))[seq_len(top_n)]
    }
    sample_filter <- input$results_heatmap_samples %||% "all"
    if (sample_filter != "all") {
      type_col <- if ("sample_type_factor" %in% names(df)) "sample_type_factor"
                  else if ("sample_type"   %in% names(df)) "sample_type"
                  else NULL
      if (!is.null(type_col)) {
        df <- if (sample_filter == "qc")
          df[tolower(as.character(df[[type_col]])) != "sample", , drop = FALSE]
        else
          df[tolower(as.character(df[[type_col]])) == "sample", , drop = FALSE]
      }
    }
    shiny::req(nrow(df) > 0)
    # Cap display at 200 samples x 100 metabolites to avoid browser serialisation lag
    if (nrow(df)   > 200L) df   <- df[seq_len(200L), , drop = FALSE]
    if (length(mets) > 100L) mets <- mets[seq_len(100L)]
    mat <- scale(as.matrix(df[, mets, drop = FALSE]))
    mat[!is.finite(mat)] <- 0
    list(mat = mat, row_labels = if ("sample_name" %in% names(df)) df$sample_name
                                 else seq_len(nrow(df)),
         mets = mets, scope = scope)
  })

  output$results_heatmap <- plotly::renderPlotly({
    hm  <- results_heatmap_mat()
    shiny::req(hm)
    mat        <- hm$mat
    row_labels <- hm$row_labels
    mets       <- hm$mets
    scope      <- hm$scope
    title_txt <- if (scope == "top30") {
      "Concentration Heatmap (Top 30 by Variance)"
    } else {
      paste0("Concentration Heatmap (", length(mets), " Metabolites)")
    }

    plotly::plot_ly(
      x = colnames(mat), y = row_labels,
      z = mat, type = "heatmap",
      colorscale = list(
        list(0,   "#0d1b2a"), list(0.2, "#1b3a5c"),
        list(0.4, "#0d9488"), list(0.6, "#2dd4bf"),
        list(0.8, "#fbbf24"), list(1,   "#fef3c7")
      ),
      colorbar = list(
        title = list(text = "Z-score", font = list(size = 11, color = "#64748b")),
        thickness = 12, len = 0.6,
        tickfont = list(size = 10, color = "#64748b"),
        outlinewidth = 0
      ),
      hovertemplate = paste0("<b>%{x}</b><br>Sample: %{y}<br>",
                            "Z-score: %{z:.2f}<extra></extra>")
    ) |>
      results_plotly_layout(
        title  = results_title_style(title_txt),
        xaxis  = c(results_axis_style, list(
          title = "", tickangle = -45,
          tickfont = list(size = 9, color = "#64748b"))),
        yaxis  = c(results_axis_style, list(
          title = "", tickfont = list(size = 8, color = "#64748b"))),
        margin = list(t = 50, r = 30, b = 130, l = 140)
      )
  })

  # -- Tab 5: Data Table --------------------------------------------------------
  # Per-view helper functions that each return a DT::datatable (SH-015).

  results_dt_concentration <- function(df) {
    shiny::req(df)
    mets <- results_filtered_metabolites()
    meta <- intersect(meta_cols, names(df))
    display_df <- df[, c(meta, mets), drop = FALSE]
    DT::datatable(display_df, filter = "top",
      options = list(pageLength = 20, scrollX = TRUE, dom = "Bfrtip",
                     buttons = c("copy", "csv")),
      extensions = "Buttons")
  }

  results_dt_rsd_summary <- function() {
    mets <- results_filtered_metabolites()
    shiny::req(length(mets) > 0)
    rsds <- results_rsd_values()
    status <- results_metabolite_status()
    class_map <- tryCatch(results_lipid_class_map(), error = function(e) character(0))
    rsd_tbl <- data.frame(
      Metabolite = mets,
      `Class` = if (length(class_map) > 0) {
        ifelse(mets %in% names(class_map), class_map[mets], "Unknown")
      } else rep("Unknown", length(mets)),
      `RSD (%)` = round(as.numeric(rsds[mets]), 1),
      Status = as.character(status[mets]),
      stringsAsFactors = FALSE, check.names = FALSE
    )
    if (!is.null(rv$bc_result)) {
      before <- rv$bc_result$qc_rsd_before
      after  <- rv$bc_result$qc_rsd_after
      rsd_tbl$`RSD Before (%)` <- round(as.numeric(before[mets]), 1)
      rsd_tbl$`RSD After (%)`  <- round(as.numeric(after[mets]), 1)
      rsd_tbl$Improved <- ifelse(
        !is.na(rsd_tbl$`RSD Before (%)`) & !is.na(rsd_tbl$`RSD After (%)`),
        ifelse(rsd_tbl$`RSD After (%)` < rsd_tbl$`RSD Before (%)`, "Yes", "No"),
        "—"
      )
    }
    rsd_tbl <- rsd_tbl[order(rsd_tbl$`RSD (%)`, decreasing = TRUE), ]
    DT::datatable(rsd_tbl, filter = "top",
      options = list(pageLength = 25, scrollX = TRUE, dom = "Bfrtip",
                     buttons = c("copy", "csv")),
      extensions = "Buttons")
  }

  results_dt_failed_mets <- function() {
    failed <- tryCatch(rv$qc_result$filters$failed_lipids, error = function(e) character(0))
    if (is.null(failed)) failed <- character(0)
    rsds <- tryCatch(results_rsd_values(), error = function(e) numeric(0))
    status <- tryCatch(results_metabolite_status(), error = function(e) character(0))
    if (length(status) > 0) {
      rsd_failed <- names(status[status == "fail"])
      failed <- unique(c(failed, rsd_failed))
    }
    if (length(failed) == 0) {
      return(DT::datatable(
        data.frame(Message = "No failed metabolites detected.", check.names = FALSE),
        options = list(dom = "t")))
    }
    class_map <- tryCatch(results_lipid_class_map(), error = function(e) character(0))
    failed_df <- data.frame(
      Metabolite = failed,
      Class = if (length(class_map) > 0) {
        ifelse(failed %in% names(class_map), class_map[failed], "Unknown")
      } else rep("Unknown", length(failed)),
      `RSD (%)` = if (length(rsds) > 0) round(as.numeric(rsds[failed]), 1) else NA_real_,
      Status = if (length(status) > 0) as.character(status[failed]) else "fail",
      stringsAsFactors = FALSE, check.names = FALSE
    )
    failed_df <- failed_df[order(failed_df$`RSD (%)`, decreasing = TRUE, na.last = TRUE), ]
    DT::datatable(failed_df, filter = "top",
      options = list(pageLength = 25, scrollX = TRUE, dom = "Bfrtip",
                     buttons = c("copy", "csv")),
      extensions = "Buttons")
  }

  results_dt_failed_samples <- function() {
    failed <- tryCatch(rv$qc_result$filters$failed_samples, error = function(e) character(0))
    if (is.null(failed)) failed <- character(0)
    mv_tbl <- tryCatch(rv$qc_result$filters$samples.missingValues, error = function(e) NULL)
    if (length(failed) == 0 && is.null(mv_tbl)) {
      df <- results_data()
      mets <- tryCatch(results_metabolite_cols(), error = function(e) character(0))
      if (!is.null(df) && length(mets) > 0) {
        miss_pct <- rowSums(is.na(df[, mets, drop = FALSE])) / length(mets) * 100
        sample_ids <- if ("sample_name" %in% names(df)) df$sample_name else seq_len(nrow(df))
        sample_types <- if ("sample_type_factor" %in% names(df)) {
          as.character(df$sample_type_factor)
        } else if ("sample_type" %in% names(df)) {
          as.character(df$sample_type)
        } else rep("unknown", nrow(df))
        sample_mv_df <- data.frame(
          Sample = sample_ids,
          `Sample Type` = sample_types,
          `Missing (%)` = round(miss_pct, 1),
          `Missing Count` = rowSums(is.na(df[, mets, drop = FALSE])),
          stringsAsFactors = FALSE, check.names = FALSE
        )
        sample_mv_df <- sample_mv_df[sample_mv_df$`Missing (%)` > 0, , drop = FALSE]
        if (nrow(sample_mv_df) == 0) {
          return(DT::datatable(
            data.frame(Message = "No failed samples detected.", check.names = FALSE),
            options = list(dom = "t")))
        }
        sample_mv_df <- sample_mv_df[order(sample_mv_df$`Missing (%)`, decreasing = TRUE), ]
        return(DT::datatable(sample_mv_df, filter = "top",
          options = list(pageLength = 25, scrollX = TRUE, dom = "Bfrtip",
                         buttons = c("copy", "csv")),
          extensions = "Buttons"))
      }
      return(DT::datatable(
        data.frame(Message = "No failed samples detected.", check.names = FALSE),
        options = list(dom = "t")))
    }
    if (!is.null(mv_tbl) && is.data.frame(mv_tbl)) {
      DT::datatable(mv_tbl, filter = "top",
        options = list(pageLength = 25, scrollX = TRUE, dom = "Bfrtip",
                       buttons = c("copy", "csv")),
        extensions = "Buttons")
    } else {
      DT::datatable(
        data.frame(`Failed Sample` = failed, check.names = FALSE),
        options = list(pageLength = 25, dom = "Bfrtip", buttons = c("copy", "csv")),
        extensions = "Buttons")
    }
  }

  results_dt_missing_values <- function() {
    mv <- tryCatch(rv$qc_result$filters$lipid.missingValues, error = function(e) NULL)
    if (!is.null(mv)) {
      if (!is.null(mv$allPlates) && is.list(mv$allPlates)) {
        combined <- tryCatch(dplyr::bind_rows(mv$allPlates), error = function(e) NULL)
        if (!is.null(combined)) {
          return(DT::datatable(combined, filter = "top",
            options = list(pageLength = 25, scrollX = TRUE, dom = "Bfrtip",
                           buttons = c("copy", "csv")),
            extensions = "Buttons"))
        }
      }
      if (!is.null(mv$summary) && is.data.frame(mv$summary)) {
        return(DT::datatable(mv$summary, filter = "top",
          options = list(pageLength = 25, scrollX = TRUE, dom = "Bfrtip",
                         buttons = c("copy", "csv")),
          extensions = "Buttons"))
      }
    }
    df <- results_data()
    mets <- tryCatch(results_filtered_metabolites(), error = function(e) character(0))
    if (!is.null(df) && length(mets) > 0) {
      n_total <- nrow(df)
      miss_count <- vapply(mets, function(m) sum(is.na(df[[m]])), integer(1))
      miss_pct <- round(miss_count / n_total * 100, 1)
      class_map <- tryCatch(results_lipid_class_map(), error = function(e) character(0))
      mv_df <- data.frame(
        Metabolite = mets,
        Class = if (length(class_map) > 0) {
          ifelse(mets %in% names(class_map), class_map[mets], "Unknown")
        } else rep("Unknown", length(mets)),
        `Missing Count` = miss_count,
        `Missing (%)` = miss_pct,
        `Total Samples` = rep(n_total, length(mets)),
        stringsAsFactors = FALSE, check.names = FALSE
      )
      mv_df <- mv_df[mv_df$`Missing Count` > 0, , drop = FALSE]
      if (nrow(mv_df) == 0) {
        return(DT::datatable(
          data.frame(Message = "No missing values detected.", check.names = FALSE),
          options = list(dom = "t")))
      }
      mv_df <- mv_df[order(mv_df$`Missing (%)`, decreasing = TRUE), ]
      return(DT::datatable(mv_df, filter = "top",
        options = list(pageLength = 25, scrollX = TRUE, dom = "Bfrtip",
                       buttons = c("copy", "csv")),
        extensions = "Buttons"))
    }
    DT::datatable(
      data.frame(Message = "No data available for missing value analysis.", check.names = FALSE),
      options = list(dom = "t"))
  }

  results_dt_sample_quality <- function() {
    df <- results_data()
    shiny::req(df)
    mets <- tryCatch(results_metabolite_cols(), error = function(e) character(0))
    shiny::req(length(mets) > 0)
    mat <- as.matrix(df[, mets, drop = FALSE])
    sample_ids <- if ("sample_name" %in% names(df)) df$sample_name else seq_len(nrow(df))
    sample_types <- if ("sample_type_factor" %in% names(df)) as.character(df$sample_type_factor)
                    else if ("sample_type" %in% names(df)) as.character(df$sample_type)
                    else rep("unknown", nrow(df))
    batches <- if ("sample_plate_id" %in% names(df)) as.character(df$sample_plate_id)
               else if ("batch" %in% names(df)) as.character(df$batch)
               else rep("-", nrow(df))
    sq_df <- data.frame(
      Sample = sample_ids,
      `Sample Type` = sample_types,
      Batch = batches,
      Median = round(apply(mat, 1, stats::median, na.rm = TRUE), 2),
      Mean = round(rowMeans(mat, na.rm = TRUE), 2),
      `CV (%)` = round(apply(mat, 1, function(r) {
        m <- mean(r, na.rm = TRUE)
        if (is.na(m) || m == 0) return(NA_real_)
        stats::sd(r, na.rm = TRUE) / m * 100
      }), 1),
      `Missing Count` = rowSums(is.na(mat)),
      `Missing (%)` = round(rowSums(is.na(mat)) / ncol(mat) * 100, 1),
      `Total Metabolites` = ncol(mat),
      stringsAsFactors = FALSE, check.names = FALSE
    )
    sq_df <- sq_df[order(sq_df$`CV (%)`, decreasing = TRUE, na.last = TRUE), ]
    DT::datatable(sq_df, filter = "top",
      options = list(pageLength = 25, scrollX = TRUE, dom = "Bfrtip",
                     buttons = c("copy", "csv")),
      extensions = "Buttons")
  }

  output$results_data_table <- DT::renderDT({
    view <- input$results_table_view %||% "concentration"
    df   <- results_data()
    switch(view,
      concentration  = results_dt_concentration(df),
      rsd_summary    = results_dt_rsd_summary(),
      failed_mets    = results_dt_failed_mets(),
      failed_samples = results_dt_failed_samples(),
      missing_values = results_dt_missing_values(),
      sample_quality = results_dt_sample_quality()
    )
  })

  # Download handler for results CSV
  output$results_download_csv <- shiny::downloadHandler(
    filename = function() {
      src <- input$results_source
      paste0("MStargetR_results_", src, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      tryCatch({
        df <- results_data()
        shiny::req(df)
        mets <- tryCatch(results_filtered_metabolites(), error = function(e) NULL)
        if (!is.null(mets) && length(mets) > 0) {
          meta <- intersect(meta_cols, names(df))
          df <- df[, c(meta, mets), drop = FALSE]
        }
        readr::write_csv(df, file)
      }, error = function(e) {
        readr::write_csv(
          data.frame(Error = paste("Export failed:", e$message),
                     stringsAsFactors = FALSE),
          file
        )
      })
    }
  )

  # Download handler for current table view
  output$results_download_table <- shiny::downloadHandler(
    filename = function() {
      view <- input$results_table_view %||% "concentration"
      paste0("MStargetR_", view, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      view <- input$results_table_view %||% "concentration"
      df   <- results_data()
      export_df <- NULL

      if (view == "concentration" && !is.null(df)) {
        mets <- tryCatch(results_filtered_metabolites(), error = function(e) NULL)
        if (!is.null(mets) && length(mets) > 0) {
          meta <- intersect(meta_cols, names(df))
          export_df <- df[, c(meta, mets), drop = FALSE]
        } else {
          export_df <- df
        }
      } else if (view == "rsd_summary") {
        mets <- tryCatch(results_filtered_metabolites(), error = function(e) character(0))
        rsds <- tryCatch(results_rsd_values(), error = function(e) numeric(0))
        class_map <- tryCatch(results_lipid_class_map(), error = function(e) character(0))
        status <- tryCatch(results_metabolite_status(), error = function(e) character(0))
        if (length(mets) > 0 && length(rsds) > 0) {
          export_df <- data.frame(
            Metabolite = mets,
            Class = if (length(class_map) > 0) ifelse(mets %in% names(class_map), class_map[mets], "Unknown") else "Unknown",
            `RSD (%)` = round(as.numeric(rsds[mets]), 2),
            Status = if (length(status) > 0) as.character(status[mets]) else "unknown",
            stringsAsFactors = FALSE, check.names = FALSE
          )
        }
      } else if (view == "failed_mets") {
        status <- tryCatch(results_metabolite_status(), error = function(e) character(0))
        rsds <- tryCatch(results_rsd_values(), error = function(e) numeric(0))
        if (length(status) > 0) {
          failed <- names(status[status == "fail"])
          if (length(failed) > 0) {
            export_df <- data.frame(
              Metabolite = failed,
              `RSD (%)` = round(as.numeric(rsds[failed]), 2),
              stringsAsFactors = FALSE, check.names = FALSE
            )
          }
        }
      } else if (view == "failed_samples" && !is.null(df)) {
        mets <- tryCatch(results_metabolite_cols(), error = function(e) character(0))
        if (length(mets) > 0) {
          miss_pct <- rowSums(is.na(df[, mets, drop = FALSE])) / length(mets) * 100
          sample_ids <- if ("sample_name" %in% names(df)) df$sample_name else seq_len(nrow(df))
          export_df <- data.frame(
            Sample = sample_ids,
            `Missing (%)` = round(miss_pct, 1),
            stringsAsFactors = FALSE, check.names = FALSE
          )
          export_df <- export_df[export_df$`Missing (%)` > 0, , drop = FALSE]
        }
      } else if (view == "missing_values" && !is.null(df)) {
        mets <- tryCatch(results_filtered_metabolites(), error = function(e) character(0))
        if (length(mets) > 0) {
          miss_count <- vapply(mets, function(m) sum(is.na(df[[m]])), integer(1))
          export_df <- data.frame(
            Metabolite = mets,
            `Missing Count` = miss_count,
            `Missing (%)` = round(miss_count / nrow(df) * 100, 1),
            stringsAsFactors = FALSE, check.names = FALSE
          )
          export_df <- export_df[export_df$`Missing Count` > 0, , drop = FALSE]
        }
      } else if (view == "sample_quality" && !is.null(df)) {
        mets <- tryCatch(results_metabolite_cols(), error = function(e) character(0))
        if (length(mets) > 0) {
          mat <- as.matrix(df[, mets, drop = FALSE])
          sample_ids <- if ("sample_name" %in% names(df)) df$sample_name else seq_len(nrow(df))
          sample_types <- if ("sample_type_factor" %in% names(df)) as.character(df$sample_type_factor)
                          else if ("sample_type" %in% names(df)) as.character(df$sample_type)
                          else rep("unknown", nrow(df))
          export_df <- data.frame(
            Sample = sample_ids,
            `Sample Type` = sample_types,
            Median = round(apply(mat, 1, stats::median, na.rm = TRUE), 2),
            Mean = round(rowMeans(mat, na.rm = TRUE), 2),
            `CV (%)` = round(apply(mat, 1, function(r) stats::sd(r, na.rm = TRUE) / mean(r, na.rm = TRUE) * 100), 1),
            `Missing Count` = rowSums(is.na(mat)),
            `Total Metabolites` = ncol(mat),
            stringsAsFactors = FALSE, check.names = FALSE
          )
        }
      }

      if (is.null(export_df) || nrow(export_df) == 0) {
        export_df <- data.frame(Message = "No data available for this view.")
      }
      readr::write_csv(export_df, file)
    }
  )

  # -- QC Downloads ------------------------------------------------------------

  output$qc_download_excel <- shiny::downloadHandler(
    filename = function() {
      paste0("qcCheckR_report_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      shiny::req(rv$qc_result)
      tryCatch({
        openxlsx::write.xlsx(corrected_data(), file)
      }, error = function(e) {
        # Write a minimal valid xlsx with the error message
        wb <- openxlsx::createWorkbook()
        openxlsx::addWorksheet(wb, "Error")
        openxlsx::writeData(wb, "Error", data.frame(Error = e$message))
        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      })
    }
  )

  output$qc_download_report <- shiny::downloadHandler(
    filename = function() {
      paste0("qcCheckR_report_", Sys.Date(), ".html")
    },
    content = function(file) {
      shiny::req(rv$qc_result)
      tryCatch({
        n_metabs <- length(results_metabolite_cols())
        n_samples <- nrow(corrected_data())
        html_content <- paste0(
          "<!DOCTYPE html><html lang='en'><head><meta charset='utf-8'>",
          "<title>qcCheckR Report</title>",
          "<style>body{font-family:sans-serif;max-width:800px;margin:2rem auto;padding:0 1rem}</style>",
          "</head><body>",
          "<h1>qcCheckR Report</h1>",
          "<p><strong>Date:</strong> ", htmltools::htmlEscape(as.character(Sys.Date())), "</p>",
          "<p><strong>Samples:</strong> ", htmltools::htmlEscape(as.character(n_samples)), "</p>",
          "<p><strong>Metabolites:</strong> ", htmltools::htmlEscape(as.character(n_metabs)), "</p>",
          "<p>Generated by MStargetR GUI.</p>",
          "</body></html>"
        )
        writeLines(html_content, file)
      }, error = function(e) {
        writeLines(paste0("<!DOCTYPE html><html><body><h1>Export Error</h1><p>",
                          htmltools::htmlEscape(e$message),
                          "</p></body></html>"), file)
      })
    }
  )
}
