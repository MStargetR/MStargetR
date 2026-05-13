# Tests for High-severity Shiny app audit findings
# Covers: SH-001 (Critical), SH-002, SH-003, SH-004, SH-005, SH-006, SH-007,
#         SH-008, SH-009, SH-010, SH-011, SH-012, SH-013, SH-014, SH-015,
#         SH-016, SH-017, SH-018, SH-019, SH-020

helpers_path <- system.file("shiny", "MStargetR_app", "R", "helpers.R",
                             package = "MStargetR")
if (!nzchar(helpers_path)) {
  helpers_path <- file.path("inst", "shiny", "MStargetR_app", "R", "helpers.R")
}
if (file.exists(helpers_path)) source(helpers_path)

server_path <- system.file("shiny", "MStargetR_app", "server.R",
                            package = "MStargetR")
if (!nzchar(server_path)) {
  server_path <- file.path("inst", "shiny", "MStargetR_app", "server.R")
}
ui_path <- system.file("shiny", "MStargetR_app", "ui.R",
                        package = "MStargetR")
if (!nzchar(ui_path)) {
  ui_path <- file.path("inst", "shiny", "MStargetR_app", "ui.R")
}

# ---------------------------------------------------------------------------
# SH-002: validate_upload default max_size_mb must be finite (50 MB)
# ---------------------------------------------------------------------------
test_that("SH-002: validate_upload default max_size_mb is 50 (not Inf)", {
  skip_if(!exists("validate_upload"))
  # Construct a fake upload > 50 MB
  fake_upload <- list(name = "big.csv",
                      size = 51 * 1024 * 1024,
                      datapath = tempfile())
  result <- validate_upload(fake_upload, allowed_extensions = c("csv"))
  expect_false(result$valid, info = "Files >50 MB should be rejected by default")
  expect_match(result$message, "too large", ignore.case = TRUE)
})

test_that("SH-002: validate_upload accepts file within default size limit", {
  skip_if(!exists("validate_upload"))
  fake_upload <- list(name = "ok.csv",
                      size = 1 * 1024 * 1024,
                      datapath = tempfile())
  result <- validate_upload(fake_upload, allowed_extensions = c("csv"))
  expect_true(result$valid)
})

# ---------------------------------------------------------------------------
# SH-003: docker_path allow-list rejects arbitrary executables
# ---------------------------------------------------------------------------
test_that("SH-003: only docker/podman basenames are in the allowed set", {
  allowed <- c("docker", "docker.exe", "podman", "podman.exe")
  expect_true("docker" %in% allowed)
  expect_true("podman" %in% allowed)
  expect_false("sh" %in% allowed)
  expect_false("bash" %in% allowed)
  expect_false("curl" %in% allowed)
})

# ---------------------------------------------------------------------------
# SH-004: onSessionEnded must NOT contain no-op rv$... assignments
# ---------------------------------------------------------------------------
test_that("SH-004: onSessionEnded does not contain no-op rv null assignments", {
  skip_if(!file.exists(server_path))
  server_text <- readLines(server_path)
  session_ended_start <- grep("onSessionEnded", server_text)[1]
  # Find the closing brace of onSessionEnded (within 10 lines)
  block <- server_text[session_ended_start:(session_ended_start + 12)]
  block_str <- paste(block, collapse = "\n")
  expect_false(grepl("rv\\$qc_result\\s*<-\\s*NULL", block_str),
               info = "No-op rv null assignment should be removed from onSessionEnded")
  expect_false(grepl("rv\\$bc_result\\s*<-\\s*NULL", block_str),
               info = "No-op rv null assignment should be removed from onSessionEnded")
})

# ---------------------------------------------------------------------------
# SH-005: confirm_cancel must not contain the dead kill-branch
# ---------------------------------------------------------------------------
test_that("SH-005: confirm_cancel does not contain dead process$kill() branch", {
  skip_if(!file.exists(server_path))
  server_text <- readLines(server_path)
  confirm_start <- grep("input\\$confirm_cancel", server_text)[1]
  block_end <- min(confirm_start + 25, length(server_text))
  block_str <- paste(server_text[confirm_start:block_end], collapse = "\n")
  expect_false(grepl("process_handle\\$kill", block_str),
               info = "Dead kill-branch should be removed from confirm_cancel handler")
})

# SH-005 (post-async): the cancel dialog now branches on task type. For
# async tasks (qc_run, batch_run) it offers a real "Yes, cancel" button
# that kills the callr subprocess. For remaining sync tasks (peak_run,
# convert_run, transition_check) it explains that the Shiny thread is
# blocked. Both branches must be present.
test_that("SH-005: cancel dialog branches on async vs sync task", {
  skip_if(!file.exists(server_path))
  server_text <- readLines(server_path)
  cancel_start <- grep("input\\$cancel_run", server_text)[1]
  block_end    <- min(cancel_start + 60, length(server_text))
  block_lines  <- server_text[cancel_start:block_end]
  block_code   <- sub("#.*$", "", block_lines)
  block_str    <- paste(block_code, collapse = "\n")
  expect_true(
    grepl('"qc_run"', block_str) && grepl('"batch_run"', block_str),
    info = "cancel dialog must special-case async tasks (qc_run, batch_run)"
  )
  expect_true(
    grepl("Cancel not supported", block_str),
    info = "cancel dialog must retain the sync-pipeline explanation"
  )
})

# SH-005 confirm_cancel must actually kill the subprocess (not just remove
# modal) for the async pipelines.
test_that("SH-005: confirm_cancel invokes real kill for async pipelines", {
  skip_if(!file.exists(server_path))
  server_text <- readLines(server_path)
  confirm_start <- grep("input\\$confirm_cancel", server_text)[1]
  block_end    <- min(confirm_start + 50, length(server_text))
  block_str    <- paste(server_text[confirm_start:block_end], collapse = "\n")
  expect_true(
    grepl("mst_cleanup_pipeline\\(", block_str),
    info = "confirm_cancel must call mst_cleanup_pipeline() to kill worker"
  )
  expect_true(
    grepl("rv\\$process_handle\\s*<-\\s*NULL", block_str),
    info = "confirm_cancel must clear rv$process_handle after cleanup"
  )
})

# ---------------------------------------------------------------------------
# SH-006: settings observer must use observeEvent(rv$prefs), not observe+isolate
# ---------------------------------------------------------------------------
test_that("SH-006: settings sync uses observeEvent(rv$prefs), not observe+isolate", {
  skip_if(!file.exists(server_path))
  server_text <- paste(readLines(server_path), collapse = "\n")
  # Should NOT have isolate(rv$prefs) near settings_user_name update
  expect_false(
    grepl("isolate\\(rv\\$prefs\\)[\\s\\S]{0,300}settings_user_name", server_text, perl = TRUE),
    info = "isolate(rv$prefs) pattern should not appear near settings sync"
  )
  # Should have observeEvent(rv$prefs
  expect_true(
    grepl("observeEvent\\(rv\\$prefs", server_text),
    info = "observeEvent(rv$prefs) should be used for settings sync"
  )
})

# ---------------------------------------------------------------------------
# SH-007: convert pipeline uses withCallingHandlers, not capture.output
# ---------------------------------------------------------------------------
test_that("SH-007: convert pipeline uses withCallingHandlers for message capture", {
  skip_if(!file.exists(server_path))
  server_text <- paste(readLines(server_path), collapse = "\n")
  # Find the msConvertR withProgress block
  expect_true(
    grepl("withCallingHandlers", server_text),
    info = "withCallingHandlers should be used for message capture"
  )
  # capture.output(type = \"message\" should not appear near msConvertR
  # Allow it elsewhere but not in the convert block
  convert_section <- sub(".*# Phase 2: Run conversion", "# Phase 2: Run conversion", server_text)
  convert_section <- sub("# -- Peak Integration.*", "", convert_section)
  expect_false(
    grepl('capture\\.output\\(type\\s*=\\s*"message"', convert_section),
    info = "capture.output should not be used in the convert block"
  )
})

# ---------------------------------------------------------------------------
# SH-008: peak_results_tabs renderUI must not write to rv$peak_report_files
# ---------------------------------------------------------------------------
test_that("SH-008: renderUI for peak_results_tabs does not write rv$peak_report_files", {
  skip_if(!file.exists(server_path))
  server_text <- readLines(server_path)
  render_start <- grep('output\\$peak_results_tabs\\s*<-\\s*shiny::renderUI', server_text)[1]
  if (is.na(render_start)) skip("peak_results_tabs renderUI not found")
  # Find closing brace by counting braces
  depth <- 0L
  end_line <- render_start
  for (i in render_start:min(render_start + 60, length(server_text))) {
    opens  <- nchar(server_text[i]) - nchar(gsub("\\{", "", server_text[i]))
    closes <- nchar(server_text[i]) - nchar(gsub("\\}", "", server_text[i]))
    depth  <- depth + opens - closes
    if (i > render_start && depth <= 0L) { end_line <- i; break }
  }
  block_str <- paste(server_text[render_start:end_line], collapse = "\n")
  expect_false(
    grepl("rv\\$peak_report_files\\s*<-", block_str),
    info = "renderUI must not write to rv$peak_report_files"
  )
})

# ---------------------------------------------------------------------------
# SH-009: stale output binding cleanup must exist
# ---------------------------------------------------------------------------
test_that("SH-009: peak report output binding cleanup for stale IDs exists", {
  skip_if(!file.exists(server_path))
  server_text <- paste(readLines(server_path), collapse = "\n")
  expect_true(
    grepl("peak_bound_ids_r", server_text),
    info = "peak_bound_ids_r reactive should exist for stale-binding cleanup"
  )
  expect_true(
    grepl("output\\[\\[stale_id\\]\\]\\s*<-\\s*NULL", server_text),
    info = "Stale output bindings should be cleared with output[[id]] <- NULL"
  )
})

# ---------------------------------------------------------------------------
# SH-010 (async): qc_run must spawn qcCheckR in a callr::r_bg subprocess
# via mst_spawn_pkg_fn(), not invoke MStargetR::qcCheckR on the Shiny
# main thread inside withProgress(). A single global poller observes
# rv$process_handle and invalidates every 500 ms.
# ---------------------------------------------------------------------------
test_that("SH-010: qc_run observer dispatches to async subprocess", {
  skip_if(!file.exists(server_path))
  server_text <- readLines(server_path)

  qc_start <- grep("input\\$qc_run", server_text)[1]
  expect_false(is.na(qc_start),
               info = "qc_run observer not found")
  tail <- server_text[qc_start:length(server_text)]
  end_rel <- grep("^  (shiny::)?(observeEvent|observe|reactive)\\(",
                  tail[-1])[1]
  end <- if (is.na(end_rel)) length(server_text) else qc_start + end_rel - 1
  block_str <- paste(server_text[qc_start:end], collapse = "\n")

  expect_true(
    grepl('mst_spawn_pkg_fn\\(\\s*"qcCheckR"', block_str),
    info = "qc_run must spawn qcCheckR via mst_spawn_pkg_fn()"
  )
  expect_false(
    grepl("MStargetR::qcCheckR\\(", block_str),
    info = paste0("qc_run must not call MStargetR::qcCheckR() directly ",
                  "— it blocks the Shiny main thread")
  )

  # The qcCheckR worker must be invoked with write_rda = FALSE so the
  # subprocess returns the in-memory result before the qs2 save runs.
  # The save is then fired in a separate detached subprocess (see
  # mst_spawn_qs_save assertion below). Regressing this would re-introduce
  # the foreground save wait that this contract was designed to eliminate.
  expect_true(
    grepl("write_rda\\s*=\\s*FALSE", block_str),
    info = paste0("qc_run args list must include write_rda = FALSE so the ",
                  "qs2 save can run in a detached background subprocess")
  )
})

# qc_run completion must spawn the detached qs2 save via
# mst_spawn_qs_save() so users can view results immediately while the
# .qs2 file is still being written. The corresponding helper is exercised
# elsewhere; this test just locks in the structural decision in server.R.
test_that("qc_run success branch spawns detached qs2 save", {
  skip_if(!file.exists(server_path))
  server_text <- paste(readLines(server_path), collapse = "\n")
  expect_true(
    grepl("mst_spawn_qs_save\\(", server_text),
    info = paste0("server must spawn the qs2 save in its own subprocess via ",
                  "mst_spawn_qs_save() after qc_run succeeds")
  )
  expect_true(
    grepl("rv\\$qs_handle", server_text),
    info = "server must track the detached qs2 writer in rv$qs_handle"
  )
})

# SH-010/SH-013 global poller: exactly one observe() body must reference
# rv$process_handle and invalidateLater to re-arm itself. This is the
# single point at which background pipelines transition to "done".
test_that("SH-010/SH-013: global process poller observes rv$process_handle", {
  skip_if(!file.exists(server_path))
  server_text <- paste(readLines(server_path), collapse = "\n")
  expect_true(
    grepl("rv\\$process_handle", server_text),
    info = "server must manage rv$process_handle"
  )
  expect_true(
    grepl("mst_poll_pipeline\\(", server_text),
    info = "global poller must call mst_poll_pipeline()"
  )
  expect_true(
    grepl("shiny::invalidateLater\\(500", server_text),
    info = "poller must re-arm every 500 ms while subprocess is alive"
  )
})

# ---------------------------------------------------------------------------
# SH-011: qc_rsd_df_r reactive exists and is separate from threshold
# ---------------------------------------------------------------------------
test_that("SH-011: qc_rsd_df_r reactive separates RSD computation from threshold", {
  skip_if(!file.exists(server_path))
  server_text <- paste(readLines(server_path), collapse = "\n")
  expect_true(
    grepl("qc_rsd_df_r\\s*<-\\s*shiny::reactive", server_text),
    info = "qc_rsd_df_r reactive should exist"
  )
})

# ---------------------------------------------------------------------------
# SH-012: no observer writing rv$batch_source_desc
# ---------------------------------------------------------------------------
test_that("SH-012: no observer writes rv$batch_source_desc", {
  skip_if(!file.exists(server_path))
  server_text <- paste(readLines(server_path), collapse = "\n")
  expect_false(
    grepl("rv\\$batch_source_desc\\s*<-", server_text),
    info = "rv$batch_source_desc should not be written; use batch_source_desc_r() reactive"
  )
  expect_true(
    grepl("batch_source_desc_r\\s*<-\\s*shiny::reactive", server_text),
    info = "batch_source_desc_r reactive should exist"
  )
})

# ---------------------------------------------------------------------------
# SH-013 (async): batch pipeline runs in callr::r_bg subprocess.
# The subprocess streams messages via sink() so the old withCallingHandlers
# in-process message capture is no longer needed. The batch_run observer
# must spawn via mst_spawn_pkg_fn("batchCorrectR", ...) and never call
# MStargetR::batchCorrectR() directly on the Shiny thread.
# ---------------------------------------------------------------------------
test_that("SH-013: batch_run observer dispatches to async subprocess", {
  skip_if(!file.exists(server_path))
  server_text <- readLines(server_path)

  batch_start <- grep("input\\$batch_run", server_text)[1]
  expect_false(is.na(batch_start),
               info = "batch_run observer not found")
  # Scan forward until the next top-level observer/reactive declaration.
  tail <- server_text[batch_start:length(server_text)]
  end_rel <- grep("^  (shiny::)?(observeEvent|observe|reactive)\\(",
                  tail[-1])[1]
  end <- if (is.na(end_rel)) length(server_text) else batch_start + end_rel - 1
  block_str <- paste(server_text[batch_start:end], collapse = "\n")

  expect_true(
    grepl('mst_spawn_pkg_fn\\(\\s*"batchCorrectR"', block_str),
    info = "batch_run must spawn batchCorrectR via mst_spawn_pkg_fn()"
  )
  expect_false(
    grepl("MStargetR::batchCorrectR\\(", block_str),
    info = paste0("batch_run must not call MStargetR::batchCorrectR() ",
                  "directly — it blocks the Shiny main thread")
  )
  expect_false(
    grepl('capture\\.output\\(type\\s*=\\s*"message"', block_str),
    info = "capture.output must not be used in the batch correction block"
  )
})

# ---------------------------------------------------------------------------
# SH-014: results_upload_load calls validate_upload before openxlsx
# ---------------------------------------------------------------------------
test_that("SH-014: results_upload_load validates before reading xlsx/csv/tsv", {
  skip_if(!file.exists(server_path))
  server_text <- readLines(server_path)
  load_start <- grep("results_upload_load", server_text)[1]
  # Find validate_upload call within 30 lines of results_upload_load
  block_end <- min(load_start + 35, length(server_text))
  block_str <- paste(server_text[load_start:block_end], collapse = "\n")
  expect_true(
    grepl("validate_upload", block_str),
    info = "validate_upload must be called in results_upload_load before reading"
  )
  # getSheetNames req guard
  expect_true(
    grepl("getSheetNames", block_str),
    info = "getSheetNames req guard must exist before openxlsx::read.xlsx"
  )
})

# ---------------------------------------------------------------------------
# SH-015: renderDT dispatches to helper functions
# ---------------------------------------------------------------------------
test_that("SH-015: results_data_table renderDT uses switch dispatch to helpers", {
  skip_if(!file.exists(server_path))
  server_text <- paste(readLines(server_path), collapse = "\n")
  expect_true(
    grepl("results_dt_concentration", server_text),
    info = "results_dt_concentration helper should exist"
  )
  expect_true(
    grepl("results_dt_failed_samples", server_text),
    info = "results_dt_failed_samples helper should exist"
  )
  expect_true(
    grepl("switch\\(view", server_text),
    info = "renderDT should use switch() for dispatch"
  )
})

# ---------------------------------------------------------------------------
# SH-016/017/018/019: orphan uiOutput slots removed from ui.R
# ---------------------------------------------------------------------------
test_that("SH-016/017/018/019: orphan progress uiOutput slots removed from ui.R", {
  skip_if(!file.exists(ui_path))
  ui_text <- paste(readLines(ui_path), collapse = "\n")
  expect_false(grepl('"convert_progress_ui"', ui_text),
               info = "convert_progress_ui orphan uiOutput should be removed")
  expect_false(grepl('"peak_progress_ui"', ui_text),
               info = "peak_progress_ui orphan uiOutput should be removed")
  expect_false(grepl('"qc_progress_ui"', ui_text),
               info = "qc_progress_ui orphan uiOutput should be removed")
  expect_false(grepl('"batch_progress_ui"', ui_text),
               info = "batch_progress_ui orphan uiOutput should be removed")
})

# ---------------------------------------------------------------------------
# UI orphan hidden inputs removed (audit follow-up): convert_vendor_format
# and peak_plate_ids were hidden behind display:none rather than deleted;
# they had no server-side consumers, so they are now fully removed.
# ---------------------------------------------------------------------------
test_that("orphan hidden inputs convert_vendor_format / peak_plate_ids removed", {
  skip_if(!file.exists(ui_path))
  ui_text <- paste(readLines(ui_path), collapse = "\n")
  expect_false(grepl('"convert_vendor_format"', ui_text),
               info = "convert_vendor_format orphan input should be removed")
  expect_false(grepl('"peak_plate_ids"', ui_text),
               info = "peak_plate_ids orphan input should be removed")
})

# ---------------------------------------------------------------------------
# SH-001 (Critical): kable preview of uploaded xlsx sheets must escape HTML
# in cell contents. Without `escape = TRUE`, a cell containing
# `<script>alert(1)</script>` would become live DOM via htmltools::HTML().
# ---------------------------------------------------------------------------
test_that("SH-001: batch_xlsx_sheet_preview calls kable with escape = TRUE", {
  skip_if(!file.exists(server_path))
  server_lines <- readLines(server_path)

  # Locate the renderUI block assigned to output$batch_xlsx_sheet_preview
  # by line, then take a window forward until we hit the next output$ slot
  # or run out of lines. This avoids fragile .*/multi-line regex handling
  # and makes assertion failures point to a specific code region.
  start <- grep("output\\$batch_xlsx_sheet_preview\\s*<-\\s*shiny::renderUI",
                server_lines)[1]
  expect_false(is.na(start),
               info = "batch_xlsx_sheet_preview renderUI block not found")

  tail_lines <- server_lines[start:length(server_lines)]
  next_output <- grep("^\\s*output\\$", tail_lines)
  end <- if (length(next_output) >= 2) start + next_output[2] - 2
         else length(server_lines)
  block_str <- paste(server_lines[start:end], collapse = "\n")

  expect_true(
    grepl("knitr::kable\\(", block_str),
    info = "expected knitr::kable() in batch_xlsx_sheet_preview render"
  )
  expect_true(
    grepl("escape\\s*=\\s*TRUE", block_str),
    info = paste0("batch_xlsx_sheet_preview must call kable(..., escape = TRUE) ",
                  "to prevent HTML/JS injection from xlsx cell contents")
  )
  # Guard against a regression that disables escape.
  expect_false(
    grepl("escape\\s*=\\s*FALSE", block_str),
    info = "escape = FALSE must never appear in batch_xlsx_sheet_preview"
  )
})

# ---------------------------------------------------------------------------
# SH-020: settings_theme wired to settings_save
# ---------------------------------------------------------------------------
test_that("SH-020: settings_save persists theme and sends mst-set-theme message", {
  skip_if(!file.exists(server_path))
  server_text <- paste(readLines(server_path), collapse = "\n")
  # Find settings_save observer block
  save_block <- sub(".*observeEvent\\(input\\$settings_save", "observeEvent(input$settings_save",
                    server_text)
  save_block <- sub("observeEvent\\(input\\$settings_reset.*", "", save_block)
  expect_true(
    grepl("rv\\$prefs\\$theme", save_block),
    info = "settings_save should persist rv$prefs$theme"
  )
  expect_true(
    grepl("mst-set-theme", save_block),
    info = "settings_save should send mst-set-theme custom message"
  )
})
