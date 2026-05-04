# Quick test of premium Shiny app
cat("=== MStargetR Premium GUI Test ===\n")

appDir <- file.path(
  normalizePath("../..", winslash = "/"),
  "inst", "shiny", "MStargetR_app"
)
cat("App:", appDir, "\n")
cat("Exists:", dir.exists(appDir), "\n\n")

cat("Parsing ui.R... ")
tryCatch({ parse(file = file.path(appDir, "ui.R")); cat("OK\n") },
  error = function(e) cat("ERROR:", e$message, "\n"))

cat("Parsing server.R... ")
tryCatch({ parse(file = file.path(appDir, "server.R")); cat("OK\n") },
  error = function(e) cat("ERROR:", e$message, "\n"))

cat("Parsing helpers.R... ")
tryCatch({ parse(file = file.path(appDir, "R", "helpers.R")); cat("OK\n") },
  error = function(e) cat("ERROR:", e$message, "\n"))

cat("\nCSS lines:", length(readLines(file.path(appDir, "www", "custom.css"))), "\n")
cat("JS lines:", length(readLines(file.path(appDir, "www", "app.js"))), "\n")

cat("\nLoading libraries... ")
library(shiny); library(bslib); library(DT); library(htmltools)
cat("OK\n")

cat("Sourcing helpers... ")
source(file.path(appDir, "R", "helpers.R"))
cat("OK\n")

cat("Evaluating ui.R... ")
tryCatch({
  ui <- source(file.path(appDir, "ui.R"), local = TRUE)$value
  cat("OK (", class(ui)[1], ")\n")
}, error = function(e) cat("ERROR:", e$message, "\n"))

cat("\nStarting on port 7778...\n")
tryCatch({
  shiny::runApp(appDir, port = 7778, host = "127.0.0.1",
                launch.browser = FALSE, quiet = FALSE)
}, error = function(e) cat("LAUNCH ERROR:", e$message, "\n"))
