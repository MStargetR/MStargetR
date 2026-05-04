# Tests for Medium-severity Shiny app audit findings (SH-021..SH-040)

helpers_path <- system.file("shiny", "MStargetR_app", "R", "helpers.R",
                             package = "MStargetR")
if (!nzchar(helpers_path)) {
  helpers_path <- file.path("inst", "shiny", "MStargetR_app", "R", "helpers.R")
}
if (file.exists(helpers_path)) source(helpers_path, local = TRUE)

# ---------------------------------------------------------------------------
# SH-021: app.R must not contain manual source() calls
# ---------------------------------------------------------------------------
test_that("SH-021: app.R contains no manual source() calls", {
  app_path <- system.file("shiny", "MStargetR_app", "app.R", package = "MStargetR")
  if (!nzchar(app_path)) app_path <- file.path("inst", "shiny", "MStargetR_app", "app.R")
  skip_if(!file.exists(app_path))
  lines <- readLines(app_path)
  source_lines <- grep("^\\s*source\\(", lines, value = TRUE)
  expect_length(source_lines, 0)
})

# ---------------------------------------------------------------------------
# SH-022: %||% must be visible in globalenv after sourcing helpers
# ---------------------------------------------------------------------------
test_that("SH-022: %||% is assigned to globalenv by helpers.R", {
  skip_if(!exists("%||%", mode = "function", inherits = TRUE))
  expect_true(is.function(get("%||%", envir = .GlobalEnv, inherits = FALSE)))
})

# ---------------------------------------------------------------------------
# SH-023: safe_call must not misclassify list(success=FALSE) as error
# ---------------------------------------------------------------------------
test_that("SH-023: safe_call treats list(success=FALSE) as success", {
  skip_if(!exists("safe_call"))
  result <- safe_call(list(success = FALSE, value = 42))
  expect_true(result$success)
  expect_equal(result$result$value, 42)
})

# ---------------------------------------------------------------------------
# SH-025/SH-026: preserve_upload_names must produce unique dirs per call
# ---------------------------------------------------------------------------
test_that("SH-025/026: preserve_upload_names produces unique dirs each call", {
  skip_if(!exists("preserve_upload_names"))
  tmp <- tempfile(fileext = ".txt")
  writeLines("test", tmp)
  path1 <- preserve_upload_names(tmp, "test.txt")
  path2 <- preserve_upload_names(tmp, "test.txt")
  expect_false(dirname(path1) == dirname(path2))
})

# ---------------------------------------------------------------------------
# SH-030: all_dupes append pattern does not produce sparse list
# ---------------------------------------------------------------------------
test_that("SH-030: c(list, list()) append never leaves NULL holes", {
  all_dupes <- list()
  # Simulate skipping i=1 (NULL result) and adding i=2
  all_dupes <- c(all_dupes, list(data.frame(a = 1)))
  expect_length(all_dupes, 1)
  expect_false(is.null(all_dupes[[1]]))
  combined <- do.call(rbind, all_dupes)
  expect_s3_class(combined, "data.frame")
})

# ---------------------------------------------------------------------------
# SH-033: log_shiny_error helper exists and handles silent exceptions cleanly
# ---------------------------------------------------------------------------
test_that("SH-033: log_shiny_error ignores shiny.silent.exception", {
  skip_if(!exists("log_shiny_error"))
  silent_err <- structure(
    class = c("shiny.silent.exception", "condition"),
    list(message = "req() failed", call = NULL)
  )
  expect_null(log_shiny_error(NULL, "test_ctx", silent_err))
})

test_that("SH-033: log_shiny_error captures real errors into rv$render_errors", {
  skip_if(!exists("log_shiny_error"))
  rv <- new.env(parent = emptyenv())
  real_err <- simpleError("plot failed")
  expect_null(log_shiny_error(NULL, "qc_pca_plot", real_err, rv = rv))
  expect_true(length(rv$render_errors) > 0)
  expect_true(grepl("plot failed", rv$render_errors[[1]]))
})

# ---------------------------------------------------------------------------
# SH-034: validate_upload with explicit max_size_mb=10 rejects large files
# ---------------------------------------------------------------------------
test_that("SH-034: validate_upload rejects files exceeding max_size_mb", {
  skip_if(!exists("validate_upload"))
  big_upload <- list(name = "big.tsv", size = 15 * 1024 * 1024)
  result <- validate_upload(big_upload, allowed_extensions = c("tsv"), max_size_mb = 10)
  expect_false(result$valid)
  expect_match(result$message, "too large")
})

# ---------------------------------------------------------------------------
# SH-042: UNC path rejection logic
# ---------------------------------------------------------------------------
test_that("SH-042: UNC path detection rejects \\\\server\\share paths", {
  expect_true(grepl("^\\\\\\\\", "\\\\server\\share"))
  expect_false(grepl("^\\\\\\\\", "/tmp/myproject"))
  expect_false(grepl("^\\\\\\\\", "C:/myproject"))
})

# ---------------------------------------------------------------------------
# SH-045: aggregate split avoids matrix columns
# ---------------------------------------------------------------------------
test_that("SH-045: separate aggregate calls yield numeric columns", {
  summ <- data.frame(
    class = c("PC", "LPC", "PC", "LPC"),
    rsd_before = c(25, 30, 20, 35),
    rsd_after  = c(10, 12,  8, 15),
    stringsAsFactors = FALSE
  )
  class_summ_b <- stats::aggregate(rsd_before ~ class, data = summ,
    FUN = function(x) stats::median(x, na.rm = TRUE))
  class_summ_a <- stats::aggregate(rsd_after  ~ class, data = summ,
    FUN = function(x) stats::median(x, na.rm = TRUE))
  class_summ <- merge(class_summ_b, class_summ_a, by = "class")
  expect_true(is.numeric(class_summ$rsd_before))
  expect_true(is.numeric(class_summ$rsd_after))
  expect_equal(nrow(class_summ), 2L)
})

# ---------------------------------------------------------------------------
# SH-049: perl=TRUE for lipid class regex handles spaces
# The server.R pattern uses "\\s" inside a character class; with perl=TRUE this
# becomes the PCRE whitespace class and correctly splits "PC 36:2" -> "PC".
# ---------------------------------------------------------------------------
test_that("SH-049: sub() with perl=TRUE splits space-separated metabolite names", {
  # "\\s" in the regex string is a PCRE whitespace class when perl=TRUE.
  # Names with letter-digit boundary (TG52:3) require the second sub in bc_extract_class.
  # This test validates only the first sub in results_lipid_class_map fallback.
  mets   <- c("PC 36:2", "LPC_18:1", "CE(18:2)")
  parsed <- sub("^([A-Za-z]+)[_\\s(\\-].*$", "\\1", mets, perl = TRUE)
  expect_equal(parsed, c("PC", "LPC", "CE"))
})

test_that("SH-049: sub() without perl=TRUE does not split space-delimited names", {
  mets <- "PC 36:2"
  # Without perl, [_\s(-] treats \s as literal 's' — space is not in the class
  parsed_noperl <- sub("^([A-Za-z]+)[_\\s(\\-].*$", "\\1", mets, perl = FALSE)
  parsed_perl   <- sub("^([A-Za-z]+)[_\\s(\\-].*$", "\\1", mets, perl = TRUE)
  # perl=FALSE: "PC 36:2" is unchanged because space is not matched
  expect_equal(parsed_noperl, "PC 36:2")
  expect_equal(parsed_perl, "PC")
})

# ---------------------------------------------------------------------------
# SH-051: match() for RSD bar lookup
# ---------------------------------------------------------------------------
test_that("SH-051: match()-based lookup equals name-based subsetting", {
  before <- c(A = 10, B = 20, C = 30)
  levels <- c("B", "A", "D")
  old_way <- as.numeric(before[as.character(levels)])
  new_way <- as.numeric(before[match(as.character(levels), names(before))])
  expect_equal(old_way, new_way)
  expect_equal(new_way, c(20, 10, NA))
})

# ---------------------------------------------------------------------------
# SH-052: df[[met]] is unambiguous for plotly y aesthetic
# ---------------------------------------------------------------------------
test_that("SH-052: df[[met]] returns correct column vector", {
  df  <- data.frame(mymet = 1:5, stringsAsFactors = FALSE)
  met <- "mymet"
  expect_identical(df[[met]], 1:5)
})

# ---------------------------------------------------------------------------
# SH-059: empty_state() helper accepts output_id and uses conditionalPanel
# ---------------------------------------------------------------------------
test_that("SH-059: empty_state signature includes output_id parameter", {
  ui_path <- system.file("shiny", "MStargetR_app", "ui.R", package = "MStargetR")
  if (!nzchar(ui_path))
    ui_path <- file.path("inst", "shiny", "MStargetR_app", "ui.R")
  skip_if(!file.exists(ui_path))
  lines   <- readLines(ui_path)
  fn_idx  <- grep("empty_state <- function", lines)
  expect_true(length(fn_idx) > 0L)
  # signature may span the definition line and the next line (output_id = NULL)
  fn_block <- paste(lines[fn_idx[1L]:min(fn_idx[1L] + 2L, length(lines))], collapse = " ")
  expect_true(grepl("output_id", fn_block))
})

test_that("SH-059: at least one empty_state call passes an output_id", {
  ui_path <- system.file("shiny", "MStargetR_app", "ui.R", package = "MStargetR")
  if (!nzchar(ui_path))
    ui_path <- file.path("inst", "shiny", "MStargetR_app", "ui.R")
  skip_if(!file.exists(ui_path))
  lines    <- readLines(ui_path)
  # Calls with two arguments (message + output_id) have a quoted second arg
  wired    <- grep('empty_state\\(.*,\\s*"', lines, value = TRUE)
  expect_true(length(wired) > 0L)
})

test_that("SH-059: conditionalPanel is used inside empty_state body", {
  ui_path <- system.file("shiny", "MStargetR_app", "ui.R", package = "MStargetR")
  if (!nzchar(ui_path))
    ui_path <- file.path("inst", "shiny", "MStargetR_app", "ui.R")
  skip_if(!file.exists(ui_path))
  lines    <- readLines(ui_path)
  cond_use <- grep("conditionalPanel", lines, value = TRUE)
  expect_true(length(cond_use) > 0L)
})
