# Tests for use_workflow.R ----
library(mockery)

# ============================================================================
# NULL workflow: list available workflows
# ============================================================================

test_that("use_workflow with NULL lists available workflows", {
  expect_message(
    result <- use_workflow(workflow = NULL),
    "Available MStargetR workflow templates"
  )
  expect_type(result, "character")
  expect_true("generic" %in% result)
  expect_true("CCSM" %in% result)
})

test_that("use_workflow with NULL prints each workflow description", {
  expect_message(
    use_workflow(workflow = NULL),
    "generic"
  )
  expect_message(
    use_workflow(workflow = NULL),
    "CCSM"
  )
})

test_that("use_workflow with NULL returns names invisibly", {
  out <- withVisible(suppressMessages(use_workflow(workflow = NULL)))
  expect_false(out$visible)
})

# ============================================================================
# Input validation
# ============================================================================

test_that("use_workflow rejects non-character workflow", {
  expect_error(
    use_workflow(workflow = 123),
    "'workflow' must be a single character string"
  )
})

test_that("use_workflow rejects multi-element workflow", {
  expect_error(
    use_workflow(workflow = c("generic", "CCSM")),
    "'workflow' must be a single character string"
  )
})

test_that("use_workflow rejects invalid workflow name", {
  expect_error(
    use_workflow(workflow = "nonexistent"),
    "must be one of"
  )
})

test_that("use_workflow errors when workflow file not found in package", {
  stub(use_workflow, "system.file", function(...) "")

  expect_error(
    use_workflow(workflow = "generic", output_dir = tempdir()),
    "Workflow file not found"
  )
})

test_that("use_workflow errors when output_dir does not exist", {
  stub(use_workflow, "system.file", function(...) "/fake/src.Rmd")

  fake_dir <- file.path(tempdir(), "nonexistent_use_workflow_test_dir")
  expect_error(
    use_workflow(workflow = "generic", output_dir = fake_dir),
    "Output directory does not exist"
  )
})

test_that("use_workflow errors when output_dir is not writable", {
  tmp <- withr::local_tempdir()
  stub(use_workflow, "system.file", function(...) file.path(tmp, "src.Rmd"))
  stub(use_workflow, "file.access", function(path, mode) -1L)

  expect_error(
    use_workflow(workflow = "generic", output_dir = tmp),
    "not writable"
  )
})

# ============================================================================
# Successful copy
# ============================================================================

test_that("use_workflow copies file to output_dir", {
  tmp <- withr::local_tempdir()
  src_file <- file.path(tmp, "src.Rmd")
  writeLines("---\ntitle: test\n---", src_file)

  stub(use_workflow, "system.file", function(...) src_file)

  expect_message(
    result <- use_workflow(workflow = "generic", output_dir = tmp, open = FALSE),
    "Workflow template copied to"
  )
  expect_true(file.exists(result))
})

test_that("use_workflow returns dest path invisibly", {
  tmp <- withr::local_tempdir()
  src_file <- file.path(tmp, "src.Rmd")
  writeLines("test", src_file)

  stub(use_workflow, "system.file", function(...) src_file)

  out <- withVisible(suppressMessages(
    use_workflow(workflow = "generic", output_dir = tmp, open = FALSE)
  ))
  expect_false(out$visible)
  expect_type(out$value, "character")
})

test_that("use_workflow refuses to overwrite existing file by default", {
  tmp <- withr::local_tempdir()
  src_file <- file.path(tmp, "src.Rmd")
  writeLines("new source", src_file)

  # Pre-create dest file with content the user could have edited.
  dest_file <- file.path(tmp, "workflow_generic.Rmd")
  writeLines("user edits here", dest_file)

  stub(use_workflow, "system.file", function(...) src_file)

  expect_error(
    use_workflow(workflow = "generic", output_dir = tmp, open = FALSE),
    "File already exists"
  )
  # User edits must survive the refused call.
  expect_equal(readLines(dest_file), "user edits here")
})

test_that("use_workflow overwrites when overwrite = TRUE", {
  tmp <- withr::local_tempdir()
  src_file <- file.path(tmp, "src.Rmd")
  writeLines("new source", src_file)

  dest_file <- file.path(tmp, "workflow_generic.Rmd")
  writeLines("user edits here", dest_file)

  stub(use_workflow, "system.file", function(...) src_file)

  expect_message(
    use_workflow(workflow = "generic", output_dir = tmp, open = FALSE,
                 overwrite = TRUE),
    "Workflow template copied to"
  )
  expect_equal(readLines(dest_file), "new source")
})

test_that("use_workflow handles case-insensitive workflow names", {
  tmp <- withr::local_tempdir()
  src_file <- file.path(tmp, "src.Rmd")
  writeLines("test", src_file)

  stub(use_workflow, "system.file", function(...) src_file)

  expect_message(
    use_workflow(workflow = "GENERIC", output_dir = tmp, open = FALSE),
    "Workflow template copied to"
  )
})

test_that("use_workflow matches CCSM workflow", {
  tmp <- withr::local_tempdir()
  src_file <- file.path(tmp, "src.Rmd")
  writeLines("test", src_file)

  stub(use_workflow, "system.file", function(...) src_file)

  expect_message(
    use_workflow(workflow = "ccsm", output_dir = tmp, open = FALSE),
    "Workflow template copied to"
  )
})

# ============================================================================
# RStudio integration (open parameter)
# ============================================================================

test_that("use_workflow with open=TRUE and RStudio available calls navigateToFile", {
  tmp <- withr::local_tempdir()
  src_file <- file.path(tmp, "src.Rmd")
  writeLines("test", src_file)

  stub(use_workflow, "system.file", function(...) src_file)

  nav_called <- FALSE
  stub(use_workflow, "requireNamespace", function(pkg, ...) {
    if (pkg == "rstudioapi") return(TRUE)
    return(TRUE)
  })
  stub(use_workflow, "rstudioapi::isAvailable", function() TRUE)
  stub(use_workflow, "rstudioapi::navigateToFile", function(f) {
    nav_called <<- TRUE
  })

  suppressMessages(
    use_workflow(workflow = "generic", output_dir = tmp, open = TRUE)
  )
  expect_true(nav_called)
})

test_that("use_workflow with open=TRUE but RStudio not available skips navigation", {
  tmp <- withr::local_tempdir()
  src_file <- file.path(tmp, "src.Rmd")
  writeLines("test", src_file)

  stub(use_workflow, "system.file", function(...) src_file)
  stub(use_workflow, "requireNamespace", function(pkg, ...) {
    if (pkg == "rstudioapi") return(FALSE)
    return(TRUE)
  })

  nav_called <- FALSE
  # Should not reach navigateToFile since requireNamespace returns FALSE
  suppressMessages(
    use_workflow(workflow = "generic", output_dir = tmp, open = TRUE)
  )
  expect_false(nav_called)
})

test_that("use_workflow with open=FALSE does not call navigateToFile", {
  tmp <- withr::local_tempdir()
  src_file <- file.path(tmp, "src.Rmd")
  writeLines("test", src_file)

  stub(use_workflow, "system.file", function(...) src_file)

  nav_called <- FALSE
  stub(use_workflow, "rstudioapi::navigateToFile", function(f) {
    nav_called <<- TRUE
  })

  suppressMessages(
    use_workflow(workflow = "generic", output_dir = tmp, open = FALSE)
  )
  expect_false(nav_called)
})

test_that("use_workflow prints edit instructions after copy", {
  tmp <- withr::local_tempdir()
  src_file <- file.path(tmp, "src.Rmd")
  writeLines("test", src_file)

  stub(use_workflow, "system.file", function(...) src_file)

  expect_message(
    use_workflow(workflow = "generic", output_dir = tmp, open = FALSE),
    "Edit the file to set your project path"
  )
})
