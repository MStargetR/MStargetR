# Tests for install_MStargetR ----
library(mockery)

# --- BiocManager availability checks ---

test_that("install_MStargetR installs BiocManager when not available", {
  install_packages_called_with <- NULL

  stub(install_MStargetR, "base::requireNamespace", function(pkg, ...) {
    if (pkg == "BiocManager") return(FALSE)
    return(TRUE)
  })
  stub(install_MStargetR, "install.packages", function(pkg, ...) {
    install_packages_called_with <<- c(install_packages_called_with, pkg)
  })
  stub(install_MStargetR, "BiocManager::install", function(...) NULL)

  suppressMessages(install_MStargetR())

  expect_true("BiocManager" %in% install_packages_called_with)
})

test_that("install_MStargetR does not install BiocManager when already available", {
  install_packages_called_with <- NULL

  stub(install_MStargetR, "base::requireNamespace", function(pkg, ...) TRUE)
  stub(install_MStargetR, "install.packages", function(pkg, ...) {
    install_packages_called_with <<- c(install_packages_called_with, pkg)
  })
  stub(install_MStargetR, "BiocManager::install", function(...) NULL)

  suppressMessages(install_MStargetR())

  expect_null(install_packages_called_with)
})

# --- BiocManager::install is called with correct arguments ---

test_that("install_MStargetR calls BiocManager::install with ask=FALSE and update=FALSE", {
  bioc_install_args <- NULL

  stub(install_MStargetR, "base::requireNamespace", function(pkg, ...) TRUE)
  stub(install_MStargetR, "install.packages", function(...) NULL)
  stub(install_MStargetR, "BiocManager::install", function(...) {
    bioc_install_args <<- list(...)
  })

  suppressMessages(install_MStargetR())

  expect_equal(bioc_install_args[[1]], "MStargetR/MStargetR")
  expect_false(bioc_install_args$ask)
  expect_false(bioc_install_args$update)
})

# --- Only one BiocManager::install call ---

test_that("install_MStargetR calls BiocManager::install exactly once", {
  bioc_install_count <- 0

  stub(install_MStargetR, "base::requireNamespace", function(pkg, ...) TRUE)
  stub(install_MStargetR, "install.packages", function(...) NULL)
  stub(install_MStargetR, "BiocManager::install", function(...) {
    bioc_install_count <<- bioc_install_count + 1
  })

  suppressMessages(install_MStargetR())

  expect_equal(bioc_install_count, 1)
})

# --- Pipeline order ---

test_that("install_MStargetR executes steps in correct order", {
  call_log <- character(0)

  stub(install_MStargetR, "base::requireNamespace", function(pkg, ...) {
    call_log <<- c(call_log, paste0("check_", pkg))
    TRUE
  })
  stub(install_MStargetR, "install.packages", function(pkg, ...) {
    call_log <<- c(call_log, paste0("install_", pkg))
  })
  stub(install_MStargetR, "BiocManager::install", function(...) {
    args <- list(...)
    call_log <<- c(call_log, paste0("bioc_install_", args[[1]]))
  })

  suppressMessages(install_MStargetR())

  expect_equal(call_log, c(
    "check_BiocManager",
    "bioc_install_MStargetR/MStargetR"
  ))
})

# --- BiocManager missing triggers install then BiocManager::install ---

test_that("install_MStargetR installs BiocManager then MStargetR when BiocManager missing", {
  call_log <- character(0)

  stub(install_MStargetR, "base::requireNamespace", function(pkg, ...) {
    call_log <<- c(call_log, paste0("check_", pkg))
    if (pkg == "BiocManager") return(FALSE)
    return(TRUE)
  })
  stub(install_MStargetR, "install.packages", function(pkg, ...) {
    call_log <<- c(call_log, paste0("install_", pkg))
  })
  stub(install_MStargetR, "BiocManager::install", function(...) {
    args <- list(...)
    call_log <<- c(call_log, paste0("bioc_install_", args[[1]]))
  })

  suppressMessages(install_MStargetR())

  expect_equal(call_log, c(
    "check_BiocManager",
    "install_BiocManager",
    "bioc_install_MStargetR/MStargetR"
  ))
})

# --- CRAN mirror is specified ---

test_that("install_MStargetR passes repos argument when installing BiocManager", {
  install_repos <- NULL

  stub(install_MStargetR, "base::requireNamespace", function(pkg, ...) FALSE)
  stub(install_MStargetR, "install.packages", function(pkg, repos = NULL, ...) {
    install_repos <<- repos
  })
  stub(install_MStargetR, "BiocManager::install", function(...) NULL)

  suppressMessages(install_MStargetR())

  expect_equal(install_repos, "https://cloud.r-project.org")
})

# --- Error handling ---

test_that("install_MStargetR propagates BiocManager::install errors", {
  stub(install_MStargetR, "base::requireNamespace", function(pkg, ...) TRUE)
  stub(install_MStargetR, "install.packages", function(...) NULL)
  stub(install_MStargetR, "BiocManager::install", function(...) {
    stop("Network error: unable to reach repository")
  })

  expect_error(
    suppressMessages(install_MStargetR()),
    "Network error"
  )
})

test_that("install_MStargetR propagates install.packages errors", {
  stub(install_MStargetR, "base::requireNamespace", function(pkg, ...) FALSE)
  stub(install_MStargetR, "install.packages", function(pkg, ...) {
    stop("Package installation failed for: ", pkg)
  })
  stub(install_MStargetR, "BiocManager::install", function(...) NULL)

  expect_error(
    suppressMessages(install_MStargetR()),
    "Package installation failed"
  )
})
