# Tests for auto-sanitisation of whitespace in raw_data/ vendor filenames.
#
# msconvert runs against bind-mounted vendor data and receives the file
# basename as a command argument; a blank space in the name is mis-parsed and
# the file cannot be located. mst_sanitize_raw_data_filenames() renames the
# offending entries on disk (whitespace -> underscore) before discovery, and
# read_plate_manifest() applies the same transform so manifests keep matching.

# -- mst_sanitize_filename (pure transform) ---------------------------------

test_that("mst_sanitize_filename collapses whitespace runs to single underscore", {
  expect_equal(mst_sanitize_filename("my file.raw"), "my_file.raw")
  expect_equal(mst_sanitize_filename("my   file.raw"), "my_file.raw")
  expect_equal(mst_sanitize_filename("a\tb.raw"), "a_b.raw")
  expect_equal(mst_sanitize_filename("clean.raw"), "clean.raw")
  # Vectorised.
  expect_equal(mst_sanitize_filename(c("a b.raw", "c.raw")),
               c("a_b.raw", "c.raw"))
})

# -- mst_sanitize_raw_data_filenames (filesystem) ---------------------------

test_that("a single spaced file is renamed on disk and reported", {
  tmp <- withr::local_tempdir()
  raw <- file.path(tmp, "raw_data")
  dir.create(raw, recursive = TRUE)
  file.create(file.path(raw, "my file.raw"))

  expect_message(
    res <- mst_sanitize_raw_data_filenames(tmp),
    "renamed raw_data"
  )
  expect_false(file.exists(file.path(raw, "my file.raw")))
  expect_true(file.exists(file.path(raw, "my_file.raw")))
  expect_equal(nrow(res), 1L)
  expect_equal(basename(res$to), "my_file.raw")
})

test_that(".wiff and .wiff.scan companion pair are renamed together", {
  tmp <- withr::local_tempdir()
  raw <- file.path(tmp, "raw_data")
  dir.create(raw, recursive = TRUE)
  file.create(file.path(raw, "my sample.wiff"))
  file.create(file.path(raw, "my sample.wiff.scan"))

  suppressMessages(mst_sanitize_raw_data_filenames(tmp))

  expect_true(file.exists(file.path(raw, "my_sample.wiff")))
  expect_true(file.exists(file.path(raw, "my_sample.wiff.scan")))
  expect_false(file.exists(file.path(raw, "my sample.wiff")))
})

test_that("a spaced file inside a spaced plate subfolder is renamed, folder too", {
  tmp <- withr::local_tempdir()
  raw <- file.path(tmp, "raw_data")
  dir.create(file.path(raw, "plate one"), recursive = TRUE)
  file.create(file.path(raw, "plate one", "sample a.raw"))

  suppressMessages(mst_sanitize_raw_data_filenames(tmp))

  expect_true(dir.exists(file.path(raw, "plate_one")))
  expect_true(file.exists(file.path(raw, "plate_one", "sample_a.raw")))
  expect_false(dir.exists(file.path(raw, "plate one")))
})

test_that("a spaced plate subfolder with a clean file renames only the folder", {
  tmp <- withr::local_tempdir()
  raw <- file.path(tmp, "raw_data")
  dir.create(file.path(raw, "plate two"), recursive = TRUE)
  file.create(file.path(raw, "plate two", "clean.raw"))

  suppressMessages(mst_sanitize_raw_data_filenames(tmp))

  expect_true(dir.exists(file.path(raw, "plate_two")))
  expect_true(file.exists(file.path(raw, "plate_two", "clean.raw")))
})

test_that("a .d vendor dir is renamed as a unit; its internals are untouched", {
  tmp <- withr::local_tempdir()
  raw <- file.path(tmp, "raw_data")
  dir.create(file.path(raw, "My Sample.d"), recursive = TRUE)
  # An internal file that itself contains a space must NOT be renamed: the .d
  # is a vendor directory passed/mounted whole and never descended into.
  file.create(file.path(raw, "My Sample.d", "analysis data.bin"))

  suppressMessages(mst_sanitize_raw_data_filenames(tmp))

  expect_true(dir.exists(file.path(raw, "My_Sample.d")))
  expect_false(dir.exists(file.path(raw, "My Sample.d")))
  # Internal name preserved (space intact) -> no descent into vendor dirs.
  expect_true(file.exists(file.path(raw, "My_Sample.d", "analysis data.bin")))
})

test_that("collision with an existing sanitised name errors without renaming", {
  tmp <- withr::local_tempdir()
  raw <- file.path(tmp, "raw_data")
  dir.create(raw, recursive = TRUE)
  file.create(file.path(raw, "my file.raw"))
  file.create(file.path(raw, "my_file.raw"))

  expect_error(
    suppressMessages(mst_sanitize_raw_data_filenames(tmp)),
    "already exists"
  )
  # Both originals intact - never clobbered.
  expect_true(file.exists(file.path(raw, "my file.raw")))
  expect_true(file.exists(file.path(raw, "my_file.raw")))
})

test_that("two distinct sources collapsing to one name errors", {
  tmp <- withr::local_tempdir()
  raw <- file.path(tmp, "raw_data")
  dir.create(raw, recursive = TRUE)
  file.create(file.path(raw, "a b.raw"))
  file.create(file.path(raw, "a  b.raw"))

  expect_error(
    suppressMessages(mst_sanitize_raw_data_filenames(tmp)),
    "collapse to the same name"
  )
})

test_that("a clean raw_data is a silent no-op", {
  tmp <- withr::local_tempdir()
  raw <- file.path(tmp, "raw_data")
  dir.create(raw, recursive = TRUE)
  file.create(file.path(raw, "clean1.raw"))
  file.create(file.path(raw, "clean2.raw"))

  expect_silent(res <- mst_sanitize_raw_data_filenames(tmp))
  expect_equal(nrow(res), 0L)
})

test_that("a missing raw_data/ returns silently (no error)", {
  tmp <- withr::local_tempdir()  # bare tempdir, no raw_data/
  expect_silent(res <- mst_sanitize_raw_data_filenames(tmp))
  expect_equal(nrow(res), 0L)
})

# -- manifest alignment ------------------------------------------------------

test_that("read_plate_manifest space-normalises raw_file to match renamed files", {
  man <- data.frame(raw_file = "my file.raw", plateID = "P1",
                    stringsAsFactors = FALSE)
  # known_files are the post-rename basenames produced by discovery.
  out <- read_plate_manifest(man, known_files = "my_file.raw")
  expect_equal(out$raw_file, "my_file.raw")
  expect_equal(out$plateID, "P1")
})

test_that("manifest rows collapsing to one file after normalisation error", {
  man <- data.frame(raw_file = c("a b.raw", "a  b.raw"),
                    plateID = c("P1", "P2"), stringsAsFactors = FALSE)
  expect_error(
    read_plate_manifest(man, known_files = "a_b.raw"),
    "duplicate"
  )
})
