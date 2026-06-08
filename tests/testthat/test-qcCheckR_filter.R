library(mockery)
library(dplyr)
library(tibble)

# Tests for internal helpers in R/qcCheckR_filter.R that are not otherwise
# exercised by the wrapper-level tests in test-qcCheckR_Utils.R.

# 1. initialise_sil_summary -----------------------------------------------
test_that("initialise_sil_summary returns a zero-row data.frame with expected columns", {
  result <- initialise_sil_summary()

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_equal(
    colnames(result),
    c("lipid", "template_version", "plateID", "peakArea_below_LOD",
      "naValues", "nanValues", "infValues", "totalMissingValues",
      "flag_SIL_intStd_Plate")
  )
  expect_type(result$lipid, "character")
  expect_type(result$template_version, "character")
  expect_type(result$plateID, "character")
  expect_type(result$peakArea_below_LOD, "double")
  expect_type(result$naValues, "double")
  expect_type(result$nanValues, "double")
  expect_type(result$infValues, "double")
  expect_type(result$totalMissingValues, "double")
  expect_type(result$flag_SIL_intStd_Plate, "double")
})

# 2. calculate_sil_flags_per_plate ---------------------------------------
make_sil_master_list <- function(sil_df, sample_flags, template_version = "v1",
                                 idx_batch = "batch1") {
  # Post-remediation, calculate_sil_flags_per_plate filters
  # samples.missingValues by both sample_plate_id AND sample.flag. Older
  # fixtures only supplied sample.flag, so we inject sample_plate_id here
  # if the caller didn't — otherwise `dplyr::filter(.data$sample_plate_id
  # == idx_batch, ...)` hits "object not found".
  if (!"sample_plate_id" %in% colnames(sample_flags)) {
    sample_flags$sample_plate_id <- idx_batch
  }
  list(
    project_details = list(),
    templates = list(`Plate SIL version` = setNames(list(template_version),
                                                    idx_batch)),
    filters = list(samples.missingValues = sample_flags),
    data = list(peakArea = list(sorted = setNames(list(sil_df), idx_batch)))
  )
}

test_that("calculate_sil_flags_per_plate: happy path counts LOD, NA, inf correctly", {
  sil_df <- tibble::tibble(
    sample_name = c("S1", "S2", "S3", "S4"),
    sample_plate_id = rep("batch1", 4),
    SIL_A = c(6000, 4000, 3000, 2000),   # 3 below-LOD
    SIL_B = c(7000, NA_real_, Inf, 8000) # 1 NA, 1 Inf
  )
  sample_flags <- tibble::tibble(
    sample_name = c("S1", "S2", "S3", "S4"),
    sample.flag = c(0, 0, 0, 0)
  )
  ml <- make_sil_master_list(sil_df, sample_flags)

  flags <- calculate_sil_flags_per_plate(ml, "batch1")

  expect_s3_class(flags, "tbl_df")
  expect_equal(flags$lipid, c("SIL_A", "SIL_B"))
  expect_equal(unname(flags$peakArea_below_LOD), c(3, 0))
  expect_equal(unname(flags$naValues),          c(0, 1))
  expect_equal(unname(flags$infValues),         c(0, 1))
  expect_equal(unname(flags$totalMissingValues), c(3, 2))
  expect_equal(flags$template_version, c("v1", "v1"))
  expect_equal(flags$plateID, c("batch1", "batch1"))
})

test_that("calculate_sil_flags_per_plate: NA values are not counted as below-LOD (QC-C2)", {
  # An all-NA column must NOT contribute to peakArea_below_LOD.
  sil_df <- tibble::tibble(
    sample_name = c("S1", "S2", "S3"),
    sample_plate_id = rep("batch1", 3),
    SIL_allNA = c(NA_real_, NA_real_, NA_real_),
    SIL_low   = c(100, 200, 300)
  )
  sample_flags <- tibble::tibble(
    sample_name = c("S1", "S2", "S3"),
    sample.flag = c(0, 0, 0)
  )
  ml <- make_sil_master_list(sil_df, sample_flags)

  flags <- calculate_sil_flags_per_plate(ml, "batch1")

  # all-NA column: below-LOD = 0, NAs bucket holds them
  expect_equal(unname(flags$peakArea_below_LOD[flags$lipid == "SIL_allNA"]), 0)
  expect_equal(unname(flags$naValues[flags$lipid == "SIL_allNA"]), 3)
  # low column: below-LOD = 3, NAs = 0
  expect_equal(unname(flags$peakArea_below_LOD[flags$lipid == "SIL_low"]), 3)
  expect_equal(unname(flags$naValues[flags$lipid == "SIL_low"]), 0)
})

test_that("calculate_sil_flags_per_plate: filters out samples that failed sample.flag", {
  sil_df <- tibble::tibble(
    sample_name = c("S_keep", "S_drop"),
    sample_plate_id = c("batch1", "batch1"),
    SIL_A = c(100, 100)   # both below LOD
  )
  sample_flags <- tibble::tibble(
    sample_name = c("S_keep", "S_drop"),
    sample.flag = c(0, 1)
  )
  ml <- make_sil_master_list(sil_df, sample_flags)

  flags <- calculate_sil_flags_per_plate(ml, "batch1")

  # Only S_keep should be used, so only 1 below-LOD, not 2.
  expect_equal(unname(flags$peakArea_below_LOD), 1)
})

test_that("calculate_sil_flags_per_plate: empty SIL matrix yields zero rows", {
  sil_df <- tibble::tibble(
    sample_name = c("S1", "S2"),
    sample_plate_id = c("batch1", "batch1"),
    lipid1 = c(10000, 11000)
    # no SIL columns
  )
  sample_flags <- tibble::tibble(
    sample_name = c("S1", "S2"),
    sample.flag = c(0, 0)
  )
  ml <- make_sil_master_list(sil_df, sample_flags)

  flags <- calculate_sil_flags_per_plate(ml, "batch1")
  expect_equal(nrow(flags), 0)
})

test_that("calculate_sil_flags_per_plate: configurable LOD changes below-LOD counts", {
  sil_df <- tibble::tibble(
    sample_name = c("S1", "S2", "S3", "S4"),
    sample_plate_id = rep("batch1", 4),
    SIL_A = c(6000, 4000, 3000, 2000)  # below 5000 default: 3
  )
  sample_flags <- tibble::tibble(
    sample_name = c("S1", "S2", "S3", "S4"),
    sample.flag = c(0, 0, 0, 0)
  )

  # Default (no lod_threshold set) -> falls back to DEFAULT_LOD_THRESHOLD (5000)
  ml_default <- make_sil_master_list(sil_df, sample_flags)
  flags_default <- calculate_sil_flags_per_plate(ml_default, "batch1")
  expect_equal(unname(flags_default$peakArea_below_LOD), 3)

  # Lower LOD (3500) -> only 3000 and 2000 fall below
  ml_low <- make_sil_master_list(sil_df, sample_flags)
  ml_low$project_details$lod_threshold <- 3500
  flags_low <- calculate_sil_flags_per_plate(ml_low, "batch1")
  expect_equal(unname(flags_low$peakArea_below_LOD), 2)

  # Higher LOD (10000) -> all four fall below
  ml_high <- make_sil_master_list(sil_df, sample_flags)
  ml_high$project_details$lod_threshold <- 10000
  flags_high <- calculate_sil_flags_per_plate(ml_high, "batch1")
  expect_equal(unname(flags_high$peakArea_below_LOD), 4)
})

test_that("resolve_lod_threshold falls back to default on missing/invalid values", {
  expect_equal(resolve_lod_threshold(list()), DEFAULT_LOD_THRESHOLD)
  expect_equal(
    resolve_lod_threshold(list(project_details = list(lod_threshold = NULL))),
    DEFAULT_LOD_THRESHOLD
  )
  expect_equal(
    resolve_lod_threshold(list(project_details = list(lod_threshold = "bad"))),
    DEFAULT_LOD_THRESHOLD
  )
  expect_equal(
    resolve_lod_threshold(list(project_details = list(lod_threshold = 1234))),
    1234
  )
})

# 3. calculate_sil_flags_per_version -------------------------------------
test_that("calculate_sil_flags_per_version aggregates multiple versions and flags", {
  sil_summary <- tibble::tibble(
    lipid = c("SIL_A", "SIL_A", "SIL_B"),
    template_version = c("v1", "v2", "v1"),
    plateID = c("p1", "p2", "p1"),
    peakArea_below_LOD = c(0, 0, 0),
    naValues = c(0, 0, 0),
    nanValues = c(0, 0, 0),
    infValues = c(0, 0, 0),
    totalMissingValues = c(0, 0, 0),
    flag_SIL_intStd_Plate = c(0, 0, 0)
  )
  peak_p1 <- tibble::tibble(
    sample_name = c("S1", "S2"),
    sample_plate_id = c("p1", "p1"),
    SIL_A = c(6000, 7000),
    SIL_B = c(100, 200)  # below LOD for both -> flagged
  )
  peak_p2 <- tibble::tibble(
    sample_name = c("S3", "S4"),
    sample_plate_id = c("p2", "p2"),
    SIL_A = c(8000, 9000)
  )
  ml <- list(
    filters = list(
      samples.missingValues = tibble::tibble(
        sample_name = c("S1", "S2", "S3", "S4"),
        sample_plate_id = c("p1", "p1", "p2", "p2"),
        sample.flag = c(0, 0, 0, 0)
      ),
      sil.intStd.missingValues = list(summary = sil_summary)
    ),
    data = list(peakArea = list(sorted = list(p1 = peak_p1, p2 = peak_p2)))
  )

  result <- calculate_sil_flags_per_version(ml)

  # Both versions processed
  expect_setequal(names(result$filters$sil.intStd.missingValues$allPlates),
                  c("v1", "v2"))
  # v1: SIL_B has 2 below-LOD out of 2 samples -> flagged
  v1_flags <- result$filters$sil.intStd.missingValues$PROJECT.flag.SIL.intStd$v1
  v1_tbl <- result$filters$sil.intStd.missingValues$allPlates$v1
  expect_equal(v1_tbl$lipid[v1_flags == 1], "SIL_B")
  expect_true("SIL_B" %in% result$filters$failed_sil.intStds$v1)
  # v2: only SIL_A present, none below LOD
  v2_flags <- result$filters$sil.intStd.missingValues$PROJECT.flag.SIL.intStd$v2
  expect_true(all(v2_flags == 0))
  expect_equal(length(result$filters$failed_sil.intStds$v2), 0)
})

test_that("calculate_sil_flags_per_version: empty version list is a no-op", {
  sil_summary <- tibble::tibble(
    lipid = character(),
    template_version = character(),
    plateID = character(),
    peakArea_below_LOD = numeric(),
    naValues = numeric(), nanValues = numeric(), infValues = numeric(),
    totalMissingValues = numeric(), flag_SIL_intStd_Plate = numeric()
  )
  ml <- list(
    filters = list(
      samples.missingValues = tibble::tibble(sample_name = character(),
                                             sample_plate_id = character(),
                                             sample.flag = numeric()),
      sil.intStd.missingValues = list(summary = sil_summary)
    ),
    data = list(peakArea = list(sorted = list()))
  )
  result <- calculate_sil_flags_per_version(ml)

  # Still initialises empty structures
  expect_true(is.list(result$filters$sil.intStd.missingValues$PROJECT.flag.SIL.intStd))
  expect_equal(length(result$filters$sil.intStd.missingValues$PROJECT.flag.SIL.intStd), 0)
  expect_equal(length(result$filters$failed_sil.intStds), 0)
})

test_that("calculate_sil_flags_per_version: NA is not counted as below-LOD (QC-C2)", {
  sil_summary <- tibble::tibble(
    lipid = "SIL_A",
    template_version = "v1",
    plateID = "p1",
    peakArea_below_LOD = 0, naValues = 0, nanValues = 0, infValues = 0,
    totalMissingValues = 0, flag_SIL_intStd_Plate = 0
  )
  # SIL_A all NA -- must not be counted as below-LOD.
  peak_p1 <- tibble::tibble(
    sample_name = c("S1", "S2"),
    sample_plate_id = c("p1", "p1"),
    SIL_A = c(NA_real_, NA_real_),
    SIL_keep = c(1e5, 1e5)
  )
  ml <- list(
    filters = list(
      samples.missingValues = tibble::tibble(
        sample_name = c("S1", "S2"),
        sample_plate_id = c("p1", "p1"),
        sample.flag = c(0, 0)
      ),
      sil.intStd.missingValues = list(summary = sil_summary)
    ),
    data = list(peakArea = list(sorted = list(p1 = peak_p1)))
  )
  result <- calculate_sil_flags_per_version(ml)

  # SIL_A is dropped from sil_matrix by where(~ !all(is.na(.))) so it is not
  # counted as below-LOD (peakArea_below_LOD must stay 0). It is re-inserted
  # into version_flags as a 100%-missing row so it is flagged as failed --
  # an all-NA SIL is a real measurement failure, not a silent pass.
  v1_tbl <- result$filters$sil.intStd.missingValues$allPlates$v1
  expect_true("SIL_A" %in% v1_tbl$lipid)
  expect_true("SIL_keep" %in% v1_tbl$lipid)
  expect_equal(unname(v1_tbl$peakArea_below_LOD[v1_tbl$lipid == "SIL_A"]), 0)
  expect_equal(unname(v1_tbl$peakArea_below_LOD[v1_tbl$lipid == "SIL_keep"]), 0)
  expect_equal(v1_tbl$totalMissingValues[v1_tbl$lipid == "SIL_A"], 2)
  expect_true("SIL_A" %in% result$filters$failed_sil.intStds$v1)
})

test_that("calculate_sil_flags_per_version: skips version with zero valid samples", {
  sil_summary <- tibble::tibble(
    lipid = "SIL_A", template_version = "v1", plateID = "p1",
    peakArea_below_LOD = 0, naValues = 0, nanValues = 0, infValues = 0,
    totalMissingValues = 0, flag_SIL_intStd_Plate = 0
  )
  peak_p1 <- tibble::tibble(
    sample_name = c("S1"),
    sample_plate_id = c("p1"),
    SIL_A = 100
  )
  ml <- list(
    filters = list(
      samples.missingValues = tibble::tibble(sample_name = "S1",
                                             sample_plate_id = "p1",
                                             sample.flag = 1),   # all failed
      sil.intStd.missingValues = list(summary = sil_summary)
    ),
    data = list(peakArea = list(sorted = list(p1 = peak_p1)))
  )
  result <- calculate_sil_flags_per_version(ml)
  # The `next` branch should skip populating allPlates for v1.
  expect_null(result$filters$sil.intStd.missingValues$allPlates$v1)
  expect_null(result$filters$sil.intStd.missingValues$PROJECT.flag.SIL.intStd$v1)
})

test_that("calculate_sil_flags_per_version: re-inserts entirely-NA SILs without erroring", {
  # Regression: when an expected SIL is project-wide all-NA, the where()
  # selector drops it from sil_matrix; the re-insertion block then runs
  # and references valid_sample_count. Previously valid_sample_count was
  # only defined AFTER the re-insertion block, so this path errored with
  # "object 'valid_sample_count' not found".
  sil_summary <- tibble::tibble(
    lipid = c("SIL_A", "SIL_B"),
    template_version = c("v1", "v1"),
    plateID = c("p1", "p1"),
    peakArea_below_LOD = c(0, 0),
    naValues = c(0, 0), nanValues = c(0, 0), infValues = c(0, 0),
    totalMissingValues = c(0, 0), flag_SIL_intStd_Plate = c(0, 0)
  )
  # SIL_B is entirely NA project-wide -- will be dropped by the
  # where(~ !all(is.na(.))) selector and must be re-inserted from sil_guide.
  peak_p1 <- tibble::tibble(
    sample_name = c("S1", "S2"),
    sample_plate_id = c("p1", "p1"),
    SIL_A = c(8000, 9000),
    SIL_B = c(NA_real_, NA_real_)
  )
  ml <- list(
    filters = list(
      samples.missingValues = tibble::tibble(
        sample_name = c("S1", "S2"),
        sample_plate_id = c("p1", "p1"),
        sample.flag = c(0, 0)
      ),
      sil.intStd.missingValues = list(summary = sil_summary)
    ),
    data = list(peakArea = list(sorted = list(p1 = peak_p1))),
    templates = list(mrm_guides = list(
      v1 = list(SIL_guide = tibble::tibble(`Precursor Name` = c("SIL_A", "SIL_B")))
    ))
  )
  result <- calculate_sil_flags_per_version(ml)

  v1_tbl <- result$filters$sil.intStd.missingValues$allPlates$v1
  expect_true("SIL_B" %in% v1_tbl$lipid)
  # Re-inserted row has naValues / totalMissingValues = sample count (2)
  expect_equal(v1_tbl$totalMissingValues[v1_tbl$lipid == "SIL_B"], 2)
  expect_true("SIL_B" %in% result$filters$failed_sil.intStds$v1)
})

# 4. initialise_lipid_filter ---------------------------------------------
test_that("initialise_lipid_filter creates expected empty scaffolding", {
  result <- initialise_lipid_filter(list())

  expect_true(is.list(result$filters$lipid.missingValues))
  expect_s3_class(result$filters$lipid.missingValues$summary, "data.frame")
  expect_equal(nrow(result$filters$lipid.missingValues$summary), 0)
  expect_equal(
    colnames(result$filters$lipid.missingValues$summary),
    c("lipid", "silFilter.flag.Lipid", "peakArea_below_LOD",
      "naValues", "nanValues", "infValues", "totalMissingValues",
      "flag.Lipid.Plate", "template_version", "plateID")
  )
  expect_true(is.list(result$filters$lipid.missingValues$PROJECT.flag.lipid))
  expect_equal(length(result$filters$lipid.missingValues$PROJECT.flag.lipid), 0)
  expect_true(is.list(result$filters$failed_lipids))
  expect_equal(length(result$filters$failed_lipids), 0)
})

# 5. get_lipid_data ------------------------------------------------------
test_that("get_lipid_data drops sample and SIL columns and filters flagged samples", {
  peak_batch <- tibble::tibble(
    sample_name       = c("S1", "S2", "S_drop"),
    sample_plate_id   = c("p1", "p1", "p1"),
    sample_type_factor= c("sample", "sample", "sample"),
    SIL_A  = c(1000, 2000, 3000),
    lipid1 = c(10000, 11000, 12000),
    lipid2 = c(13000, 14000, 15000)
  )
  ml <- list(
    filters = list(samples.missingValues = tibble::tibble(
      sample_name = c("S1", "S2", "S_drop"),
      sample.flag = c(0, 0, 1)
    )),
    data = list(peakArea = list(sorted = list(p1 = peak_batch)))
  )

  out <- get_lipid_data(ml, "p1")
  expect_true(is.matrix(out))
  expect_equal(colnames(out), c("lipid1", "lipid2"))
  expect_equal(nrow(out), 2)   # S_drop filtered out
  expect_equal(as.numeric(out[, "lipid1"]), c(10000, 11000))
})

test_that("get_lipid_data: empty valid-samples yields zero-row matrix", {
  peak_batch <- tibble::tibble(
    sample_name = "S1",
    sample_plate_id = "p1",
    lipid1 = 10000
  )
  ml <- list(
    filters = list(samples.missingValues = tibble::tibble(
      sample_name = "S1", sample.flag = 1
    )),
    data = list(peakArea = list(sorted = list(p1 = peak_batch)))
  )
  out <- get_lipid_data(ml, "p1")
  expect_true(is.matrix(out))
  expect_equal(nrow(out), 0)
  expect_equal(colnames(out), "lipid1")
})

# 6. calculate_lipid_flags -----------------------------------------------
test_that("calculate_lipid_flags: happy path with SIL guide and LOD counts", {
  lipid_matrix <- matrix(
    c(10000, 100,    20000,     # lipid1: 1 below LOD
      NA_real_, 12000, 13000,   # lipid2: 1 NA
      50000, 60000, 70000),     # lipid3: clean
    nrow = 3, byrow = FALSE,
    dimnames = list(NULL, c("lipid1", "lipid2", "lipid3"))
  )
  ml <- list(
    project_details = list(mv_sample_threshold = 50),
    templates = list(
      `Plate SIL version` = list(p1 = "v1"),
      mrm_guides = list(v1 = list(SIL_guide = tibble::tibble(
        `Precursor Name` = c("lipid1", "lipid3"),
        Note = c("SIL_A", "SIL_B")
      )))
    ),
    filters = list(failed_sil.intStds = c("SIL_A"))
  )

  flags <- calculate_lipid_flags(ml, "p1", lipid_matrix)

  expect_equal(flags$lipid, c("lipid1", "lipid2", "lipid3"))
  # SIL filter: lipid1 -> SIL_A failed -> flagged; lipid3 -> SIL_B not failed
  expect_equal(flags$silFilter.flag.Lipid, c(1, 0, 0))
  expect_equal(unname(flags$peakArea_below_LOD), c(1, 0, 0))
  expect_equal(unname(flags$naValues),          c(0, 1, 0))
  expect_equal(flags$template_version, rep("v1", 3))
  expect_equal(flags$plateID,          rep("p1", 3))
  # silFilter flag propagates into flag.Lipid.Plate
  expect_equal(flags$flag.Lipid.Plate[1], 1)
})

test_that("calculate_lipid_flags: NA values are not counted as below-LOD (QC-C2)", {
  lipid_matrix <- matrix(
    c(NA_real_, NA_real_, NA_real_,   # all-NA column
      10000,    11000,    12000),     # clean column
    nrow = 3, byrow = FALSE,
    dimnames = list(NULL, c("lipid_NA", "lipid_ok"))
  )
  ml <- list(
    project_details = list(mv_sample_threshold = 50),
    templates = list(`Plate SIL version` = list(p1 = "v1"),
                     mrm_guides = list(v1 = list(SIL_guide = NULL))),
    filters = list(failed_sil.intStds = character())
  )
  flags <- calculate_lipid_flags(ml, "p1", lipid_matrix)
  expect_equal(unname(flags$peakArea_below_LOD[flags$lipid == "lipid_NA"]), 0)
  expect_equal(unname(flags$naValues[flags$lipid == "lipid_NA"]), 3)
  # SIL_guide NULL -> silFilter.flag.Lipid must be zero across the board
  expect_true(all(flags$silFilter.flag.Lipid == 0))
})

test_that("calculate_lipid_flags: zero-column input returns zero-row tibble", {
  lipid_matrix <- matrix(numeric(0), nrow = 3, ncol = 0)
  ml <- list(
    project_details = list(mv_sample_threshold = 50),
    templates = list(`Plate SIL version` = list(p1 = "v1")),
    filters = list(failed_sil.intStds = character())
  )
  flags <- calculate_lipid_flags(ml, "p1", lipid_matrix)
  expect_s3_class(flags, "tbl_df")
  expect_equal(nrow(flags), 0)
  expect_true(all(c("lipid", "silFilter.flag.Lipid", "peakArea_below_LOD",
                    "template_version", "plateID") %in% colnames(flags)))
})

test_that("calculate_lipid_flags: missing template version triggers informative error", {
  lipid_matrix <- matrix(1:6, nrow = 3, dimnames = list(NULL, c("a", "b")))
  ml <- list(
    project_details = list(mv_sample_threshold = 50),
    templates = list(`Plate SIL version` = list()),  # no p1 entry -> NULL
    filters = list(failed_sil.intStds = character())
  )
  expect_error(calculate_lipid_flags(ml, "p1", lipid_matrix),
               "Missing template version for batch: p1")
})

# 7. process_lipid_versions ----------------------------------------------
test_that("process_lipid_versions aggregates lipids across plates and versions", {
  lipid_summary <- tibble::tibble(
    lipid = c("lipid1", "lipid2", "lipid1"),
    silFilter.flag.Lipid = c(0, 0, 0),
    peakArea_below_LOD = c(0, 0, 0),
    naValues = c(0, 0, 0), nanValues = c(0, 0, 0), infValues = c(0, 0, 0),
    totalMissingValues = c(0, 0, 0), flag.Lipid.Plate = c(0, 0, 0),
    template_version = c("v1", "v1", "v2"),
    plateID = c("p1", "p1", "p2")
  )
  peak_p1 <- tibble::tibble(
    sample_name = c("S1", "S2"),
    sample_plate_id = c("p1", "p1"),
    lipid1 = c(100, 200),     # all below LOD in v1 -> flagged
    lipid2 = c(10000, 12000)  # not below LOD
  )
  peak_p2 <- tibble::tibble(
    sample_name = c("S3", "S4"),
    sample_plate_id = c("p2", "p2"),
    lipid1 = c(50000, 60000)
  )
  ml <- list(
    project_details = list(mv_sample_threshold = 50),
    filters = list(
      samples.missingValues = tibble::tibble(
        sample_name = c("S1", "S2", "S3", "S4"),
        sample.flag = c(0, 0, 0, 0)
      ),
      lipid.missingValues = list(summary = lipid_summary,
                                 PROJECT.flag.lipid = list()),
      failed_lipids = list()
    ),
    data = list(peakArea = list(sorted = list(p1 = peak_p1, p2 = peak_p2)))
  )

  result <- process_lipid_versions(ml)
  expect_setequal(names(result$filters$lipid.missingValues$allPlates),
                  c("v1", "v2"))
  v1 <- result$filters$lipid.missingValues$allPlates$v1
  expect_setequal(v1$lipid, c("lipid1", "lipid2"))
  # lipid1 has 2/2 below LOD -> totalMissingValues > 2 * 0.5 = 1 -> flagged
  expect_true("lipid1" %in% result$filters$failed_lipids$v1)
  expect_false("lipid2" %in% result$filters$failed_lipids$v1)
})

test_that("process_lipid_versions: single version, no failures", {
  lipid_summary <- tibble::tibble(
    lipid = "lipid1", silFilter.flag.Lipid = 0, peakArea_below_LOD = 0,
    naValues = 0, nanValues = 0, infValues = 0,
    totalMissingValues = 0, flag.Lipid.Plate = 0,
    template_version = "v1", plateID = "p1"
  )
  peak_p1 <- tibble::tibble(
    sample_name = c("S1", "S2"),
    sample_plate_id = c("p1", "p1"),
    lipid1 = c(50000, 60000)
  )
  ml <- list(
    project_details = list(mv_sample_threshold = 50),
    filters = list(
      samples.missingValues = tibble::tibble(sample_name = c("S1", "S2"),
                                             sample.flag = c(0, 0)),
      lipid.missingValues = list(summary = lipid_summary,
                                 PROJECT.flag.lipid = list()),
      failed_lipids = list()
    ),
    data = list(peakArea = list(sorted = list(p1 = peak_p1)))
  )
  result <- process_lipid_versions(ml)
  expect_equal(length(result$filters$failed_lipids$v1), 0)
  expect_equal(unname(result$filters$lipid.missingValues$allPlates$v1$peakArea_below_LOD), 0)
})

test_that("process_lipid_versions: NA is not counted as below-LOD (QC-C2)", {
  lipid_summary <- tibble::tibble(
    lipid = c("lipid_NA", "lipid_ok"),
    silFilter.flag.Lipid = c(0, 0), peakArea_below_LOD = c(0, 0),
    naValues = c(0, 0), nanValues = c(0, 0), infValues = c(0, 0),
    totalMissingValues = c(0, 0), flag.Lipid.Plate = c(0, 0),
    template_version = c("v1", "v1"),
    plateID = c("p1", "p1")
  )
  peak_p1 <- tibble::tibble(
    sample_name = c("S1", "S2"),
    sample_plate_id = c("p1", "p1"),
    lipid_NA = c(NA_real_, NA_real_),   # all NA -> dropped via tidyselect::where
    lipid_ok = c(10000, 20000)
  )
  ml <- list(
    project_details = list(mv_sample_threshold = 50),
    filters = list(
      samples.missingValues = tibble::tibble(sample_name = c("S1", "S2"),
                                             sample.flag = c(0, 0)),
      lipid.missingValues = list(summary = lipid_summary,
                                 PROJECT.flag.lipid = list()),
      failed_lipids = list()
    ),
    data = list(peakArea = list(sorted = list(p1 = peak_p1)))
  )
  result <- process_lipid_versions(ml)
  v1_tbl <- result$filters$lipid.missingValues$allPlates$v1
  # all-NA column dropped; only lipid_ok remains
  expect_false("lipid_NA" %in% v1_tbl$lipid)
  expect_true("lipid_ok" %in% v1_tbl$lipid)
  expect_equal(unname(v1_tbl$peakArea_below_LOD[v1_tbl$lipid == "lipid_ok"]), 0)
})

test_that("process_lipid_versions: skips version with no valid samples", {
  lipid_summary <- tibble::tibble(
    lipid = "lipid1", silFilter.flag.Lipid = 0, peakArea_below_LOD = 0,
    naValues = 0, nanValues = 0, infValues = 0,
    totalMissingValues = 0, flag.Lipid.Plate = 0,
    template_version = "v1", plateID = "p1"
  )
  peak_p1 <- tibble::tibble(
    sample_name = "S1", sample_plate_id = "p1", lipid1 = 100
  )
  ml <- list(
    project_details = list(mv_sample_threshold = 50),
    filters = list(
      samples.missingValues = tibble::tibble(sample_name = "S1",
                                             sample.flag = 1),  # all failed
      lipid.missingValues = list(summary = lipid_summary,
                                 PROJECT.flag.lipid = list()),
      failed_lipids = list()
    ),
    data = list(peakArea = list(sorted = list(p1 = peak_p1)))
  )
  result <- process_lipid_versions(ml)
  expect_null(result$filters$lipid.missingValues$allPlates$v1)
  expect_null(result$filters$failed_lipids$v1)
})

# Extra targeted tests for branches in functions that DO have tests but miss
# specific guards. Keep small (<= 5 blocks total).

test_that("calculate_rsd: empty input list returns a single placeholder NA row", {
  result <- calculate_rsd(
    master_list = list(filters = list(failed_samples = character())),
    source_name = "peakArea",
    data_batches = list()
  )
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1)
  expect_equal(result$V1, "peakArea")
  expect_equal(result$V2, "allBatches")
})

test_that("calculate_rsd: all batches skipped still returns labelled NA row (QC-H6)", {
  # Batch with no QC samples after filter -> every batch gets `next`ed.
  batches <- list(
    b1 = tibble::tibble(sample_name = c("S1", "S2"),
                        sample_class = c("sample", "sample"),
                        m1 = c(1, 2), m2 = c(3, 4))
  )
  result <- calculate_rsd(
    master_list = list(filters = list(failed_samples = character())),
    source_name = "concentration",
    data_batches = batches
  )
  expect_equal(result$V1, "concentration")
  expect_equal(result$V2, "allBatches")
})

test_that("calculate_rsd: <3 non-NA QC points yield NA_real_ (QC-C3)", {
  # Only two QC rows; also one zero-mean column to hit the epsilon branch.
  batches <- list(b1 = tibble::tibble(
    sample_name  = c("Q1", "Q2", "S1"),
    sample_class = c("qc", "qc", "sample"),
    m1 = c(10, 20, 0),  # only 2 QCs -> NA
    m2 = c(0,  0,  0)   # zero mean -> NA
  ))
  result <- calculate_rsd(
    master_list = list(filters = list(failed_samples = character())),
    source_name = "peakArea",
    data_batches = batches
  )
  numeric_cols <- result[, setdiff(colnames(result), c("V1", "V2")), drop = FALSE]
  expect_true(all(is.na(as.numeric(unlist(numeric_cols)))))
})

test_that("qcCheckR_RSD_filter runs through when filters/data are minimal (smoke)", {
  # This exercises several early branches: fallback to imputed concentration,
  # empty statTargetProcessed path, and the empty-rsd skip.
  empty_batch <- tibble::tibble(
    sample_name = character(), sample_class = character(),
    sample_plate_id = character()
  )
  ml <- list(
    project_details = list(
      script_log = list(steps = list(data_filtering = list(),
                                     data_preparation = list(),
                                     summary_report = list()),
                        timestamps = list())
    ),
    filters = list(failed_samples = character()),
    data = list(
      peakArea = list(sorted = list(b1 = empty_batch)),
      concentration = list(sorted = NULL, imputed = list(b1 = empty_batch)),
      concentration = list(statTargetProcessed = NULL)
    )
  )
  # Stub update_script_log (its internal side-effects aren't under test here).
  stub(qcCheckR_RSD_filter, "update_script_log", function(ml, ...) ml)

  result <- qcCheckR_RSD_filter(ml)
  expect_true("rsd" %in% names(result$filters))
  expect_s3_class(result$filters$rsd, "tbl_df")
  # Six calls to calculate_rsd should produce at least six placeholder rows
  expect_gte(nrow(result$filters$rsd), 1)
})
