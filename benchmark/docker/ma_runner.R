#!/usr/bin/env Rscript
# ma_runner.R -- in-container MetaboAnalystR stage dispatcher for the
# MStargetR head-to-head benchmark.
#
# Reads the canonical shared table (samples in rows: sample_name, sample_type,
# batch, run_order, <features...>), runs ONE processing stage, and:
#   * prints STAGE_TIME_S=<seconds> and STAGE_PEAK_MB=<MiB> to stdout
#     (only the MA compute call is timed; reshape/IO is excluded)
#   * writes the stage output as a canonical wide CSV to --out for the
#     equivalence comparison.
#
# Usage (inside container, /data is the bind-mounted benchmark dir):
#   Rscript /data/ma_runner.R --stage=combat \
#     --input=/data/shared_table.csv --out=/data/ma_out_combat.csv
#
# Stages: read_sanity impute filter_mv filter_rsd normalize combat qcrlsc pca
#
# NOTE: MetaboAnalystR's function signatures drift across versions. Each MA
# call is wrapped so that on failure we print the function's formals to the
# log -- the Docker smoke test (benchmark step in README) uses this to confirm
# argument names against the pinned image.

suppressWarnings(suppressMessages({
  library(MetaboAnalystR)
}))

# MetaboAnalystR accumulates user-facing messages in GLOBAL variables
# (current.msg, err.vec, msg.vec) that are normally pre-initialised by the web
# front end. In a bare Rscript context they may be unset, so the FIRST call to
# AddErrMsg() inside SanityCheckData (which emits a benign "N groups found"
# info message) errors with "object 'current.msg' not found". Pre-create them
# in .GlobalEnv. Also flag that we are NOT on the public web server.
for (.v in c("current.msg", "err.vec", "msg.vec")) {
  if (!exists(.v, envir = .GlobalEnv)) assign(.v, "", envir = .GlobalEnv)
}
if (!exists(".on.public.web", envir = .GlobalEnv)) {
  assign(".on.public.web", FALSE, envir = .GlobalEnv)
}

# ---- arg parsing (no external dep) ----------------------------------------
args <- commandArgs(trailingOnly = TRUE)
getarg <- function(name, default = NA_character_) {
  hit <- grep(sprintf("^--%s=", name), args, value = TRUE)
  if (length(hit) == 0) return(default)
  sub(sprintf("^--%s=", name), "", hit[1])
}
stage  <- getarg("stage")
input  <- getarg("input")
outp   <- getarg("out")
rsd_cut <- as.numeric(getarg("rsd", "30"))
mv_pct  <- as.numeric(getarg("mv_percent", "0.5"))
# Valid ImputeMissingVar method strings (source: R/util_missing.R):
#   "lod", "colmin", "mean", "median", "knn_var", "knn_smp",
#   "bpca", "ppca", "svdImpute", "missForest", "qrilc", "exclude"
# "min" is NOT valid; default is "lod" (half-minimum of each feature column).
impute_method <- getarg("impute_method", "lod")
scale_norm    <- getarg("scale", "ParetoNorm")   # match qcCheckR ropls pareto
stopifnot(!is.na(stage), !is.na(input), !is.na(outp))

# ---- helpers ---------------------------------------------------------------
emit <- function(time_s, peak_mb) {
  cat(sprintf("STAGE_TIME_S=%.6f\n", time_s))
  cat(sprintf("STAGE_PEAK_MB=%.3f\n", peak_mb))
}

# Time + peak-RAM bracket: returns elapsed seconds and peak MiB.
timed <- function(expr) {
  pr_time <- NA_real_; pr_mem <- NA_real_
  e <- substitute(expr); env <- parent.frame()
  if (requireNamespace("peakRAM", quietly = TRUE)) {
    res <- NULL
    pr <- peakRAM::peakRAM({ res <- eval(e, env) })
    pr_time <- pr$Elapsed_Time_sec[1]
    pr_mem  <- pr$Peak_RAM_Used_MiB[1]
    attr(res, ".time") <- pr_time; attr(res, ".mem") <- pr_mem
    return(res)
  }
  t0 <- proc.time()[["elapsed"]]
  res <- eval(e, env)
  attr(res, ".time") <- proc.time()[["elapsed"]] - t0
  attr(res, ".mem") <- NA_real_
  res
}

# Read canonical shared table.
read_shared <- function(path) {
  df <- utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
  meta <- c("sample_name", "sample_type", "batch", "run_order")
  list(df = df, meta = meta, feat = setdiff(names(df), meta))
}

# Write a MetaboAnalyst "rowu" upload file (Sample, Label, features...).
# Read.TextData(mSetObj, filePath, format="rowu", lbl.type="disc")
# Source: R/general_data_utils.R — col1=sample name, col2=class label,
# then feature columns. format "rowu" = samples in rows, unpaired labels.
write_ma_textfile <- function(sh, path, label_col = "sample_type") {
  out <- data.frame(Sample = sh$df$sample_name,
                    Label  = sh$df[[label_col]],
                    check.names = FALSE, stringsAsFactors = FALSE)
  out <- cbind(out, sh$df[, sh$feat, drop = FALSE])
  utils::write.csv(out, path, row.names = FALSE)
  path
}

# Write a MetaboAnalyst "rowu" upload file with DUMMY balanced 2-group labels.
#
# PURPOSE: SanityCheckData (R/general_proc_utils.R) enforces "at least 2 groups
# with >= 3 replicates per group" before saving preproc.qs.  With the real
# sample_type column, non-QC rows are all labelled "sample" => 1 group => the
# check returns -1 and preproc.qs is never written, breaking all downstream
# calls (ImputeMissingVar, FilterVariable, Normalization, PCA.Anal).
#
# For stages where the group label is irrelevant to the numeric computation
# (imputation, %-missing filter, normalisation, PCA scores all ignore group
# membership), we assign alternating "A"/"B" labels to every row — including
# rows that were previously "qc".  With 120 rows this gives 60 A / 60 B, well
# above the min-3-replicates threshold.  The feature VALUES and all computed
# statistics (PCA scores, normalization, etc.) are completely unaffected.
#
# EXCEPTION: filter_rsd uses qc.filter="T" which requires rows labelled "qc"
# so that FilterVariable can compute per-feature RSD over QC injections.  For
# that stage use write_ma_textfile_rsd() below instead.
write_ma_textfile_dummy <- function(sh, path) {
  n <- nrow(sh$df)
  # Alternate A/B so both halves are exactly equal (or differ by 1 for odd n).
  dummy_label <- ifelse(seq_len(n) %% 2 == 1, "A", "B")
  out <- data.frame(Sample = sh$df$sample_name,
                    Label  = dummy_label,
                    check.names = FALSE, stringsAsFactors = FALSE)
  out <- cbind(out, sh$df[, sh$feat, drop = FALSE])
  utils::write.csv(out, path, row.names = FALSE)
  path
}

# Write a MetaboAnalyst "rowu" upload file for the filter_rsd stage.
#
# PURPOSE: FilterVariable(qc.filter="T") identifies QC rows by checking
#   tolower(as.character(cls)) == "qc"
# and then computes per-feature RSD over those rows.  So we MUST keep the
# original "qc" labels for QC injections.
#
# At the same time SanityCheckData strips QC rows out before the group check:
#   cls.Clean = cls with "qc"/"blank" removed
# and then requires length(levels(cls.Clean)) >= 2 AND min(table(cls.Clean)) >= 3.
#
# With the real data (108 non-QC rows all labelled "sample") cls.Clean has only
# 1 level => check fails.  Fix: split the 108 non-QC rows into two equal
# dummy groups A/B so cls.Clean = A(54) + B(54) => 2 groups, 54 reps each.
# QC rows keep their "qc" label and are excluded from the group check, then
# recognised by FilterVariable for RSD computation.
write_ma_textfile_rsd <- function(sh, path) {
  n      <- nrow(sh$df)
  labels <- character(n)
  non_qc_idx <- which(tolower(sh$df$sample_type) != "qc")
  qc_idx     <- which(tolower(sh$df$sample_type) == "qc")
  # QC rows keep the "qc" label (case-insensitive match in FilterVariable)
  labels[qc_idx] <- "qc"
  # Non-QC rows split into alternating A/B dummy groups
  labels[non_qc_idx] <- ifelse(seq_along(non_qc_idx) %% 2 == 1, "A", "B")
  out <- data.frame(Sample = sh$df$sample_name,
                    Label  = labels,
                    check.names = FALSE, stringsAsFactors = FALSE)
  out <- cbind(out, sh$df[, sh$feat, drop = FALSE])
  utils::write.csv(out, path, row.names = FALSE)
  path
}

# Write a MetaboAnalyst batch combined table for Read.BatchDataTB.
# Source: R/batch_effect_utils.R Read.BatchDataTB (format="row") reads:
#   col1 = sample name  (dat[,1])
#   col2 = class label  (dat[,2])  <- QC samples must have class "QC"
#   col3 = batch        (dat[,3])
#   col4 = order        (dat[,4])
#   col5+ = features
# The previous code had col2=Injection.order, col3=Batch, col4=Class which
# was WRONG (class and order were swapped relative to what the source reads).
write_ma_batchfile <- function(sh, path) {
  out <- data.frame(
    Sample = sh$df$sample_name,
    # col2: class label; QC class must be exactly "QC" (case-sensitive grep
    # in my.batch.correct: grep("QC", as.character(class.lbl2))).
    Class  = ifelse(tolower(sh$df$sample_type) == "qc", "QC", "Sample"),
    # col3: batch ID
    Batch  = sh$df$batch,
    # col4: injection/run order
    Order  = sh$df$run_order,
    check.names = FALSE, stringsAsFactors = FALSE
  )
  out <- cbind(out, sh$df[, sh$feat, drop = FALSE])
  utils::write.csv(out, path, row.names = FALSE)
  path
}

# Pull the current working data matrix (samples x features) out of an mSet
# at whatever stage it is, normalizing to canonical wide schema.
mset_to_wide <- function(mSet, sample_names) {
  dat <- NULL
  # Only inspect inline slots if mSet is actually a list (some MA functions
  # return a numeric scalar on success, e.g. FilterVariable returns 1 or 2).
  if (is.list(mSet)) {
    for (slot in c("data.norm", "data.proc", "data.filtered")) {
      if (!is.null(mSet$dataSet[[slot]])) { dat <- mSet$dataSet[[slot]]; break }
    }
    if (is.null(dat)) dat <- mSet$dataSet$norm
  }
  # Fallback: read from the qs file cascade that MA functions write to disk.
  # Priority order matches the recency order used inside MetaboAnalystR
  # (most-processed file first):
  #   data.filt.qs  -- written by FilterVariable
  #   data_proc.qs  -- written by FilterVariable (no-NA path) / ImputeMissingVar
  #   preproc.qs    -- written by RemoveMissingByPercent / SanityCheckData
  #   preproc.orig.qs -- written by SanityCheckData (most raw)
  if (is.null(dat)) {
    qs_candidates <- c("data.filt.qs", "data_proc.qs", "preproc.qs", "preproc.orig.qs")
    for (qf in qs_candidates) {
      if (file.exists(file.path(getwd(), qf))) {
        dat <- MetaboAnalystR:::ov_qs_read(file.path(getwd(), qf))
        break
      }
    }
  }
  dat <- as.data.frame(dat, check.names = FALSE)
  out <- cbind(sample_name = rownames(dat), dat)
  rownames(out) <- NULL
  out
}

# Try an MA call; on error, dump its formals to stderr to aid smoke testing.
safe_ma <- function(fn_name, ...) {
  fn <- get(fn_name, envir = asNamespace("MetaboAnalystR"))
  tryCatch(fn(...), error = function(e) {
    message(sprintf("[ma_runner] %s() failed: %s", fn_name, conditionMessage(e)))
    message(sprintf("[ma_runner] %s formals: %s", fn_name,
                    paste(names(formals(fn)), collapse = ", ")))
    stop(e)
  })
}

sh <- read_shared(input)
work <- tempfile(fileext = ".csv")
setwd(tempdir())  # MA writes pca_score.csv etc. into the working dir

# ---- build a base mSet through SanityCheck (shared by most stages) --------
# InitDataObjects signature (R/general_data_utils.R):
#   InitDataObjects(data.type, anal.type, paired=FALSE, default.dpi)
#   data.type: "conc" for concentration/peak table data
#   anal.type: "stat" for statistical analysis path
#
# label_mode controls which label column SanityCheckData sees:
#   "real"   -- use actual sample_type (needed for read_sanity)
#   "dummy"  -- alternating A/B for all rows (impute/filter_mv/normalize/pca)
#   "rsd"    -- qc rows keep "qc"; non-QC rows split A/B (filter_rsd)
init_stat <- function(label_mode = "real") {
  if (label_mode == "dummy") {
    write_ma_textfile_dummy(sh, work)
  } else if (label_mode == "rsd") {
    write_ma_textfile_rsd(sh, work)
  } else {
    write_ma_textfile(sh, work)
  }
  # InitDataObjects: data.type="conc", anal.type="stat", paired=FALSE
  # Source: R/general_data_utils.R
  mSet <- InitDataObjects("conc", "stat", FALSE, default.dpi = 72L)
  # Read.TextData signature (R/general_data_utils.R):
  #   Read.TextData(mSetObj, filePath, format="rowu", lbl.type="disc", nmdr=FALSE)
  #   format "rowu" = samples in rows, unpaired; lbl.type "disc" = discrete groups
  mSet <- safe_ma("Read.TextData", mSet, work, "rowu", "disc")
  # SanityCheckData signature: SanityCheckData(mSetObj=NA)
  # Source: R/general_proc_utils.R
  mSet <- safe_ma("SanityCheckData", mSet)
  mSet
}

result_df <- NULL
elapsed <- NA_real_; peak <- NA_real_

if (stage == "read_sanity") {
  mSet <- InitDataObjects("conc", "stat", FALSE, default.dpi = 72L)
  write_ma_textfile(sh, work)
  r <- timed({
    mSet <- safe_ma("Read.TextData", mSet, work, "rowu", "disc")
    mSet <- safe_ma("SanityCheckData", mSet)
    mSet
  })
  elapsed <- attr(r, ".time"); peak <- attr(r, ".mem")
  result_df <- sh$df  # unchanged; just proves the read path works

} else if (stage == "impute") {
  # Dummy 2-group labels: SanityCheckData requires >= 2 groups with >= 3 reps
  # each. With real labels all non-QC rows are "sample" (1 group) => check
  # returns -1 and never saves preproc.qs, so ImputeMissingVar fails.
  # Dummy A/B labels (60/60) satisfy the check. The imputed values are
  # identical regardless of the group label.
  #
  # NOTE: ReplaceMin() does NOT exist in MetaboAnalystR 4.3.0.
  # (exists("ReplaceMin", asNamespace("MetaboAnalystR")) == FALSE)
  # The equivalent behaviour -- replacing zeros AND NAs with 1/5 of the
  # per-column minimum positive value -- is performed internally by
  # ImputeMissingVar(method="lod") via MetaboAnalystR:::.replace.by.lod:
  #   lod <- min(x[x > 0], na.rm=T) / 5
  #   x[x == 0 | is.na(x)] <- lod
  # So ImputeMissingVar alone is sufficient; no separate zero-replacement step.
  mSet <- init_stat("dummy")
  r <- timed({
    # ImputeMissingVar signature (R/general_proc_utils.R -> R/util_missing.R):
    #   ImputeMissingVar(mSetObj=NA, method="lod", grpLod=F, grpMeasure=F)
    # Valid method strings: "lod","colmin","mean","median","knn_var","knn_smp",
    #   "bpca","ppca","svdImpute","missForest","qrilc","exclude"
    # NOTE: "min" and "KNN" are NOT valid; use "lod" or "colmin" for min-based.
    mSet <- safe_ma("ImputeMissingVar", mSet, method = impute_method)
    mSet
  })
  elapsed <- attr(r, ".time"); peak <- attr(r, ".mem")
  result_df <- mset_to_wide(r, sh$df$sample_name)

} else if (stage == "filter_mv") {
  # Dummy 2-group labels for the same SanityCheckData reason as impute.
  # RemoveMissingByPercent does not use group labels; the filter is global.
  mSet <- init_stat("dummy")
  # BUG WORKAROUND — MetaboAnalystR 4.3.0 partial-match collision in
  # RemoveMissingByPercent (R/general_proc_utils.R):
  #   if (!.on.public.web && !is.null(mSetObj$dataSet$proc)) {
  #     int.mat <- mSetObj$dataSet$proc   # <-- bug: $ does partial matching
  #
  # With .on.public.web=FALSE the function tries mSetObj$dataSet$proc first.
  # The $ operator does PARTIAL matching, so "proc" matches "proc.cls" (a factor
  # of class labels set by SanityCheckData), not the intended data matrix.
  # !is.null(factor) is TRUE, so the function assigns the factor to int.mat
  # and then crashes at colMeans(is.na(int.mat)) with 'not a 2D array'.
  #
  # Fix: pre-populate mSet$dataSet$proc with the actual preprocessed data
  # matrix read from preproc.orig.qs (written by SanityCheckData).  The exact
  # "proc" key now has priority over the partial-match to "proc.cls".
  proc_mat_path <- file.path(getwd(), "preproc.orig.qs")
  if (file.exists(proc_mat_path)) {
    mSet$dataSet$proc <- as.data.frame(
      MetaboAnalystR:::ov_qs_read(proc_mat_path)
    )
  }
  # RemoveMissingByPercent signature (R/general_proc_utils.R):
  #   RemoveMissingByPercent(mSetObj=NA, percent=0.20, grpWise=FALSE)
  #   percent is a FRACTION (0–1), not a percentage (0–100).
  #   Default 0.20 = remove features missing in >20% of samples.
  r <- timed({ safe_ma("RemoveMissingByPercent", mSet, percent = mv_pct) })
  elapsed <- attr(r, ".time"); peak <- attr(r, ".mem")
  result_df <- mset_to_wide(r, sh$df$sample_name)

} else if (stage == "filter_rsd") {
  # RSD filter: needs QC labels for FilterVariable(qc.filter="T") AND needs
  # >= 2 non-QC groups for SanityCheckData.
  # write_ma_textfile_rsd() uses "qc" for QC rows and alternating A/B for
  # non-QC rows: SanityCheckData sees cls.Clean={A(54),B(54)} => passes;
  # FilterVariable sees rows where cls=="qc" => computes RSD over those.
  #
  # NOTE: ReplaceMin() does NOT exist in MetaboAnalystR 4.3.0 (see impute
  # stage above). Removed; the lod path inside FilterVariable / ImputeMissingVar
  # handles zero/NA replacement internally.
  mSet <- init_stat("rsd")
  r <- timed({
    # FilterVariable CURRENT signature (R/general_proc_utils.R):
    #   FilterVariable(mSetObj=NA, qc.filter="F", rsd, var.filter="iqr",
    #                  var.cutoff=NULL, int.filter="mean", int.cutoff=0,
    #                  blank.subtraction=F, blank.threshold=10)
    # CORRECTIONS vs old code:
    #   - arg is qc.filter (dot), NOT qcFilter (camelCase)
    #   - there is NO "filter" argument in the current signature
    #   - qc.filter uses string "T"/"F" (not logical TRUE/FALSE)
    #   - rsd is passed as a percentage (e.g. 30 for 30%); inside the
    #     qc.filter=="T" block the function does rsd <- rsd/100 itself.
    safe_ma("FilterVariable", mSet, qc.filter = "T", rsd = rsd_cut)
  })
  elapsed <- attr(r, ".time"); peak <- attr(r, ".mem")
  result_df <- mset_to_wide(r, sh$df$sample_name)

} else if (stage == "normalize") {
  # Dummy 2-group labels for the same SanityCheckData reason as impute.
  # Normalization (log + pareto scaling) does not use group labels.
  #
  # NOTE: ReplaceMin() does NOT exist in MetaboAnalystR 4.3.0.  Removed.
  # Normalization reads from prenorm.qs which is populated by
  # .prepare.prenorm.data (called internally at the top of Normalization()).
  # The prenorm source files (preproc.qs / preproc.orig.qs) are written by
  # SanityCheckData only when it passes, hence the dummy label fix above.
  #
  # PreparePrenormData is still called explicitly here for clarity; it is
  # harmless redundancy since Normalization() calls .prepare.prenorm.data
  # internally as its first step.
  mSet <- init_stat("dummy")
  mSet <- safe_ma("PreparePrenormData", mSet)
  r <- timed({
    # Normalization signature (R/general_norm_utils.R):
    #   Normalization(mSetObj=NA, rowNorm, transNorm, scaleNorm,
    #                 ref=NULL, ratio=FALSE, ratioNum=20)
    # rowNorm "NULL" string: falls through to else/no-op (no row norm).
    # Valid transNorm: "LogNorm","Log2Norm","SrNorm","CrNorm","VsnNorm"
    # Valid scaleNorm: "MeanCenter","AutoNorm","ParetoNorm","RangeNorm"
    # Passing "NULL" string for rowNorm/transNorm/scaleNorm is harmless
    # (falls to else/N/A branch) — this matches the MetaboAnalyst web
    # interface convention for "no transformation".
    safe_ma("Normalization", mSet, "NULL", "LogNorm", scale_norm,
            ratio = FALSE, ratioNum = 20)
  })
  elapsed <- attr(r, ".time"); peak <- attr(r, ".mem")
  result_df <- mset_to_wide(r, sh$df$sample_name)

} else if (stage == "pca") {
  # Dummy 2-group labels for the same SanityCheckData reason as impute.
  # PCA scores are purely geometric and unaffected by group labels.
  #
  # NOTE: ReplaceMin() does NOT exist in MetaboAnalystR 4.3.0.  Removed.
  # PCA.Anal reads mSetObj$dataSet$norm which is populated by Normalization().
  #
  # NOTE: PCA.Anal() is NOT called here because it requires the RSclient and
  # factoextra packages (both absent from this Docker image) to compute variable
  # contributions via run_func_via_rsclient().  It crashes unconditionally with
  # "there is no package called 'RSclient'" before writing pca_score.csv.
  #
  # Instead, we replicate the PCA.Anal score computation directly:
  #   1. Run Normalization to populate mSetObj$dataSet$norm (logged + pareto).
  #   2. Call prcomp(..., center=TRUE, scale=FALSE) exactly as PCA.Anal does.
  #   3. Write pca_score.csv with signif(pca$x, 5) and row.names=TRUE (same
  #      format as PCA.Anal would produce).
  # Variable contributions are omitted (they require factoextra) but the sample
  # scores PC1–PCn are numerically identical to what PCA.Anal would produce.
  mSet <- init_stat("dummy")
  mSet <- safe_ma("PreparePrenormData", mSet)
  mSet <- safe_ma("Normalization", mSet, "NULL", "LogNorm", scale_norm,
                  ratio = FALSE, ratioNum = 20)
  r <- timed({
    # Replicate PCA.Anal's prcomp call on the normalised matrix.
    # mSet$dataSet$norm is set by Normalization() (verified in source).
    norm_mat <- mSet$dataSet$norm
    pca <- prcomp(norm_mat, center = TRUE, scale. = FALSE)
    # Write pca_score.csv in same format as PCA.Anal (for any downstream reader)
    score_path <- file.path(getwd(), "pca_score.csv")
    MetaboAnalystR:::fast.write.csv(signif(pca$x, 5), file = score_path)
    pca   # return prcomp object so attr(.time/.mem) can be attached
  })
  elapsed <- attr(r, ".time"); peak <- attr(r, ".mem")
  # Read the written pca_score.csv (row 1 = header with PC names, col 1 = sample names).
  score_path <- file.path(getwd(), "pca_score.csv")
  if (file.exists(score_path)) {
    sc <- utils::read.csv(score_path, check.names = FALSE, row.names = 1)
    result_df <- cbind(sample_name = rownames(sc), sc[, 1:min(3, ncol(sc)), drop = FALSE])
  } else {
    sc <- r$x
    result_df <- cbind(sample_name = rownames(sc), as.data.frame(sc)[, 1:min(3, ncol(sc)), drop = FALSE])
  }

} else if (stage %in% c("combat", "qcrlsc")) {
  # Batch-effect correction path.
  #
  # Read.BatchDataTB signature (R/batch_effect_utils.R):
  #   Read.BatchDataTB(mSetObj=NA, filePath, format, missingEstimate)
  # format="row": reads col1=sample, col2=class, col3=batch, col4=order, col5+=features
  # missingEstimate: "lods","rmean","rmed","knn","ppca" (handled inside the fn)
  #
  # PerformBatchCorrection signature (R/batch_effect_utils.R):
  #   PerformBatchCorrection(mSetObj=NA, imgName=NULL, Method=NULL, center=NULL)
  # Valid Method strings (case-sensitive): "Combat","QC_RLSC","WaveICA",
  #   "EigenMS","ANCOVA","RUV_random","RUV_2","RUV_s","RUV_r","RUV_g",
  #   "NOMIS","CCMN","auto"
  # center: "" (empty string) for no centering; "QC" to center on QC samples.
  #
  # Output file (written by my.batch.correct via write.table):
  #   "MetaboAnalyst_batch_data.csv"
  #   columns: NAME, CLASS, Dataset, <feature1>, <feature2>, ...
  #   (source: R/util_batch.R — colnames(res) <- c('NAME','CLASS','Dataset',colnames(best.table)))
  #
  # InitDataObjects: "raw" is NOT a valid anal.type; use "stat" which is the
  # recognized path. The function has no special handling for unknown types and
  # passes them to .init.global.vars, but "stat" is the safest choice for a
  # peak-table batch-correction workflow.
  method <- if (stage == "combat") "Combat" else "QC_RLSC"
  batch_file <- tempfile(fileext = ".csv")
  write_ma_batchfile(sh, batch_file)
  # Use "stat" anal.type (not "raw" — "raw" is not a documented anal.type value)
  mSet <- InitDataObjects("conc", "stat", FALSE, default.dpi = 72L)
  # Read.BatchDataTB requires 4 positional args — missingEstimate has no default.
  # Use "lods" (limit of detection substitution) as the safe default.
  mSet <- safe_ma("Read.BatchDataTB", mSet, batch_file, "row", "lods")
  r <- timed({ safe_ma("PerformBatchCorrection", mSet, "", method, "") })
  elapsed <- attr(r, ".time"); peak <- attr(r, ".mem")
  # MetaboAnalyst_batch_data.csv columns: NAME, CLASS, Dataset, <features...>
  # Drop the metadata columns CLASS and Dataset; rename NAME -> sample_name.
  cand <- c("MetaboAnalyst_batch_data.csv", "MetaboAnalyst_signal_drift.csv")
  found <- cand[file.exists(file.path(getwd(), cand))]
  if (length(found) > 0) {
    corr <- utils::read.csv(file.path(getwd(), found[1]),
                            check.names = FALSE, stringsAsFactors = FALSE)
    # col1 is "NAME" (sample identifiers) — rename to canonical "sample_name"
    names(corr)[1] <- "sample_name"
    # Drop known MA metadata columns that follow the sample-name column
    meta_drop <- intersect(c("CLASS", "Class", "class",
                             "Dataset", "dataset",
                             "Batch", "batch",
                             "Order", "order",
                             "Injection.order"),
                           names(corr))
    result_df <- corr[, setdiff(names(corr), meta_drop), drop = FALSE]
  } else {
    result_df <- mset_to_wide(r, sh$df$sample_name)
  }

} else if (stage %in% c("full_combat", "full_qcrlsc")) {
  # End-to-end "complete pipeline" timing for the batch-correction workflow:
  # Read.BatchDataTB -> PerformBatchCorrection -> PCA, all in ONE container
  # invocation. This is the realistic per-run cost of taking a shared peak
  # table through MA's batch-correction + ordination path. Compared against
  # the MStargetR full_* row of the same name, it isolates pipeline throughput
  # from the per-stage Docker startup overhead the per-stage rows carry.
  #
  # PerformBatchCorrection internally runs MA's preprocess (impute / drop / log)
  # before the correction itself, so this single block is the complete
  # comparable pipeline. PCA uses prcomp(scale.=TRUE) for parity with the
  # MStargetR full_* path (both autoscale on the corrected matrix).
  method <- if (stage == "full_combat") "Combat" else "QC_RLSC"
  batch_file <- tempfile(fileext = ".csv")
  write_ma_batchfile(sh, batch_file)
  mSet <- InitDataObjects("conc", "stat", FALSE, default.dpi = 72L)
  r <- timed({
    mSet <- safe_ma("Read.BatchDataTB", mSet, batch_file, "row", "lods")
    mSet <- safe_ma("PerformBatchCorrection", mSet, "", method, "")
    cand <- c("MetaboAnalyst_batch_data.csv", "MetaboAnalyst_signal_drift.csv")
    found <- cand[file.exists(file.path(getwd(), cand))]
    corr_mat <- if (length(found) > 0) {
      d <- utils::read.csv(file.path(getwd(), found[1]),
                           check.names = FALSE, stringsAsFactors = FALSE)
      meta_drop <- intersect(c("CLASS","Class","class","Dataset","dataset",
                               "Batch","batch","Order","order","Injection.order",
                               "NAME"), names(d))
      mat <- as.matrix(d[, setdiff(names(d), meta_drop), drop = FALSE])
      rownames(mat) <- d[[1]]
      mat
    } else NULL
    if (!is.null(corr_mat) && nrow(corr_mat) > 1 && ncol(corr_mat) > 1) {
      # Drop zero-variance features so scale.=TRUE doesn't divide by zero.
      keep <- apply(corr_mat, 2, function(x) is.finite(stats::sd(x)) && stats::sd(x) > 0)
      prcomp(corr_mat[, keep, drop = FALSE], center = TRUE, scale. = TRUE)
    } else NULL
  })
  elapsed <- attr(r, ".time"); peak <- attr(r, ".mem")
  if (!is.null(r) && !is.null(r$x)) {
    sc <- r$x
    result_df <- cbind(sample_name = rownames(sc),
                       as.data.frame(sc)[, 1:min(3, ncol(sc)), drop = FALSE])
  } else {
    result_df <- data.frame(sample_name = sh$df$sample_name)
  }

} else {
  stop(sprintf("ma_runner: unknown --stage '%s'", stage))
}

# ---- write output + emit timings ------------------------------------------
if (!is.null(result_df)) {
  utils::write.csv(result_df, outp, row.names = FALSE)
}
if (requireNamespace("digest", quietly = TRUE)) {
  cat(sprintf("INPUT_SHA256=%s\n", digest::digest(file = input, algo = "sha256")))
}
emit(elapsed, peak)
