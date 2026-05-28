# benchmark/03_scaling_fullpipe.R
# MStargetR-only FULL-pipeline scaling: vendor .wiff -> mzML (msConvertR) ->
# peak picking (PeakForgeR) -> QC/correct/report (qcCheckR), on replicated
# copies of the two real .wiff files. MetaboAnalyst cannot ingest .wiff, so
# this curve has no MA counterpart -- it quantifies the front-end stages that
# are unique to MStargetR. Regenerates bm_res.csv (+ linear/loess fits).
#
# REQUIRES Docker (msConvertR/PeakForgeR) on the host. Gated behind
# MSTARGETR_RUN_PIPELINE=1 because each point runs the heavy container stack.
#
# Output (benchmark folder): bm_res.csv, bm_linear.png, bm_loess.png
#         results/bm_fullpipe_stages.csv (per-stage breakdown)

suppressWarnings(suppressMessages({ library(MStargetR); library(ggplot2) }))

here <- dirname(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1]))
if (is.na(here) || !nzchar(here)) here <- "benchmark"
source(file.path(here, "lib", "config.R"))
source(file.path(here, "lib", "timing.R"))

ensure_results_dir()

if (!nzchar(Sys.getenv("MSTARGETR_RUN_PIPELINE"))) {
  stop("03-fullpipe: set MSTARGETR_RUN_PIPELINE=1 to run the vendor pipeline ",
       "(needs Docker Desktop + the Skyline/ProteoWizard images).")
}

raw <- list.files(raw_data_dir(), pattern = "\\.wiff$", full.names = TRUE)
if (length(raw) == 0) stop("03-fullpipe: no .wiff files in ", raw_data_dir())
template <- system.file("extdata", "LGW_lipid_mrm_template_v1.tsv",
                        package = "MStargetR")

# Replication grid: how many copies of the base .wiff set per point.
# Full grid reduced to c(1,2,4) for tractable runtime (~163->652 samples,
# 3-point curve still shows clear linear scaling). The rep_n=8 point (~1304
# samples, ~2.3 h alone) made the sweep ~4.5 h and fragile, so it is dropped;
# override with MSTARGETR_FULLPIPE_REPS="1,2,4,8" if a longer curve is wanted.
reps_env <- Sys.getenv("MSTARGETR_FULLPIPE_REPS", unset = "")
n_reps <- if (nzchar(Sys.getenv("MSTARGETR_BENCH_QUICK"))) {
  c(1L)
} else if (nzchar(reps_env)) {
  as.integer(strsplit(reps_env, ",")[[1]])
} else {
  c(1L, 2L, 4L)
}
samples_per_wiff <- as.integer(Sys.getenv("MSTARGETR_SAMPLES_PER_WIFF", "84"))

stage_time <- function(expr) {
  e <- substitute(expr); env <- parent.frame()
  if (requireNamespace("peakRAM", quietly = TRUE)) {
    pr <- peakRAM::peakRAM({ eval(e, env) })
    return(c(sec = pr$Elapsed_Time_sec[1], mb = pr$Peak_RAM_Used_MiB[1]))
  }
  t0 <- proc.time()[["elapsed"]]; eval(e, env)
  c(sec = proc.time()[["elapsed"]] - t0, mb = NA_real_)
}

# ---- Checkpoint: persist each completed rep so a mid-run kill never loses
# already-finished points. The heavy container stack (msConvertR/PeakForgeR)
# can exhaust host RAM on the larger reps and get the R process killed; without
# this, every restart re-ran reps 1..2 from scratch. We append each rep's stage
# rows to results/bm_fullpipe_checkpoint.csv as soon as it finishes, and skip
# any rep already present on restart.
ckpt_file <- file.path(results_dir(), "bm_fullpipe_checkpoint.csv")
ckpt_rows <- if (file.exists(ckpt_file)) {
  utils::read.csv(ckpt_file, stringsAsFactors = FALSE)
} else {
  data.frame(n_wiff = integer(0), n_samples = integer(0), stage = character(0),
             wall_s = numeric(0), peak_mb = numeric(0), stringsAsFactors = FALSE)
}
done_nwiff <- unique(ckpt_rows$n_wiff)

stage_rows <- if (nrow(ckpt_rows)) split(ckpt_rows, seq_len(nrow(ckpt_rows))) else list()
for (rep_n in n_reps) {
  n_wiff_pt <- rep_n * length(raw)
  if (n_wiff_pt %in% done_nwiff) {
    message(sprintf("03-fullpipe: %d wiff-set copies already in checkpoint -- skipping.", rep_n))
    next
  }
  message(sprintf("03-fullpipe: %d wiff-set copies ...", rep_n))
  proj <- file.path(tempdir(), sprintf("fullpipe_%02d", rep_n))
  unlink(proj, recursive = TRUE); dir.create(file.path(proj, "raw_data"),
                                              recursive = TRUE, showWarnings = FALSE)
  # Replicate the base .wiff (+ .wiff.scan) set rep_n times with unique names.
  for (k in seq_len(rep_n)) {
    for (w in raw) {
      base <- tools::file_path_sans_ext(basename(w))
      file.copy(w, file.path(proj, "raw_data", sprintf("%s_c%02d.wiff", base, k)))
      scan <- paste0(w, ".scan")
      if (file.exists(scan))
        file.copy(scan, file.path(proj, "raw_data", sprintf("%s_c%02d.wiff.scan", base, k)))
    }
  }

  # Skip msConvertR if every expected plate dir already has .mzML files
  # (same guard used in 01_make_shared_table.R to avoid file.copy errors on re-run).
  wiff_in_proj <- list.files(file.path(proj, "raw_data"),
                             pattern = "\\.wiff$", ignore.case = TRUE,
                             full.names = FALSE)
  plate_names_proj <- sub("\\.wiff$", "", wiff_in_proj, ignore.case = TRUE)
  plates_have_mzml <- vapply(plate_names_proj, function(p) {
    d <- file.path(proj, p, "data", "mzml")
    dir.exists(d) && length(list.files(d, pattern = "\\.mzML$", ignore.case = TRUE)) > 0
  }, logical(1))
  conv_skipped <- length(plate_names_proj) > 0 && all(plates_have_mzml)

  t_conv <- stage_time({
    if (conv_skipped) {
      message("03-fullpipe: mzML already present -- skipping msConvertR for this point.")
    } else {
      msConvertR(input_directory = proj, output_directory = proj)
    }
  })

  # Wrap PeakForgeR in tryCatch: Skyline CSVs are written before post-checks
  # fire, so a late error is non-fatal (same pattern as 01_make_shared_table.R).
  # mrm_template_list must be a named list (v1=template) not unnamed.
  t_peak <- stage_time({
    tryCatch(
      PeakForgeR(user_name = "benchmark", project_directory = proj,
                 mrm_template_list = list(v1 = template), QC_sample_label = "LTR"),
      error = function(e) {
        message("03-fullpipe: PeakForgeR threw (continuing -- Skyline CSVs should exist):\n    ",
                conditionMessage(e))
      }
    )
  })

  # qcCheckR: write_rda=FALSE avoids the .rda/.qs2 output; named mrm_template_list.
  t_qc <- stage_time({
    qcCheckR(user_name = "benchmark", project_directory = proj,
             mrm_template_list = list(v1 = list(SIL_guide = template,
               conc_guide = system.file("extdata", "LGW_SIL_batch_103.tsv",
                                         package = "MStargetR"))),
             QC_sample_label = "LTR", sample_tags = c("COND", "VLTR", "PQC", "LTR"),
             batch_method = "ComBat", write_rda = FALSE)
  })

  n_samples <- rep_n * length(raw) * samples_per_wiff
  # NB: keep the (name, named-numeric) pairs in a Map rather than c()'ing them
  # together -- c("msConvertR", t_conv) coerces the timing vector to character
  # and [[ ]] then drops the "sec"/"mb" names, which silently produced all-NA
  # wall_s/peak_mb columns in the checkpoint on the first run.
  rep_rows <- do.call(rbind, Map(
    function(stage_name, t) data.frame(
      n_wiff = n_wiff_pt, n_samples = n_samples, stage = stage_name,
      wall_s = unname(t["sec"]), peak_mb = unname(t["mb"]),
      stringsAsFactors = FALSE),
    c("msConvertR", "PeakForgeR", "qcCheckR"),
    list(t_conv, t_peak, t_qc)))
  stage_rows[[length(stage_rows) + 1]] <- rep_rows
  # Append this rep to the checkpoint immediately (write header only if new).
  utils::write.table(rep_rows, ckpt_file, sep = ",", row.names = FALSE,
                     col.names = !file.exists(ckpt_file), append = file.exists(ckpt_file))
  unlink(proj, recursive = TRUE)
}

stages <- do.call(rbind, stage_rows)
stages <- stages[order(stages$n_wiff), ]

# Only emit the final artifacts once every grid point is present (a kill mid-run
# leaves the checkpoint partial; the next restart finishes the curve, then writes).
expected_nwiff <- sort(n_reps * length(raw))
if (!all(expected_nwiff %in% unique(stages$n_wiff))) {
  missing <- setdiff(expected_nwiff, unique(stages$n_wiff))
  stop(sprintf(paste0("03-fullpipe: checkpoint incomplete (have n_wiff %s; missing %s). ",
                      "Re-run to resume the remaining point(s); final CSV/plots not written yet."),
               paste(sort(unique(stages$n_wiff)), collapse = ","),
               paste(missing, collapse = ",")))
}

utils::write.csv(stages, file.path(results_dir(), "bm_fullpipe_stages.csv"),
                 row.names = FALSE)

# ---- bm_res.csv (total pipeline, prior schema) ----------------------------
tot <- aggregate(wall_s ~ n_wiff + n_samples, stages, sum)
bm_res <- data.frame(
  n_wiff_files    = tot$n_wiff,
  samples         = tot$n_samples,
  time_mins       = round(tot$wall_s / 60, 2),
  time_hours      = round(tot$wall_s / 3600, 4),
  samples_per_min = round(tot$n_samples / (tot$wall_s / 60), 4),
  samples_per_hour= round(tot$n_samples / (tot$wall_s / 3600), 4)
)
bm_res <- bm_res[order(bm_res$n_wiff_files), ]
utils::write.csv(bm_res, bench_path("bm_res.csv"), row.names = FALSE)
message("03-fullpipe: wrote ", bench_path("bm_res.csv"))

# ---- linear + loess fits (regenerate prior plots) -------------------------
mk_fit_plot <- function(method, file, subtitle) {
  p <- ggplot(bm_res, aes(samples, time_hours)) +
    geom_point(size = 3) +
    geom_smooth(method = method, se = FALSE, colour = "blue",
                formula = y ~ x) +
    labs(title = "Total Analysis Time vs Number of Samples",
         subtitle = subtitle, x = "Number of samples", y = "Time (hours)") +
    theme_bw(base_size = 14)
  ggsave(bench_path(file), p, width = 9, height = 6, dpi = 120)
}
mk_fit_plot("lm",    "bm_linear.png", "Linear (blue)")
mk_fit_plot("loess", "bm_loess.png",  "LOESS (blue)")
message("03-fullpipe: regenerated bm_linear.png / bm_loess.png")
