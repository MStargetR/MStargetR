# benchmark/03_scaling_shared.R
# Shared-table head-to-head SCALING sweep. At each grid point a single table is
# generated (bootstrapped from the real peak table), hashed, then fed to BOTH
# engines so timings are on identical input. Sweeps one axis at a time around a
# baseline (samples=500, features=200, batches=2).
#
# Output: results/bm_scaling.csv + bm_scaling_{samples,features,batches}.png
#         + bm_memory.png
#
# This is the expensive script -- run in the background. Limit the grid with
# env vars MSTARGETR_BENCH_QUICK=1 (tiny grid for validation).

suppressWarnings(suppressMessages({
  library(MStargetR); library(ggplot2)
}))

here <- dirname(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1]))
if (is.na(here) || !nzchar(here)) here <- "benchmark"
source(file.path(here, "lib", "config.R"))
source(file.path(here, "lib", "timing.R"))
source(file.path(here, "lib", "scale_table.R"))

ensure_results_dir()
pin_single_thread()
quick <- nzchar(Sys.getenv("MSTARGETR_BENCH_QUICK"))

# Real table to bootstrap from (falls back to synthetic if not present).
real_df <- if (file.exists(shared_table_csv())) {
  utils::read.csv(shared_table_csv(), check.names = FALSE, stringsAsFactors = FALSE)
} else {
  NULL
}
gen <- function(ns, nf, nb, seed) {
  if (is.null(real_df)) make_scaled_table(ns, nf, nb, seed)
  else make_scaled_from_real(real_df, ns, nf, nb, seed = seed)
}

# ---- grid (one-axis-at-a-time around baseline) -----------------------------
base_s <- 500L; base_f <- 200L; base_b <- 2L
if (quick) {
  grid <- rbind(
    data.frame(axis = "samples",  n_samples = c(50, 100), n_features = base_f, n_batches = base_b),
    data.frame(axis = "features", n_samples = base_s, n_features = c(50, 100), n_batches = base_b)
  )
} else {
  grid <- rbind(
    data.frame(axis = "samples",  n_samples = c(50,100,250,500,1000,2100),
               n_features = base_f, n_batches = base_b),
    data.frame(axis = "features", n_samples = base_s,
               n_features = c(50,200,500,1000), n_batches = base_b),
    data.frame(axis = "batches",  n_samples = base_s, n_features = base_f,
               n_batches = c(1,2,5,10))
  )
}
grid <- unique(grid)

file.copy(file.path(here, "docker", "ma_runner.R"),
          bench_path("ma_runner.R"), overwrite = TRUE)

# QCRFSC (statTarget/vroom) is slow and may fail in quick-mode validation; keep
# it only in the full run to avoid noisy DLL errors during CI/quick checks.
ms_methods <- if (quick) c("ComBat", "QCRLSC") else c("ComBat", "QCRLSC", "QCRFSC")
ma_methods <- c(combat = "ComBat", qcrlsc = "QCRLSC")

scale_temp <- bench_path("scale_table.csv")   # regenerated per grid point
records <- list()

for (i in seq_len(nrow(grid))) {
  g <- grid[i, ]
  message(sprintf("03-shared [%d/%d] axis=%s  s=%d f=%d b=%d",
                  i, nrow(grid), g$axis, g$n_samples, g$n_features, g$n_batches))
  tbl <- gen(g$n_samples, g$n_features, g$n_batches, seed = 100 + i)
  hash <- write_shared_table(tbl, scale_temp)
  feat_cols <- bench_feature_cols(tbl)

  # MStargetR side
  for (m in ms_methods) {
    row <- tryCatch(
      measure_stage(m, iterations = if (quick) 2L else 3L, expr = {
        MStargetR::batchCorrectR(tbl, qc_label = BENCH_QC_LABEL, method = m,
                                 report = FALSE, plot = FALSE)
      }),
      error = function(e) { message("   MStargetR ", m, ": ", conditionMessage(e)); NULL })
    if (!is.null(row)) {
      row$method <- m
      records[[length(records) + 1]] <- cbind(row[, c("engine","stage","method",
        "wall_s_median","wall_s_sd","peak_mb")], g, hash = hash)
    }
  }

  # MetaboAnalyst side (Docker)
  for (st in names(ma_methods)) {
    row <- tryCatch(
      run_ma_stage_repeated(st, input = basename(scale_temp),
                            out = sprintf("ma_scale_%s.csv", st),
                            mount_dir = bench_dir(), reps = if (quick) 1L else 2L),
      error = function(e) { message("   MA ", st, ": ", conditionMessage(e)); NULL })
    if (!is.null(row)) {
      rr <- data.frame(engine = "MetaboAnalyst", stage = st,
                       method = unname(ma_methods[st]),
                       wall_s_median = row$wall_s_median, wall_s_sd = row$wall_s_sd,
                       peak_mb = row$peak_mb, stringsAsFactors = FALSE)
      records[[length(records) + 1]] <- cbind(rr, g, hash = hash)
    }
  }
}

res <- do.call(rbind, records)
outfile <- file.path(results_dir(), "bm_scaling.csv")
utils::write.csv(res, outfile, row.names = FALSE)
message("03-shared: wrote ", outfile)

# ---- plots -----------------------------------------------------------------
mk_plot <- function(df, xvar, xlab, file) {
  df <- df[is.finite(df$wall_s_median) & df$wall_s_median > 0, ]
  if (nrow(df) == 0) return(invisible())
  df$grp <- paste(df$engine, df$method)
  p <- ggplot(df, aes(.data[[xvar]], wall_s_median, colour = grp)) +
    geom_point(size = 2) + geom_line() +
    scale_x_log10() + scale_y_log10() +
    labs(title = sprintf("Correction wall-clock vs %s (log-log)", xlab),
         x = xlab, y = "wall-clock (s, warm median)", colour = "engine x method") +
    theme_bw()
  ggsave(file.path(results_dir(), file), p, width = 8, height = 5, dpi = 120)
}
mk_plot(res[res$axis == "samples", ], "n_samples", "number of samples",
        "bm_scaling_samples.png")
mk_plot(res[res$axis == "features", ], "n_features", "number of features",
        "bm_scaling_features.png")
mk_plot(res[res$axis == "batches", ], "n_batches", "number of batches",
        "bm_scaling_batches.png")

# memory vs samples
mem <- res[res$axis == "samples" & is.finite(res$peak_mb), ]
if (nrow(mem) > 0) {
  mem$grp <- paste(mem$engine, mem$method)
  p <- ggplot(mem, aes(n_samples, peak_mb, colour = grp)) +
    geom_point(size = 2) + geom_line() +
    labs(title = "Peak memory vs number of samples", x = "number of samples",
         y = "peak RAM (MiB)", colour = "engine x method") + theme_bw()
  ggsave(file.path(results_dir(), "bm_memory.png"), p, width = 8, height = 5, dpi = 120)
}
message("03-shared: plots written to ", results_dir())
