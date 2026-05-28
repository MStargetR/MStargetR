# benchmark/lib/timing.R
# Timing + memory measurement helpers with single-thread pinning, so the
# MStargetR-on-host and MetaboAnalystR-in-Docker numbers are comparable.
#
# Requires (host side): bench, peakRAM. Optional: RhpcBLASctl, data.table.

# ---- Thread pinning --------------------------------------------------------
# Pin every parallel backend to a single thread so the head-to-head measures
# single-core compute (statTarget RF and BLAS are otherwise multi-threaded).
# Call once at the top of any timed script. Returns the previous settings so
# a multi-thread pass can restore them.
pin_single_thread <- function() {
  prev <- list(
    omp = Sys.getenv("OMP_NUM_THREADS", unset = NA_character_),
    dt  = if (requireNamespace("data.table", quietly = TRUE))
      data.table::getDTthreads() else NA_integer_
  )
  Sys.setenv(OMP_NUM_THREADS = "1")
  Sys.setenv(OPENBLAS_NUM_THREADS = "1")
  Sys.setenv(MKL_NUM_THREADS = "1")
  if (requireNamespace("data.table", quietly = TRUE)) {
    data.table::setDTthreads(1L)
  }
  if (requireNamespace("RhpcBLASctl", quietly = TRUE)) {
    try(RhpcBLASctl::blas_set_num_threads(1L), silent = TRUE)
    try(RhpcBLASctl::omp_set_num_threads(1L), silent = TRUE)
  }
  invisible(prev)
}

# ---- Warm + timed measurement ---------------------------------------------
# Measure a single expression: one untimed warm-up, then `iterations` timed
# runs. Returns a one-row data.frame of wall-clock (median/min) and memory.
# `expr` is captured unevaluated; pass it as a function arg, e.g.
#   measure_stage("combat", iterations = 5, expr = { bc_run_combat(...) })
measure_stage <- function(stage, iterations = 5L, warmup = TRUE, expr) {
  expr_q <- substitute(expr)
  env <- parent.frame()

  if (isTRUE(warmup)) {
    eval(expr_q, envir = env)  # untimed warm-up (package load, JIT, disk cache)
  }

  # peakRAM gives resident peak (MB); run it around the full timed block.
  use_peakram <- requireNamespace("peakRAM", quietly = TRUE)

  times <- numeric(iterations)
  peak_mb <- NA_real_
  measure_one <- function() {
    gc(verbose = FALSE)
    t0 <- proc.time()[["elapsed"]]
    eval(expr_q, envir = env)
    proc.time()[["elapsed"]] - t0
  }

  if (use_peakram) {
    pr <- peakRAM::peakRAM({
      for (i in seq_len(iterations)) times[i] <- measure_one()
    })
    peak_mb <- pr$Peak_RAM_Used_MiB[1]
  } else {
    for (i in seq_len(iterations)) times[i] <- measure_one()
  }

  data.frame(
    stage          = stage,
    engine         = "MStargetR",
    iterations     = iterations,
    wall_s_median  = stats::median(times),
    wall_s_min     = min(times),
    wall_s_sd      = stats::sd(times),
    peak_mb        = peak_mb,
    stringsAsFactors = FALSE
  )
}

# ---- Drive a MetaboAnalystR stage inside Docker ----------------------------
# Runs `ma_runner.R --stage=<stage>` in the MA container, capturing the
# in-container compute time (authoritative) and peak RAM that the runner
# prints as STAGE_TIME_S=/STAGE_PEAK_MB=. `mount_dir` is bind-mounted to
# /data; `input`/`out` are paths *relative to that mount* (forward slashes).
run_ma_stage <- function(stage, input, out, mount_dir,
                         cpus = 1, memory = "8g",
                         image = MA_DOCKER_IMAGE,
                         extra_args = character()) {
  mount <- normalizePath(mount_dir, winslash = "/", mustWork = TRUE)

  # system2("docker", ...) splits on spaces inside each args[] element, so a
  # mount path like "C:/path with spaces:/data" gets mangled into an invalid
  # reference (Docker error 125). Use system() with intern=TRUE instead, which
  # passes the full command through the OS shell and lets the shell handle
  # quoting correctly. The mount path is quoted with double quotes below.
  extra_str <- if (length(extra_args) > 0) paste(extra_args, collapse = " ") else ""
  cmd <- paste(
    "docker", "run", "--rm",
    sprintf("--cpus=%s", cpus),
    sprintf("--memory=%s", memory),
    sprintf('-v "%s:/data"', mount),
    image,
    "Rscript", "/data/ma_runner.R",
    sprintf("--stage=%s", stage),
    sprintf("--input=/data/%s", input),
    sprintf("--out=/data/%s", out),
    extra_str
  )

  host_t0 <- proc.time()[["elapsed"]]
  res <- suppressWarnings(system(cmd, intern = TRUE, ignore.stderr = FALSE))
  host_elapsed <- proc.time()[["elapsed"]] - host_t0
  status <- attr(res, "status")
  out_txt <- paste(res, collapse = "\n")

  grab <- function(key) {
    m <- regmatches(out_txt, regexpr(sprintf("%s=([0-9.eE+-]+)", key), out_txt))
    if (length(m) == 0) return(NA_real_)
    as.numeric(sub(sprintf("%s=", key), "", m))
  }

  data.frame(
    stage             = stage,
    engine            = "MetaboAnalyst",
    iterations        = 1L,
    wall_s_median     = grab("STAGE_TIME_S"),   # in-container compute time
    wall_s_min        = grab("STAGE_TIME_S"),
    wall_s_sd         = NA_real_,
    peak_mb           = grab("STAGE_PEAK_MB"),
    host_total_s      = host_elapsed,            # informational (incl. startup)
    docker_status     = if (is.null(status)) 0L else status,
    stringsAsFactors  = FALSE,
    log               = I(list(out_txt))
  )
}

# Repeat a Docker stage N times and return the median row (warm-aware:
# discards the first run as warm-up when reps > 1).
run_ma_stage_repeated <- function(stage, input, out, mount_dir, reps = 3L, ...) {
  rows <- lapply(seq_len(reps), function(i) {
    run_ma_stage(stage, input, out, mount_dir, ...)
  })
  df <- do.call(rbind, lapply(rows, function(r) r[setdiff(names(r), "log")]))
  timed <- if (reps > 1L) df[-1L, , drop = FALSE] else df
  out_row <- timed[1, , drop = FALSE]
  out_row$wall_s_median <- stats::median(timed$wall_s_median, na.rm = TRUE)
  out_row$wall_s_min    <- min(timed$wall_s_median, na.rm = TRUE)
  out_row$wall_s_sd     <- stats::sd(timed$wall_s_median)
  out_row$peak_mb       <- stats::median(timed$peak_mb, na.rm = TRUE)
  out_row
}
