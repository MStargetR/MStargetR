# container_runtime.R
#
# Runtime-agnostic wrapper around Docker and Apptainer/Singularity for the
# ProteoWizard image used by msConvertR() and PeakForgeR().
#
# The two user-facing functions toggle between runtimes via `enable_HPC`:
#
#   * enable_HPC = FALSE (default)  -> docker run ...
#   * enable_HPC = TRUE             -> apptainer exec ... <sif>
#
# Apptainer is used on HPC clusters where Docker is typically forbidden.
# Apptainer's `pull docker://...` builds a SIF directly from any Docker image,
# so there is no separate .def file and no MStargetR-owned image to maintain.
#
# All functions in this file are internal (@keywords internal, not exported).

#' Full ProteoWizard image reference (`name:tag`).
#'
#' Single source of truth for the image string used by Docker and as the source
#' for `apptainer pull docker://...`. Reads the tag resolved at package load
#' (see `MSTARGETR_DOCKER_IMAGE_TAG` in `R/config.R`).
#' @keywords internal
mstargetr_image_ref <- function() {
  paste0("proteowizard/pwiz-skyline-i-agree-to-the-vendor-licenses:",
         MSTARGETR_DOCKER_IMAGE_TAG)
}

#' Locate the Apptainer/Singularity executable, accepting either name.
#'
#' Returns the executable name (`"apptainer"` or `"singularity"`) suitable for
#' passing to `system2()`. Returns `""` if neither is on `PATH`.
#' @keywords internal
mstargetr_find_apptainer <- function() {
  for (bin in c("apptainer", "singularity")) {
    if (nzchar(Sys.which(bin))) return(bin)
  }
  ""
}

#' Assert that the requested container runtime is available.
#'
#' @param runtime One of `"docker"` or `"apptainer"`. The latter also accepts
#'   `singularity` as a fallback.
#' @return Invisibly, the executable name (`"docker"`, `"apptainer"`, or
#'   `"singularity"`). Stops with an actionable error if not found.
#' @keywords internal
assert_runtime_available <- function(runtime) {
  if (identical(runtime, "docker")) {
    if (!nzchar(Sys.which("docker"))) {
      stop("docker not found on PATH. Install Docker Desktop ",
           "(https://www.docker.com/get-started/) or set enable_HPC = TRUE ",
           "to use Apptainer instead.", call. = FALSE)
    }
    return(invisible("docker"))
  }
  if (identical(runtime, "apptainer")) {
    bin <- mstargetr_find_apptainer()
    if (!nzchar(bin)) {
      stop("apptainer (or singularity) not found on PATH. ",
           "Load the HPC module (e.g. `module load apptainer`) ",
           "or set enable_HPC = FALSE to use Docker.", call. = FALSE)
    }
    return(invisible(bin))
  }
  stop("assert_runtime_available: unknown runtime '", runtime,
       "'. Expected 'docker' or 'apptainer'.", call. = FALSE)
}

#' Resolve the SIF file for the current image tag.
#'
#' Lookup order:
#' \enumerate{
#'   \item `getOption("MStargetR.sif_path")`, if set and the file exists.
#'   \item `tools::R_user_dir("MStargetR", "cache")/mstargetr-pwiz-<tag>.sif`,
#'         if it exists.
#'   \item `apptainer pull docker://<image>:<tag>` into the cache, writing
#'         `mstargetr-pwiz-<tag>.sif`.
#' }
#'
#' The filename encodes the image tag so a tag bump triggers a one-time
#' re-pull and earlier SIFs remain on disk for reproducing prior analyses.
#'
#' If the auto-pull fails (the most likely cause on an HPC compute node is
#' no outbound network), the error directs the user to pull the SIF on a
#' login node and set `options(MStargetR.sif_path = "...")`.
#'
#' @return Absolute path to a SIF file.
#' @keywords internal
resolve_sif <- function() {
  user_sif <- getOption("MStargetR.sif_path", default = NULL)
  if (!is.null(user_sif) && nzchar(user_sif)) {
    if (!file.exists(user_sif)) {
      stop("resolve_sif: 'MStargetR.sif_path' is set but the file does not exist: '",
           user_sif, "'. Either pull a SIF and update the option, or unset it ",
           "to fall back to the package cache.", call. = FALSE)
    }
    return(normalizePath(user_sif, winslash = "/", mustWork = TRUE))
  }

  cache_dir <- tools::R_user_dir("MStargetR", which = "cache")
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  }
  sif_name <- paste0("mstargetr-pwiz-", MSTARGETR_DOCKER_IMAGE_TAG, ".sif")
  sif_path <- file.path(cache_dir, sif_name)
  if (file.exists(sif_path)) {
    return(normalizePath(sif_path, winslash = "/", mustWork = TRUE))
  }

  bin <- assert_runtime_available("apptainer")
  message("Pulling ", mstargetr_image_ref(), " into ", sif_path,
          " (one-time, ~GB)...")
  pull_args <- c("pull", "--dir", cache_dir, sif_name,
                 paste0("docker://", mstargetr_image_ref()))
  pull_output <- suppressWarnings(
    system2(bin, args = pull_args, stdout = TRUE, stderr = TRUE)
  )
  pull_exit <- attr(pull_output, "status")
  if ((!is.null(pull_exit) && pull_exit != 0L) || !file.exists(sif_path)) {
    stop("resolve_sif: failed to pull SIF for ", mstargetr_image_ref(), ".\n",
         "Most HPC compute nodes have no outbound network. To work around this:\n",
         "  1. On a login node, run:\n",
         "       ", bin, " pull --dir <cache> ", sif_name,
         " docker://", mstargetr_image_ref(), "\n",
         "  2. In R, point MStargetR at the SIF:\n",
         "       options(MStargetR.sif_path = \"/path/to/", sif_name, "\")\n",
         "Pull output:\n", paste(pull_output, collapse = "\n"),
         call. = FALSE)
  }
  message("SIF ready: ", sif_path)
  normalizePath(sif_path, winslash = "/", mustWork = TRUE)
}

#' Build the `--bind` / `-v` argument list for a list of binds.
#'
#' Each bind entry is a list with elements `host`, `container`, and optional
#' `ro` (logical, default `FALSE`). The flag differs between runtimes:
#' Docker uses `-v host:container[:ro]`, Apptainer uses
#' `--bind host:container[:ro]`.
#' @keywords internal
mstargetr_bind_args <- function(binds, runtime) {
  if (is.null(binds) || length(binds) == 0L) return(character(0))
  flag <- if (identical(runtime, "docker")) "-v" else "--bind"
  out <- character(0)
  for (b in binds) {
    if (!is.list(b) || is.null(b$host) || is.null(b$container)) {
      stop("mstargetr_bind_args: each bind must be a list with 'host' and ",
           "'container'.", call. = FALSE)
    }
    spec <- paste0(b$host, ":", b$container)
    if (isTRUE(b$ro)) spec <- paste0(spec, ":ro")
    out <- c(out, flag, spec)
  }
  out
}

#' Run a containerised command via Docker or Apptainer.
#'
#' Sole entry point used by `msConvertR()` and `PeakForgeR()` so neither
#' function has to know which runtime is active.
#'
#' For Docker, the standard hardening flags applied to msConvertR/PeakForgeR
#' today are always emitted: `--rm --cap-drop=ALL --network=none
#' --security-opt seccomp=unconfined`. Callers may pass additional Docker-only
#' flags via `docker_extra_args` (e.g. msConvertR passes `--user=1000:1000`
#' on POSIX hosts).
#'
#' Apptainer runs as the invoking user with no network namespace by default,
#' so the Docker hardening flags are omitted; Apptainer-only extras can be
#' passed via `apptainer_extra_args`.
#'
#' @param image_command Character vector. The command to execute *inside* the
#'   container, e.g. `c("wine", "msconvert", "-o", "/output", ...)`.
#' @param binds List of bind specifications. Each element is itself a list
#'   with `host`, `container`, and optional `ro` (logical).
#' @param enable_HPC Logical. `FALSE` -> Docker. `TRUE` -> Apptainer.
#' @param docker_extra_args Optional character vector inserted between the
#'   standard Docker hardening flags and the bind mounts. Ignored for
#'   Apptainer.
#' @param apptainer_extra_args Optional character vector inserted before the
#'   bind mounts in the Apptainer invocation. Ignored for Docker.
#' @param stdout,stderr Passed through to `system2()`. Defaults to `TRUE`
#'   (capture as character vector).
#' @return The value returned by `system2()` (typically a character vector
#'   with `attr(., "status")`).
#' @keywords internal
run_container <- function(image_command,
                          binds = list(),
                          enable_HPC = getOption("MStargetR.enable_HPC", FALSE),
                          docker_extra_args = NULL,
                          apptainer_extra_args = NULL,
                          stdout = TRUE,
                          stderr = TRUE) {
  if (!is.character(image_command) || length(image_command) == 0L) {
    stop("run_container: 'image_command' must be a non-empty character vector.",
         call. = FALSE)
  }
  if (!is.logical(enable_HPC) || length(enable_HPC) != 1L || is.na(enable_HPC)) {
    stop("run_container: 'enable_HPC' must be a single logical (TRUE/FALSE).",
         call. = FALSE)
  }

  if (isTRUE(enable_HPC)) {
    bin <- assert_runtime_available("apptainer")
    sif <- resolve_sif()
    argv <- c("exec",
              if (length(apptainer_extra_args)) apptainer_extra_args,
              mstargetr_bind_args(binds, runtime = "apptainer"),
              sif,
              image_command)
  } else {
    assert_runtime_available("docker")
    bin <- "docker"
    argv <- c("run", "--rm",
              "--network=none",
              "--cap-drop=ALL",
              "--security-opt", "seccomp=unconfined",
              if (length(docker_extra_args)) docker_extra_args,
              mstargetr_bind_args(binds, runtime = "docker"),
              mstargetr_image_ref(),
              image_command)
  }

  if (isTRUE(getOption("MStargetR.verbose", FALSE))) {
    message("run_container: ", bin, " ", paste(argv, collapse = " "))
  }

  system2(bin, args = argv, stdout = stdout, stderr = stderr)
}
