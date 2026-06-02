# Run a containerised command via Docker or Apptainer.

Sole entry point used by
[`msConvertR()`](https://mstargetr.github.io/MStargetR/reference/msConvertR.md)
and
[`PeakForgeR()`](https://mstargetr.github.io/MStargetR/reference/PeakForgeR.md)
so neither function has to know which runtime is active.

## Usage

``` r
run_container(
  image_command,
  binds = list(),
  enable_HPC = getOption("MStargetR.enable_HPC", FALSE),
  docker_extra_args = NULL,
  apptainer_extra_args = NULL,
  stdout = TRUE,
  stderr = TRUE
)
```

## Arguments

- image_command:

  Character vector. The command to execute *inside* the container, e.g.
  `c("wine", "msconvert", "-o", "/output", ...)`.

- binds:

  List of bind specifications. Each element is itself a list with
  `host`, `container`, and optional `ro` (logical).

- enable_HPC:

  Logical. `FALSE` -\> Docker. `TRUE` -\> Apptainer.

- docker_extra_args:

  Optional character vector inserted between the standard Docker
  hardening flags and the bind mounts. Ignored for Apptainer.

- apptainer_extra_args:

  Optional character vector inserted before the bind mounts in the
  Apptainer invocation. Ignored for Docker.

- stdout, stderr:

  Passed through to [`system2()`](https://rdrr.io/r/base/system2.html).
  Defaults to `TRUE` (capture as character vector).

## Value

The value returned by [`system2()`](https://rdrr.io/r/base/system2.html)
(typically a character vector with `attr(., "status")`).

## Details

For Docker, the standard hardening flags applied to
msConvertR/PeakForgeR today are always emitted:
`--rm --cap-drop=ALL --network=none --security-opt seccomp=unconfined`.
Callers may pass additional Docker-only flags via `docker_extra_args`
(e.g. msConvertR passes `--user=1000:1000` on POSIX hosts).

Apptainer runs as the invoking user with no network namespace by
default, so the Docker hardening flags are omitted; Apptainer-only
extras can be passed via `apptainer_extra_args`.
