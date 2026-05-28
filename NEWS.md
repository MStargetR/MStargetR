## MStargetR 1.3.0

Released 2026-05-28 ([compare](https://github.com/MStargetR/MStargetR/compare/v1.2.0...v1.3.0)).

### Bug Fixes

- **docs:** correct vignette URL in README to articles/ pkgdown path ([b7364f5](https://github.com/MStargetR/MStargetR/commit/b7364f5744f8bb58f2674af46e5436d91b90a702))
- **docs:** repair Rd cross-reference and vignette logo path for clean check + pkgdown ([d323e30](https://github.com/MStargetR/MStargetR/commit/d323e309429c272934bff7cf4df03991a2f4b531))
- **plots:** avoid cairo_pdf on macOS in save_figure to prevent R segfault ([814dced](https://github.com/MStargetR/MStargetR/commit/814dceda3ee941f378bddf554bbcd260bf2075f4))
- **plots:** use exact-match indexing for pca$scores guard; clean up test warnings ([eb752a0](https://github.com/MStargetR/MStargetR/commit/eb752a023ce313ec67afd1ead2357639d44c1528))
- **shiny:** drop forced grayscale font-smoothing that blurs UI on Windows ([c2b9bce](https://github.com/MStargetR/MStargetR/commit/c2b9bce2f06653f3fb7a8098b249e8c1056590d2))

### Features

- **batch:** add QC-RLSC robust LOESS signal correction method ([d8babf7](https://github.com/MStargetR/MStargetR/commit/d8babf7558b1d92baf4f4a302fd7fb9a4d893c8f))
- **container:** add Apptainer/HPC support via enable_HPC dispatcher ([1eb90b4](https://github.com/MStargetR/MStargetR/commit/1eb90b4ad582cf693f436c4652d71d2c5fdaa70a))
- **plots:** expose every GUI plot to R users via advanced_plots = TRUE ([e4a0807](https://github.com/MStargetR/MStargetR/commit/e4a080793a95a79ed735f8efccf8b9b4ac444253))

## MStargetR 1.2.0

Released 2026-05-27 ([compare](https://github.com/MStargetR/MStargetR/compare/v1.1.2...v1.2.0)).

### Features

- QC-RLSC batch correction, HPC/Apptainer support, and R-side plot parity ([791cad3](https://github.com/MStargetR/MStargetR/commit/791cad30896af38cd44b0c6f297c0099c7d14355))

## Unreleased

### Features

- **batch correction:** add a third user-selectable method, **QC-RLSC**
  (Quality Control-based Robust LOESS Signal Correction; Dunn et al. 2011,
  <doi:10.1038/nprot.2011.335>) via the CRAN package `qcrlscR`. Selectable as
  `method = "QCRLSC"` in `batchCorrectR()` and `batch_method = "QCRLSC"` in
  `qcCheckR()`, and from both Shiny batch-correction tabs. Like QCRFSC it
  requires QC samples (the LOESS trend is fit through them). New `qcrlsc_*`
  arguments expose the scaling mode (`"subtract"` default / `"divide"`),
  intra/inter-batch correction, LOESS span GCV optimisation, log10 transform,
  QC outlier detection, and `batch.shift`. `qcrlscR` is an optional dependency
  (Suggests), loaded behind a `requireNamespace()` guard, so it errors with an
  install hint only when the method is actually selected.
- **plots:** R-side parity with the GUI for every Quality Check, Batch
  Correction, and Results Explorer figure. `qcCheckR()` and
  `batchCorrectR()` gain an opt-in `advanced_plots = FALSE` argument and
  a new exported `resultsExplorerR()` function wraps the Results Explorer
  tab as a callable API. When `advanced_plots = TRUE` (and a
  `project_dir` is supplied), every plot the GUI renders is written to
  `<project_dir>/all/figures/{qcCheckR|batch_corrector|results_explorer}/`
  as both a static `.pdf` (via `ggplot2::ggsave`) and an interactive
  `.html` (via `htmlwidgets::saveWidget`). The Shiny app's signal-drift
  and run-order helpers now delegate to the same package-internal
  constructors, so GUI and R users always see the byte-identical figure.
  Default behaviour is unchanged -- existing scripts that don't set
  `advanced_plots` write nothing extra.
- **batchCorrectR:** soft-deprecate the `plot` argument in favour of
  `advanced_plots`. `plot = TRUE` still populates `result$plots` for
  back-compat; passing it explicitly now emits a one-time deprecation
  warning suggesting `advanced_plots = TRUE` (which both attaches the
  extended GUI plot set to `result$plots` and saves it to disk).
- **container:** add Apptainer/HPC support for `msConvertR()` and
  `PeakForgeR()`. Both functions accept a new `enable_HPC` argument
  (default `FALSE`); when `TRUE` the ProteoWizard container is invoked
  via Apptainer/Singularity instead of Docker, with the SIF resolved
  from `getOption("MStargetR.sif_path")`, the user cache
  (`tools::R_user_dir("MStargetR", "cache")`), or pulled on demand from
  the same pinned image tag. HPC users can set
  `options(MStargetR.enable_HPC = TRUE)` once in `.Rprofile` and never
  pass the argument explicitly. Docker remains the default and
  workstation behaviour is unchanged. See the "Running on HPC" sections
  of the README and vignette for SIF setup.

## MStargetR 1.1.2

Released 2026-05-11 ([compare](https://github.com/MStargetR/MStargetR/compare/v1.1.1...v1.1.2)).

### Bug Fixes

- **ci:** resolve R CMD check WARNINGs blocking build on release/oldrel ([40df0a3](https://github.com/MStargetR/MStargetR/commit/40df0a35b3dc32ed9cd43a2ae8eaaa4466a0c23d))
- **deps:** promote GUI runtime packages to Imports so they install by default ([7cbe7f4](https://github.com/MStargetR/MStargetR/commit/7cbe7f4b24a5e779a95023251e2d519d0703955b))
- **qcCheckR:** use mzML startTimeStamp for sample timestamps; resolve dmy/mdy at cohort level ([9ec1dc7](https://github.com/MStargetR/MStargetR/commit/9ec1dc7c27e853c012f4d67378f58e33aa62a987))
- **reports:** stream-embed resources to avoid pandoc OOM on large cohorts ([0dedae8](https://github.com/MStargetR/MStargetR/commit/0dedae8304c31f35fd59adddc4c2d9dc33ceab06))
- **reports:** unblock 54-plate cohort report+RDA exports on Windows ([9f1c6eb](https://github.com/MStargetR/MStargetR/commit/9f1c6ebab6b8bc0491c7a3b6e8ccf498e931c052))

## MStargetR 1.1.1

Released 2026-05-11 ([compare](https://github.com/MStargetR/MStargetR/compare/v1.1.0...v1.1.1)).

### Bug Fixes

- **docker:** allow Wine syscalls via seccomp=unconfined (DOCK-C6) ([e054e65](https://github.com/MStargetR/MStargetR/commit/e054e6524a17b650d95f4f11700cd01ec18d4016))

## MStargetR 1.1.0

Released 2026-05-06 ([compare](https://github.com/MStargetR/MStargetR/compare/v1.0.0...v1.1.0)).

### Bug Fixes

- **shiny:** unbreak SH-013 audit by rewording comment ([24665ce](https://github.com/MStargetR/MStargetR/commit/24665ceac5b1e344d6381230a9f398fc4e9cf0bf))

### Features

- **batch-correction:** make combat_ref.batch usable in the GUI ([d5e6f67](https://github.com/MStargetR/MStargetR/commit/d5e6f6702faf0f7e76fd4c5e40dd5ee673439941))
- **qcCheckR:** expose batch_column to mirror batchCorrectR + GUI ([8eaf3e9](https://github.com/MStargetR/MStargetR/commit/8eaf3e9d5f53d1fc75687d5c772990c0a40ec7e0))

## MStargetR 1.0.0

Initial public release.

### Features

- Comprehensive workflow for targeted MRM mass spectrometry data processing
- `PeakForgeR()` for automated Skyline document creation and peak integration via Docker
- `msConvertR()` for vendor file conversion to mzML with parallel processing via Docker
- `qcCheckR()` for quality control assessment with configurable QC types and ANPC filtering
- `batchCorrectR()` for standalone interbatch correction with QCRFSC and ComBat methods
- `launchMStargetR()` Shiny GUI for interactive pipeline execution
- Interactive HTML report generation with plotly visualisations
- Cross-platform support (Windows, macOS, Linux) with Docker integration
- Comprehensive test suite and input validation
