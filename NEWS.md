## MStargetR 1.5.0

Released 2026-07-07 ([compare](https://github.com/MStargetR/MStargetR/compare/v1.4.0...v1.5.0)).

### Bug Fixes

- **ci:** resolve Bioconductor via bioconductor.org, not broken PPM mirror ([4aa7819](https://github.com/MStargetR/MStargetR/commit/4aa7819aa4e2b596646d97c88f3826e6fadf8798))
- **ci:** route Bioconductor through Posit PPM mirror for archived releases ([ad3f969](https://github.com/MStargetR/MStargetR/commit/ad3f969e591cfd4f98d18e36aa2ddc080bf668e7))
- **gui:** correct per-plot downloads, trim master_list RAM, keep logs in place ([31e10ef](https://github.com/MStargetR/MStargetR/commit/31e10ef77f9470a6b7f6f50b90f9f9d5072a9a85))
- **qcCheckR:** match user QC choice case-insensitively ([e4414b9](https://github.com/MStargetR/MStargetR/commit/e4414b9a4ffcab06f77d5976a3cfa82d4cf28c44))
- quiet diagnostics, cross-tab project dir, universal export, full R logging ([87c7bcd](https://github.com/MStargetR/MStargetR/commit/87c7bcd58d93a2c6f8e59ed3350c1f4459ae5218))
- **timestamps:** parse AcquiredTime slash formats as UTC, not session-local ([1189b19](https://github.com/MStargetR/MStargetR/commit/1189b1986056ec169866b98d6d2a3d47d17b4636))
- **workflow:** pass mrm_template_list as named list and refresh params ([edcc005](https://github.com/MStargetR/MStargetR/commit/edcc00553965461c0b973d985d053046c92cfed2))

### Features

- **shiny:** expose date_order selector in the QC tab ([2982756](https://github.com/MStargetR/MStargetR/commit/2982756ef1085a79b15bbedaf6b26ea780995768))

## MStargetR 1.4.0

Released 2026-06-12 ([compare](https://github.com/MStargetR/MStargetR/compare/v1.3.0...v1.4.0)).

### Bug Fixes

- **msConvertR:** auto-rename raw_data files with spaces before msconvert ([91f8a46](https://github.com/MStargetR/MStargetR/commit/91f8a469521dd986090d0907b8c8213c88c958de))
- **qcCheckR:** honour AM/PM seconds and ISO date-prefix hints in date detection ([f7c185a](https://github.com/MStargetR/MStargetR/commit/f7c185a2ba1b1ddfc2cce703a964034605a74f78))

### Features

- **msConvertR:** auto-discover plate grouping from filenames ([e061d75](https://github.com/MStargetR/MStargetR/commit/e061d754ce7cfa0d9272b165205b2931013f9846))
- **msConvertR:** group one-file-per-sample vendors into per-plate mzML ([561ef5d](https://github.com/MStargetR/MStargetR/commit/561ef5debfb5a5042570b6e3f0380b31ba523d3a))
- **qcCheckR:** make instrumental LOD threshold configurable ([4257c2b](https://github.com/MStargetR/MStargetR/commit/4257c2b30a21e1764ef12c2e183e930f0ba7509f))

## MStargetR 1.1.0

Released 2026-06-11 ([compare](https://github.com/MStargetR/MStargetR/compare/v1.0.0...v1.1.0)).

### Bug Fixes

- **ci:** resolve R CMD check WARNINGs blocking build on release/oldrel ([3b5c778](https://github.com/MStargetR/MStargetR/commit/3b5c778427957714c56e018d4a88fd68134f12d9))
- **deps:** promote GUI runtime packages to Imports so they install by default ([9e6cd99](https://github.com/MStargetR/MStargetR/commit/9e6cd992c6b1bc9a926293f8ba400c1e1b0a71e6))
- **docker:** allow Wine syscalls via seccomp=unconfined (DOCK-C6) ([4d0ba57](https://github.com/MStargetR/MStargetR/commit/4d0ba57f1842ab730df4f9014eeddf097ddf9ce2))
- **docs:** correct vignette URL in README to articles/ pkgdown path ([f81990c](https://github.com/MStargetR/MStargetR/commit/f81990c20ae84127268031dc5c88bca807c97c22))
- **docs:** repair Rd cross-reference and vignette logo path for clean check + pkgdown ([f6e1c63](https://github.com/MStargetR/MStargetR/commit/f6e1c6372da7b125cf1a521bb661431b5e16ea03))
- **plots:** avoid cairo_pdf on macOS in save_figure to prevent R segfault ([e560257](https://github.com/MStargetR/MStargetR/commit/e5602576598007b32756d8f36ff393f918c5dd6f))
- **plots:** use exact-match indexing for pca$scores guard; clean up test warnings ([8af6362](https://github.com/MStargetR/MStargetR/commit/8af6362f02540ea9bf42c386b223c968a8bbf33c))
- **qcCheckR:** use mzML startTimeStamp for sample timestamps; resolve dmy/mdy at cohort level ([2746c81](https://github.com/MStargetR/MStargetR/commit/2746c814d5ea13c696b4b6aa021c09392b9ed82b))
- **reports:** stream-embed resources to avoid pandoc OOM on large cohorts ([a082cb8](https://github.com/MStargetR/MStargetR/commit/a082cb8e8c79a234a1c7bcfbe4ed2ba32dc53705))
- **reports:** unblock 54-plate cohort report+RDA exports on Windows ([db9ce2d](https://github.com/MStargetR/MStargetR/commit/db9ce2d849271e2624db2af551563cf0ea050cec))
- **shiny:** drop forced grayscale font-smoothing that blurs UI on Windows ([7ff1e4a](https://github.com/MStargetR/MStargetR/commit/7ff1e4a9a7398e1568013bb2ff10cea94a9d07a4))
- **shiny:** unbreak SH-013 audit by rewording comment ([5886552](https://github.com/MStargetR/MStargetR/commit/5886552737ecbe5e3ae2bac405045e781491907f))

### Features

- **batch-correction:** make combat_ref.batch usable in the GUI ([ebfa586](https://github.com/MStargetR/MStargetR/commit/ebfa5867dd9c65632b741a013ce421c02ea6f887))
- **batch:** add QC-RLSC robust LOESS signal correction method ([62c7120](https://github.com/MStargetR/MStargetR/commit/62c7120266609f54780e607114729e328a763971))
- **container:** add Apptainer/HPC support via enable_HPC dispatcher ([6bb0675](https://github.com/MStargetR/MStargetR/commit/6bb06750ac793d12d942df199e154516f09a998c))
- **msConvertR:** auto-discover plate grouping from filenames ([e061d75](https://github.com/MStargetR/MStargetR/commit/e061d754ce7cfa0d9272b165205b2931013f9846))
- **msConvertR:** group one-file-per-sample vendors into per-plate mzML ([561ef5d](https://github.com/MStargetR/MStargetR/commit/561ef5debfb5a5042570b6e3f0380b31ba523d3a))
- **plots:** expose every GUI plot to R users via advanced_plots = TRUE ([838e34e](https://github.com/MStargetR/MStargetR/commit/838e34e2dada817709a41b4282001c9e5f9b984e))
- QC-RLSC batch correction, HPC/Apptainer support, and R-side plot parity ([005cc1f](https://github.com/MStargetR/MStargetR/commit/005cc1f2e7fa4ba6a89adad26b44ea8257535d1d))
- **qcCheckR:** expose batch_column to mirror batchCorrectR + GUI ([7c2278a](https://github.com/MStargetR/MStargetR/commit/7c2278ab5d31fc64f81084ca700a9a8d52b84924))
- **qcCheckR:** make instrumental LOD threshold configurable ([4257c2b](https://github.com/MStargetR/MStargetR/commit/4257c2b30a21e1764ef12c2e183e930f0ba7509f))

### Reverts

- Revert "chore(benchmark): add MStargetR vs MetaboAnalyst 6.0 benchmark suite" ([af1e142](https://github.com/MStargetR/MStargetR/commit/af1e14242c45315b7402767eef9abc6beeed0e25))

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
- **qcCheckR:** the instrumental limit of detection (LOD) used for below-LOD
  flagging is now configurable via a new `lod_threshold` argument (default
  `5000`, preserving previous behaviour), exposed both in `qcCheckR()` and the
  Shiny QC Check tab. The LOD is instrument- and lab-specific, so labs can set
  it to their instrument's detection floor instead of the hardcoded `5000`.
  **Breaking:** the QC missing-value summary column `peakArea_5000_LOD` is
  renamed to `peakArea_below_LOD` (threshold-independent); update any code that
  reads this column by name.

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
