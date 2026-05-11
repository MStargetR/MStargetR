# Changelog

## MStargetR 1.1.2

Released 2026-05-11
([compare](https://github.com/MStargetR/MStargetR/compare/v1.1.1...v1.1.2)).

### Bug Fixes

- **ci:** resolve R CMD check WARNINGs blocking build on release/oldrel
  ([40df0a3](https://github.com/MStargetR/MStargetR/commit/40df0a35b3dc32ed9cd43a2ae8eaaa4466a0c23d))
- **deps:** promote GUI runtime packages to Imports so they install by
  default
  ([7cbe7f4](https://github.com/MStargetR/MStargetR/commit/7cbe7f4b24a5e779a95023251e2d519d0703955b))
- **qcCheckR:** use mzML startTimeStamp for sample timestamps; resolve
  dmy/mdy at cohort level
  ([9ec1dc7](https://github.com/MStargetR/MStargetR/commit/9ec1dc7c27e853c012f4d67378f58e33aa62a987))
- **reports:** stream-embed resources to avoid pandoc OOM on large
  cohorts
  ([0dedae8](https://github.com/MStargetR/MStargetR/commit/0dedae8304c31f35fd59adddc4c2d9dc33ceab06))
- **reports:** unblock 54-plate cohort report+RDA exports on Windows
  ([9f1c6eb](https://github.com/MStargetR/MStargetR/commit/9f1c6ebab6b8bc0491c7a3b6e8ccf498e931c052))

## MStargetR 1.1.1

Released 2026-05-11
([compare](https://github.com/MStargetR/MStargetR/compare/v1.1.0...v1.1.1)).

### Bug Fixes

- **docker:** allow Wine syscalls via seccomp=unconfined (DOCK-C6)
  ([e054e65](https://github.com/MStargetR/MStargetR/commit/e054e6524a17b650d95f4f11700cd01ec18d4016))

## MStargetR 1.1.0

Released 2026-05-06
([compare](https://github.com/MStargetR/MStargetR/compare/v1.0.0...v1.1.0)).

### Bug Fixes

- **shiny:** unbreak SH-013 audit by rewording comment
  ([24665ce](https://github.com/MStargetR/MStargetR/commit/24665ceac5b1e344d6381230a9f398fc4e9cf0bf))

### Features

- **batch-correction:** make combat_ref.batch usable in the GUI
  ([d5e6f67](https://github.com/MStargetR/MStargetR/commit/d5e6f6702faf0f7e76fd4c5e40dd5ee673439941))
- **qcCheckR:** expose batch_column to mirror batchCorrectR + GUI
  ([8eaf3e9](https://github.com/MStargetR/MStargetR/commit/8eaf3e9d5f53d1fc75687d5c772990c0a40ec7e0))

## MStargetR 1.0.0

Initial public release.

### Features

- Comprehensive workflow for targeted MRM mass spectrometry data
  processing
- [`PeakForgeR()`](https://mstargetr.github.io/MStargetR/reference/PeakForgeR.md)
  for automated Skyline document creation and peak integration via
  Docker
- [`msConvertR()`](https://mstargetr.github.io/MStargetR/reference/msConvertR.md)
  for vendor file conversion to mzML with parallel processing via Docker
- [`qcCheckR()`](https://mstargetr.github.io/MStargetR/reference/qcCheckR.md)
  for quality control assessment with configurable QC types and ANPC
  filtering
- [`batchCorrectR()`](https://mstargetr.github.io/MStargetR/reference/batchCorrectR.md)
  for standalone interbatch correction with QCRFSC and ComBat methods
- [`launchMStargetR()`](https://mstargetr.github.io/MStargetR/reference/launchMStargetR.md)
  Shiny GUI for interactive pipeline execution
- Interactive HTML report generation with plotly visualisations
- Cross-platform support (Windows, macOS, Linux) with Docker integration
- Comprehensive test suite and input validation
