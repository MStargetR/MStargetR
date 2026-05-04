# Changelog

## MStargetR 1.0.1

Released
[2026-04-23](https://github.com/MStargetR/MStargetR/compare/v1.0.0...v1.0.1).

### Bug Fixes

- **batch:** resolve QCRFSC ‘subscript out of bounds’ and add
  sample_tags input
  ([c51707b](https://github.com/MStargetR/MStargetR/commit/c51707b6759acb201d2779456c23decfe7f9b93f))

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
