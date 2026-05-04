# Changelog

## MStargetR 1.0.0

Released 2026-05-04.

### Bug Fixes

- **ci:** apply prettier to .mjs and widen lint-staged glob
  ([68c2cb0](https://github.com/MStargetR/MStargetR/commit/68c2cb026031a11aa0aada1d6fac83a5eb440fc4))
- **ci:** clear pkgdown + R CMD check failures
  ([0433c1f](https://github.com/MStargetR/MStargetR/commit/0433c1f5321094a89a2ea3c0d0c03eb70c1a2148))
- **ci:** repair regressions from previous CI fix attempt
  ([d3caa55](https://github.com/MStargetR/MStargetR/commit/d3caa553057be53e8365f7447d43dd24b845a1b3))
- **test:** skip source-reading audit tests under installed-package runs
  ([eda5ca9](https://github.com/MStargetR/MStargetR/commit/eda5ca949efc4e833a246264975a0730a9ac9570))

### Performance Improvements

- **qcCheckR:** write RDA in detached background subprocess
  ([6d7968c](https://github.com/MStargetR/MStargetR/commit/6d7968c250e6c7678a43b1330202142b2498c3c3))

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
