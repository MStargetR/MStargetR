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
