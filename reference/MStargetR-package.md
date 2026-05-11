# MStargetR: Targeted MRM Mass Spectrometry Data Processing and Quality Control

MStargetR provides a comprehensive workflow for processing targeted
multiple reaction monitoring (MRM) mass spectrometry data from raw
vendor files through to concentration values ready for statistical
analysis.

## Details

The package includes tools for:

- Raw data conversion from vendor formats to mzML via Docker
  ([`msConvertR`](https://mstargetr.github.io/MStargetR/reference/msConvertR.md))

- Peak picking, retention time optimisation, and integration
  ([`PeakForgeR`](https://mstargetr.github.io/MStargetR/reference/PeakForgeR.md))

- Quality control assessment, batch correction, and reporting
  ([`qcCheckR`](https://mstargetr.github.io/MStargetR/reference/qcCheckR.md))

- Standalone interbatch correction for external data
  ([`batchCorrectR`](https://mstargetr.github.io/MStargetR/reference/batchCorrectR.md))

- An interactive Shiny application for visual workflow management
  ([`launchMStargetR`](https://mstargetr.github.io/MStargetR/reference/launchMStargetR.md))

- Utility functions for transition list validation
  ([`transition_checkR`](https://mstargetr.github.io/MStargetR/reference/transition_checkR.md),
  [`compare_mrm_template_with_guide`](https://mstargetr.github.io/MStargetR/reference/compare_mrm_template_with_guide.md))

## Getting started

See
[`vignette("MStargetR-vignette")`](https://mstargetr.github.io/MStargetR/articles/MStargetR-vignette.md)
for a comprehensive guide to using the package. The typical workflow is:

1.  Set up a project directory with raw vendor files

2.  Convert files with
    [`msConvertR`](https://mstargetr.github.io/MStargetR/reference/msConvertR.md)

3.  Process peaks with
    [`PeakForgeR`](https://mstargetr.github.io/MStargetR/reference/PeakForgeR.md)

4.  Run quality control with
    [`qcCheckR`](https://mstargetr.github.io/MStargetR/reference/qcCheckR.md)

## Requirements

- R \>= 4.1.0

- Docker Desktop (for `msConvertR` and `PeakForgeR`)

- Bioconductor packages: mzR, ropls, statTarget

## See also

Useful links:

- <https://github.com/MStargetR/MStargetR>

- Report bugs at <https://github.com/MStargetR/MStargetR/issues>

## Author

**Maintainer**: Harrison Szemray <Hszemray@live.com.au>
([ORCID](https://orcid.org/0009-0008-5829-540X))

Authors:

- Harrison Szemray <Hszemray@live.com.au>
  ([ORCID](https://orcid.org/0009-0008-5829-540X))

- Vimalnath Nambiar <vimalnath.nambiar@murdoch.edu.au>
  ([ORCID](https://orcid.org/0000-0001-5384-6788))

- Nathan Lawler ([ORCID](https://orcid.org/0000-0001-9649-425X))

- Julien Wist ([ORCID](https://orcid.org/0000-0002-3416-2572))

- Samantha Lodge ([ORCID](https://orcid.org/0000-0001-9193-0462))

- Luke Whiley ([ORCID](https://orcid.org/0000-0002-9088-4799))
