# MStargetR

[![license](https://img.shields.io/github/license/MStargetR/MStargetR.svg?color=%23A31F34)](https://mstargetr.github.io/MStargetR/LICENSE)
[![semantic-release](https://img.shields.io/badge/semantic--release-angular-E10079.svg?logo=semantic-release&logoColor=%23E10079)](https://github.com/semantic-release/semantic-release)
[![doi-zenodo](https://img.shields.io/badge/zenodo-10.5281/zenodo.18476537-blue.svg?logo=doi&logoColor=blue)](https://doi.org/10.5281/zenodo.18476537)
[![codecov](https://codecov.io/gh/MStargetR/MStargetR/branch/main/graph/badge.svg)](https://codecov.io/gh/MStargetR/MStargetR)
\## Overview

MStargetR provides a complete, end-to-end pipeline for processing
targeted multiple reaction monitoring (MRM) liquid chromatography–mass
spectrometry (LC-MS) data. It takes raw vendor files as input and
produces quality-controlled, batch-corrected concentration data ready
for downstream statistical analysis. The package is designed for
metabolomics researchers who need a reproducible, automated workflow
that handles multi-plate and multi-method experiments.

Cross-platform support is achieved through Docker, which encapsulates
the vendor file conversion and peak integration steps so that the same
workflow runs on Windows, macOS, and Linux. An interactive Shiny
application is also included for users who prefer a graphical interface
over scripting.

The core pipeline consists of three stages: (1) conversion of
proprietary vendor files to open mzML format via ProteoWizard, (2)
retention time optimisation and peak picking/integration via Skyline,
and (3) quality control assessment with batch and signal drift
correction. Each stage is exposed as an independent function that can be
used on its own or combined into a single script.

![MStargetR pipeline architecture: vendor raw files pass through
msConvertR, PeakForgeR and qcCheckR to produce HTML, Excel and .qs2
outputs](reference/figures/architecture.png)

## Key Features

- **Vendor file conversion** –
  [`msConvertR()`](https://mstargetr.github.io/MStargetR/reference/msConvertR.md)
  converts proprietary instrument files (SCIEX .wiff, Bruker .d, Thermo
  .raw, Waters .raw, and others) to open mzML format using ProteoWizard
  msConvert through Docker.
- **Peak picking and integration** –
  [`PeakForgeR()`](https://mstargetr.github.io/MStargetR/reference/PeakForgeR.md)
  performs QC-based retention time optimisation, peak boundary
  refinement, and peak integration via Skyline in Docker. Supports
  multi-method and multi-plate projects.
- **Quality control and reporting** –
  [`qcCheckR()`](https://mstargetr.github.io/MStargetR/reference/qcCheckR.md)
  performs missing value imputation, batch and signal drift correction,
  sample and feature filtering, and generates HTML and Excel reports
  with interactive PCA and control chart visualisations. The
  instrumental limit of detection used for below-LOD flagging is
  configurable via `lod_threshold` (default `5000`) to match each lab’s
  instrument.
- **Standalone batch correction** –
  [`batchCorrectR()`](https://mstargetr.github.io/MStargetR/reference/batchCorrectR.md)
  provides independent QC-based signal drift and interbatch correction
  using random forest (QCRFSC) or empirical Bayes (ComBat) methods. It
  accepts a simple data.frame and can be used outside the main MStargetR
  pipeline.
- **Interactive Shiny application** –
  [`launchMStargetR()`](https://mstargetr.github.io/MStargetR/reference/launchMStargetR.md)
  launches a GUI for running the full pipeline without writing code.
- **Transition list validation** –
  [`transition_checkR()`](https://mstargetr.github.io/MStargetR/reference/transition_checkR.md)
  verifies that all MRM transitions are unique, and
  [`compare_mrm_template_with_guide()`](https://mstargetr.github.io/MStargetR/reference/compare_mrm_template_with_guide.md)
  confirms that internal standards in the transition list match the
  concentration guide.
- **Multi-method and multi-plate support** – All pipeline functions
  handle projects spanning multiple analytical methods and instrument
  plates, combining results where common long-term reference samples and
  consistent metabolite naming conventions are used.

## Requirements

- **R** \>= 4.1.0. Download and install from
  <https://cran.r-project.org/>.
- **Container runtime** for
  [`msConvertR()`](https://mstargetr.github.io/MStargetR/reference/msConvertR.md)
  and
  [`PeakForgeR()`](https://mstargetr.github.io/MStargetR/reference/PeakForgeR.md):
  - **Docker Desktop** on workstations (default). Download from
    <https://www.docker.com/get-started/>. Ensure Docker is running in
    the background before calling these functions.
  - **Apptainer / Singularity** on HPC clusters (set
    `enable_HPC = TRUE`, see “Running on HPC” below).
- **Bioconductor dependencies** – the packages `mzR`, `ropls`, and
  `statTarget` are installed automatically when using the helper
  installation function below. `sva` is additionally required for ComBat
  correction (`batch_method = "ComBat"` in
  [`qcCheckR()`](https://mstargetr.github.io/MStargetR/reference/qcCheckR.md),
  or `method = "ComBat"` in
  [`batchCorrectR()`](https://mstargetr.github.io/MStargetR/reference/batchCorrectR.md)).
  It is only a suggested dependency, so install it separately with
  `BiocManager::install("sva")`.
- **Minimum 8 GB RAM** recommended for processing large datasets.

**Apple Silicon limitation:** Devices with Apple silicon (M1, M2, M3,
M4) are currently unable to run
[`msConvertR()`](https://mstargetr.github.io/MStargetR/reference/msConvertR.md)
or
[`PeakForgeR()`](https://mstargetr.github.io/MStargetR/reference/PeakForgeR.md)
(with method set to Skyline) due to Docker image architecture
constraints on ARM processors.

## Running on HPC (Apptainer / Singularity)

Most HPC clusters forbid Docker.
[`msConvertR()`](https://mstargetr.github.io/MStargetR/reference/msConvertR.md)
and
[`PeakForgeR()`](https://mstargetr.github.io/MStargetR/reference/PeakForgeR.md)
accept an `enable_HPC = TRUE` argument that swaps Docker for Apptainer
(formerly Singularity), which converts the same ProteoWizard image to a
`.sif` and runs it as the invoking user with no daemon required.

**Step 1 – pull the SIF on a login node (one-time, ~GB):**

``` bash
module load apptainer
apptainer pull mstargetr-pwiz.sif \
    docker://proteowizard/pwiz-skyline-i-agree-to-the-vendor-licenses:skyline_26.1.0.057-c07debd
```

(Substitute the tag pinned by your installed MStargetR version –
`MStargetR:::MSTARGETR_DOCKER_IMAGE_TAG` prints it.)

**Step 2 – point MStargetR at the SIF in your `.Rprofile` so every job
picks it up automatically:**

``` r

options(
  MStargetR.enable_HPC = TRUE,
  MStargetR.sif_path   = "/path/to/mstargetr-pwiz.sif"
)
```

With these set you can call
[`msConvertR()`](https://mstargetr.github.io/MStargetR/reference/msConvertR.md)
and
[`PeakForgeR()`](https://mstargetr.github.io/MStargetR/reference/PeakForgeR.md)
exactly as on a workstation – no other code changes needed.

If `MStargetR.sif_path` is unset and the compute node has outbound
network, MStargetR will auto-pull the SIF into
`tools::R_user_dir("MStargetR", "cache")` on first use. HPC compute
nodes typically lack internet, so set the option explicitly.

## Installation

### Windows installer (recommended for GUI users)

Download `MStargetR_Setup.exe` from the [GitHub
Releases](https://github.com/MStargetR/MStargetR/releases) page and run
it. The installer will:

1.  Check for R and offer to download it if not found.
2.  Check for Docker Desktop (optional – only needed for file conversion
    and peak integration).
3.  Install all R package dependencies and MStargetR itself.
4.  Create a desktop shortcut that launches the Shiny GUI.

No R knowledge is required – just double-click the desktop shortcut
after installation.

### From R: helper function

The quickest way to install MStargetR from an R session is with the
bundled helper function:

``` r

source("https://raw.githubusercontent.com/MStargetR/MStargetR/main/R/install.R")
install_MStargetR()
rm(install_MStargetR)
library(MStargetR)
```

### From R: manual installation

If you prefer to manage dependencies yourself:

``` r

# Install Bioconductor dependencies first
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
BiocManager::install(c("mzR", "ropls", "statTarget"))

# Install MStargetR from GitHub
if (!requireNamespace("remotes", quietly = TRUE))
    install.packages("remotes")
remotes::install_github("MStargetR/MStargetR")
```

## Quick Start

### 1. Set up the project directory

Create a project folder with a `raw_data` subfolder containing your
vendor files. Each output mzML is grouped under a **plate** folder,
which is the unit used for QC and batch correction downstream.

**SCIEX `.wiff` (one multi-sample file per plate)** — place the files
flat in `raw_data/`; each `.wiff` is treated as its own plate
automatically:

``` R
MyProject/
  raw_data/
    plate1.wiff
    plate1.wiff.scan
    plate2.wiff
    plate2.wiff.scan
```

**One-file-per-sample formats (`.d`, `.raw`, …)** — in most cases just
place the files flat in `raw_data/` and let MStargetR group them. It
infers plate membership in this priority order:

1.  **A remembered `plate_grouping.csv`** at the project root, or an
    explicit `manifest =` argument (both are `raw_file,plateID` CSVs) —
    the override.

2.  **Per-plate subfolders** of `raw_data/` named after the plate:

    ``` R
    MyProject/
      raw_data/
        PlateA/
          sample1.raw
          sample2.raw
        PlateB/
          sample1.raw
          sample2.raw
    ```

3.  **Filename auto-discovery** — for files left flat, MStargetR detects
    which part of the filename identifies the plate (no lab-specific
    pattern needed), **reports** the grouping it inferred, and saves it
    to an editable `plate_grouping.csv` so the decision is stable across
    runs and you can correct it once if it guessed wrong:

    ``` R
    MyProject/
      raw_data/
        ..._COVp298_001.raw   # plate COVp298
        ..._COVp298_002.raw
        ..._COVp299_001.raw   # plate COVp299
        ..._COVp299_002.raw
    ```

> Auto-discovery proceeds rather than blocking: it always reports what
> it grouped, so a wrong guess is visible before conversion. If the
> filenames share no structure it can use, each file is converted as its
> own plate with a **warning** — group them by adding a subfolder, a
> `manifest =`, or by editing the generated `plate_grouping.csv`.
> Multi-sample `.wiff` files are exempt: each is always its own plate.

### 2. Convert vendor files to mzML

``` r

library(MStargetR)

msConvertR(
  input_directory  = "C:/Users/me/Desktop/MyProject/raw_data",
  output_directory = "C:/Users/me/Desktop/MyProject"
)

# With a manifest (one-file-per-sample formats grouped by an external sheet):
msConvertR(
  input_directory  = "C:/Users/me/Desktop/MyProject/raw_data",
  output_directory = "C:/Users/me/Desktop/MyProject",
  manifest         = "C:/Users/me/Desktop/MyProject/manifest.csv"
)
```

### 3. Peak picking and integration

``` r

PeakForgeR(
  user_name         = "jdoe",
  project_directory = "C:/Users/me/Desktop/MyProject",
  mrm_template_list = list("C:/Users/me/templates/lipid_mrm_v1.tsv"),
  QC_sample_label   = "LTR"
)
```

### 4. Quality control and reporting

``` r

# Using QCRFSC (default, requires QC samples)
qcCheckR(
  user_name         = "jdoe",
  project_directory = "C:/Users/me/Desktop/MyProject",
  mrm_template_list = list(
    v1 = list(
      SIL_guide  = "C:/Users/me/templates/lipid_mrm_v1.tsv",
      conc_guide = "C:/Users/me/templates/SIL_conc_guide_v1.tsv"
    )
  ),
  QC_sample_label   = "LTR",
  sample_tags       = c("sample", "control", "blank", "qc"),
  mv_threshold      = 50,
  batch_method      = "QCRFSC"
)

# Using ComBat (does not require QC samples)
qcCheckR(
  user_name         = "jdoe",
  project_directory = "C:/Users/me/Desktop/MyProject",
  mrm_template_list = list(
    v1 = list(
      SIL_guide  = "C:/Users/me/templates/lipid_mrm_v1.tsv",
      conc_guide = "C:/Users/me/templates/SIL_conc_guide_v1.tsv"
    )
  ),
  QC_sample_label   = "LTR",
  sample_tags       = c("sample", "control", "blank", "qc"),
  mv_threshold      = 50,
  batch_method      = "ComBat"
)
```

## Shiny Application

For an interactive, code-free experience, launch the built-in Shiny GUI:

``` r

library(MStargetR)
launchMStargetR()
```

The GUI requires additional packages (`shiny`, `bslib`, `DT`,
`shinyWidgets`, `htmltools`). If any are missing,
[`launchMStargetR()`](https://mstargetr.github.io/MStargetR/reference/launchMStargetR.md)
will display installation instructions.

## Standalone Batch Correction

[`batchCorrectR()`](https://mstargetr.github.io/MStargetR/reference/batchCorrectR.md)
can be used independently of the main pipeline to apply QC-based signal
drift and interbatch correction to any targeted LC-MS dataset. It
accepts a data.frame with columns `sample_name`, `batch`, `sample_type`,
`run_order`, and one or more numeric metabolite columns:

``` r

result <- batchCorrectR(
  data   = my_data,
  method = "QCRFSC",
  plot   = TRUE,
  report = TRUE
)

# Access corrected data
corrected <- result$corrected_data

# View per-metabolite RSD improvement
result$correction_summary
```

Two correction methods are available: `QCRFSC` (random forest, default)
and `ComBat` (empirical Bayes). See
[`?batchCorrectR`](https://mstargetr.github.io/MStargetR/reference/batchCorrectR.md)
for full parameter documentation.

## Project Directory Structure

After running the full pipeline, the project directory will have the
following structure:

``` R
MyProject/
  archive/                          # Archived vendor files and logs per plate
  all/
    data/
      batch_correction/             # Batch and signal drift correction outputs
      qs2/                          # Saved R data objects from qcCheckR (qs2 format; load with qs2::qs_read())
    html_report/                    # HTML report with PCA and control charts
    xlsx_report/                    # Excel files for downstream analysis
  PLATE_NAME/                       # One folder per plate
    data/
      mzml/                         # Converted mzML files
      qs2/                          # Saved R data objects from PeakForgeR (qs2 format; load with qs2::qs_read())
      raw_data/                     # Original vendor files
      reports/                      # Peak picking reports
      chromatograms/                # Exported chromatogram files
```

## Documentation

- **Vignette:** A detailed user guide is available at
  <https://mstargetr.github.io/MStargetR/articles/MStargetR-vignette.html>
- **Function reference:** Use
  [`?msConvertR`](https://mstargetr.github.io/MStargetR/reference/msConvertR.md),
  [`?PeakForgeR`](https://mstargetr.github.io/MStargetR/reference/PeakForgeR.md),
  [`?qcCheckR`](https://mstargetr.github.io/MStargetR/reference/qcCheckR.md),
  [`?batchCorrectR`](https://mstargetr.github.io/MStargetR/reference/batchCorrectR.md),
  [`?launchMStargetR`](https://mstargetr.github.io/MStargetR/reference/launchMStargetR.md),
  [`?transition_checkR`](https://mstargetr.github.io/MStargetR/reference/transition_checkR.md),
  or
  [`?compare_mrm_template_with_guide`](https://mstargetr.github.io/MStargetR/reference/compare_mrm_template_with_guide.md)
  for full parameter documentation.

## Troubleshooting

- **Run R as administrator** on Windows to ensure Docker communication
  works correctly.
- **Verify Docker is installed and running.** Both
  [`msConvertR()`](https://mstargetr.github.io/MStargetR/reference/msConvertR.md)
  and
  [`PeakForgeR()`](https://mstargetr.github.io/MStargetR/reference/PeakForgeR.md)
  require Docker Desktop to be active in the background – unless you are
  on HPC, in which case set `enable_HPC = TRUE` and use Apptainer (see
  “Running on HPC” above).
- **HPC: pre-pull the SIF on a login node.** Auto-pull fails on compute
  nodes without outbound network; set
  `options(MStargetR.sif_path = "/path/to/mstargetr-pwiz.sif")` after
  pulling once on a login node.
- **HPC: load the Apptainer module.** Most HPC sites require
  `module load apptainer` (or `singularity`) before R is launched.
- **Keep software up to date.** Ensure R, RStudio, and all package
  dependencies are current.
- **Check for file corruption.** Corrupted vendor files or incomplete
  mzML conversions can cause processing failures.
- **Ensure sufficient disk space.** Conversion and peak picking generate
  intermediate files that require additional storage.
- **Minimum 8 GB RAM** is required. Large multi-plate datasets benefit
  from processing plate-by-plate, which the pipeline handles
  automatically.

## Citation

If you use MStargetR in your work, please cite:

> Szemray, H., Nambiar, V., Lawler, N., Wist, J., Lodge, S., & Whiley,
> L. (2025). *MStargetR* \[Computer software\]. Zenodo.
> <https://doi.org/10.5281/zenodo.18476537>

## License

MStargetR is released under the MIT License. See the
[LICENSE](https://mstargetr.github.io/MStargetR/LICENSE) file for
details.

## Support

For bug reports, feature requests, or technical assistance, please open
an issue on GitHub: <https://github.com/MStargetR/MStargetR/issues>
