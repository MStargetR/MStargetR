# MStargetR

> **Note:** Code examples in this vignette use `eval=FALSE` because the
> pipeline requires Docker containers for file conversion and peak
> integration. To run the examples, ensure Docker Desktop is installed
> and running, then copy the code into an interactive R session.

## Introduction

`MStargetR` is an R package that provides an end-to-end pipeline for
processing targeted multiple reaction monitoring (MRM) liquid
chromatography-mass spectrometry (LC-MS) data. Starting from vendor raw
files, the package converts, integrates, quality-controls, and
batch-corrects data to produce concentration values that are ready for
downstream statistical analysis.

The package is designed for metabolomics researchers who work with
targeted MRM assays and require a reproducible, cross-platform workflow.
Cross-platform compatibility is achieved through Docker containers that
encapsulate vendor-dependent conversion and peak-picking tools.

### What this vignette covers

This vignette describes:

- Installation and prerequisites
- The three-step core pipeline (`msConvertR` -\> `PeakForgeR` -\>
  `qcCheckR`)
- Batch correction methods (QC-based QCRFSC and QC-RLSC, QC-free ComBat)
- Standalone batch correction with `batchCorrectR`
- The interactive Shiny application
- Utility functions for template validation
- Multi-method project configuration
- Output structure and troubleshooting

### Prerequisites

- **R** \>= 4.1.0
- **Docker Desktop** installed and running
  (<https://www.docker.com/get-started/>). Docker is required for the
  `msConvertR` and `PeakForgeR` steps, which use containerised
  ProteoWizard and Skyline tools.
- An active internet connection for the initial Docker image pulls.

> **HPC users:** Docker is typically forbidden on HPC clusters. Pass
> `enable_HPC = TRUE` (or set `options(MStargetR.enable_HPC = TRUE)`) to
> route
> [`msConvertR()`](https://mstargetr.github.io/MStargetR/reference/msConvertR.md)
> and
> [`PeakForgeR()`](https://mstargetr.github.io/MStargetR/reference/PeakForgeR.md)
> through Apptainer / Singularity instead. See the “Running on HPC”
> section of the README for SIF setup instructions.

------------------------------------------------------------------------

## Installation

### Windows installer (recommended for GUI users)

Download `MStargetR_Setup.exe` from the [GitHub
Releases](https://github.com/MStargetR/MStargetR/releases) page and run
it. The installer will:

1.  Check for R and offer to download it if not found.
2.  Check for Docker Desktop (optional – only needed for `msConvertR`
    and `PeakForgeR`).
3.  Install all R package dependencies and MStargetR itself.
4.  Create a desktop shortcut that launches the Shiny GUI.

No R knowledge is required – just double-click the desktop shortcut
after installation.

### From R: helper function (recommended)

The
[`install_MStargetR()`](https://mstargetr.github.io/MStargetR/reference/install_MStargetR.md)
helper ensures that BiocManager, remotes, and all Bioconductor
dependencies are installed and up to date before installing the package
from GitHub.

``` r

source("https://raw.githubusercontent.com/MStargetR/MStargetR/main/R/install.R")
install_MStargetR()
```

### From R: manual installation

If you prefer to install manually, use
[`remotes::install_github()`](https://remotes.r-lib.org/reference/install_github.html)
after ensuring that Bioconductor packages are available:

``` r

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install(c("mzR", "ropls", "statTarget", "sva"), ask = FALSE)
remotes::install_github("MStargetR/MStargetR")
```

### Loading the package

``` r

library(MStargetR)
```

------------------------------------------------------------------------

## Running on HPC (Apptainer / Singularity)

Docker is forbidden on most HPC clusters.
[`msConvertR()`](https://mstargetr.github.io/MStargetR/reference/msConvertR.md)
and
[`PeakForgeR()`](https://mstargetr.github.io/MStargetR/reference/PeakForgeR.md)
accept an `enable_HPC` argument that swaps Docker for **Apptainer**
(formerly Singularity) without changing anything else in the pipeline.
The same pinned ProteoWizard image is used; Apptainer pulls it into a
`.sif` file and runs it as the invoking user, no daemon required.

If you are running on a workstation with Docker, you can skip this
section – the default (`enable_HPC = FALSE`) keeps Docker behaviour
unchanged.

### One-time setup on the login node

HPC compute nodes typically have no outbound network, so build the
`.sif` once on a login node and stash the path. The image tag is pinned
by your installed MStargetR version; print it with
`MStargetR:::MSTARGETR_DOCKER_IMAGE_TAG`.

``` bash
# On a login node (shell), with Apptainer available:
module load apptainer

apptainer pull \
  /scratch/$USER/mstargetr-pwiz.sif \
  docker://proteowizard/pwiz-skyline-i-agree-to-the-vendor-licenses:skyline_26.1.0.057-c07debd
```

The pull is ~GB and only needs to be repeated when the MStargetR image
tag changes (a major release).

### Per-job R configuration

Set the two MStargetR options once – either in your project script, your
`~/.Rprofile`, or via `R_PROFILE_USER` in your job script. With these
set, you call
[`msConvertR()`](https://mstargetr.github.io/MStargetR/reference/msConvertR.md)
and
[`PeakForgeR()`](https://mstargetr.github.io/MStargetR/reference/PeakForgeR.md)
exactly as on a workstation.

``` r

options(
  MStargetR.enable_HPC = TRUE,
  MStargetR.sif_path   = "/scratch/your_user/mstargetr-pwiz.sif"
)

library(MStargetR)

msConvertR(
  input_directory  = "/scratch/your_user/MyProject/raw_data",
  output_directory = "/scratch/your_user/MyProject"
)

PeakForgeR(
  user_name         = "your_user",
  project_directory = "/scratch/your_user/MyProject",
  mrm_template_list = list("/scratch/your_user/templates/lipid_mrm_v1.tsv"),
  QC_sample_label   = "LTR"
)
```

You can also enable HPC mode for a single call without setting the
option:

``` r

msConvertR(
  input_directory  = "/scratch/your_user/MyProject/raw_data",
  output_directory = "/scratch/your_user/MyProject",
  enable_HPC       = TRUE
)
```

### SIF resolution order

`enable_HPC = TRUE` looks for a `.sif` in this order:

1.  `getOption("MStargetR.sif_path")` – the explicit option (most
    reliable on HPC because it bypasses any pull attempt).
2.  `tools::R_user_dir("MStargetR", "cache")/mstargetr-pwiz-<tag>.sif` –
    a tag-versioned cache populated by step 3.
3.  `apptainer pull docker://...` into the cache – only succeeds if the
    node has outbound network. The pull is one-time per tag; older
    `.sif` files stay on disk so prior analyses remain reproducible.

If you forget to set `MStargetR.sif_path` on a network-less compute
node, the auto-pull fails with a clear error pointing you back at the
login-node command above.

### Sample SLURM script

A minimal sketch – adapt module names and resource requests to your
site:

``` bash
#!/bin/bash
#SBATCH --job-name=mstargetr
#SBATCH --cpus-per-task=8
#SBATCH --mem=32G
#SBATCH --time=04:00:00

module load apptainer
module load r/4.4.0

Rscript -e '
  options(
    MStargetR.enable_HPC = TRUE,
    MStargetR.sif_path   = "/scratch/$USER/mstargetr-pwiz.sif"
  )
  library(MStargetR)
  msConvertR("/scratch/$USER/MyProject/raw_data",
             "/scratch/$USER/MyProject")
'
```

------------------------------------------------------------------------

## Core Workflow

The MStargetR pipeline consists of three sequential steps. Each step
produces outputs that feed into the next.

    Raw vendor files
          |
          v
     [msConvertR]   -- convert to mzML
          |
          v
     [PeakForgeR]   -- retention time optimisation, peak picking, integration
          |
          v
     [qcCheckR]     -- QC, batch correction, filtering, reporting
          |
          v
     Concentration data ready for statistics

------------------------------------------------------------------------

### Project Setup

Before running the pipeline, create a project directory and place your
vendor raw files (e.g., `.wiff`, `.wiff.scan`, `.raw`, `.d`) inside a
subdirectory named `raw_data`.

The pipeline organises everything by **plate** – one folder per plate,
which is the unit used for QC and batch correction. How you arrange
`raw_data/` tells
[`msConvertR()`](https://mstargetr.github.io/MStargetR/reference/msConvertR.md)
which samples belong to which plate.

**SCIEX `.wiff` (one multi-sample file per plate).** Place the files
flat in `raw_data/`; each `.wiff` is its own plate automatically.

``` r

# Define the project directory
project_dir <- "/path/to/my_project"

# Create the raw_data subdirectory
raw_data_dir <- file.path(project_dir, "raw_data")
dir.create(raw_data_dir, recursive = TRUE)

# Flat .wiff layout (each file = one plate):
#   my_project/
#     raw_data/
#       STUDY_PLATE_1.wiff
#       STUDY_PLATE_1.wiff.scan
#       STUDY_PLATE_2.wiff
#       STUDY_PLATE_2.wiff.scan
```

**One-file-per-sample formats (`.d`, `.raw`, …).** These vendors write
one file per sample, so you must tell
[`msConvertR()`](https://mstargetr.github.io/MStargetR/reference/msConvertR.md)
how to group them into plates. Use **either** a per-plate subfolder
(recommended) **or** a manifest CSV.

``` r

# Option A -- per-plate subfolders (subfolder name = plate ID):
#   my_project/
#     raw_data/
#       PlateA/
#         sample01.raw
#         sample02.raw
#       PlateB/
#         sample01.raw
#         sample02.raw

# Option B -- a manifest CSV mapping each file to a plate
#   (use when grouping comes from an instrument worklist or LIMS export):
manifest <- data.frame(
  raw_file = c("sample01.raw", "sample02.raw", "sample03.raw"),
  plateID  = c("PlateA",       "PlateA",       "PlateB")
)
write.csv(manifest, file.path(project_dir, "manifest.csv"), row.names = FALSE)
```

> **Why grouping matters.** If several single-sample files are left flat
> in `raw_data/` with no subfolder and no manifest,
> [`msConvertR()`](https://mstargetr.github.io/MStargetR/reference/msConvertR.md)
> stops with guidance instead of silently creating one folder per sample
> – which would collapse per-plate QC and batch correction to a single
> sample. Multi-sample `.wiff` files are exempt; each is its own plate.

------------------------------------------------------------------------

### Step 1: msConvertR – Vendor File Conversion

#### Purpose

[`msConvertR()`](https://mstargetr.github.io/MStargetR/reference/msConvertR.md)
converts vendor-specific mass spectrometry files into the open mzML
format using the ProteoWizard `msconvert` tool, executed inside a Docker
container. This step ensures reproducibility and eliminates the need for
a Windows-only ProteoWizard installation.

#### Parameters

| Parameter | Type | Description |
|:---|:---|:---|
| `input_directory` | Character | Path to the directory containing a `raw_data/` folder of vendor raw files. |
| `output_directory` | Character | Path where converted mzML files and the project structure will be created. May be the same as `input_directory`. |
| `manifest` | Character / data.frame / NULL | Optional `raw_file,plateID` mapping (CSV path or data frame) used to group one-file-per-sample formats into plates. When `NULL` (default), plate membership is inferred from per-plate subfolders under `raw_data/`, falling back to the filename for flat files. |

#### Supported vendor formats

Bruker (`.d`, `.baf`, `.fid`, `.yep`, `.tsf`, `.tdf`), SCIEX (`.wiff`,
`.wiff.scan`, `.wiff2`), Shimadzu (`.lcd`, `.lcdproj`), Thermo (`.raw`),
Waters (`.raw` directory), and others.

#### Example

``` r

# Flat .wiff or per-plate subfolders -- no manifest needed:
msConvertR(
  input_directory  = "/path/to/my_project",
  output_directory = "/path/to/my_project"
)

# One-file-per-sample formats grouped by a manifest:
msConvertR(
  input_directory  = "/path/to/my_project",
  output_directory = "/path/to/my_project",
  manifest         = "/path/to/my_project/manifest.csv"
)
```

After conversion, `msConvertR` creates a structured project directory
with one subfolder per plate. Each plate folder contains `data/mzml/`
holding the converted files and `data/raw_data/` holding the original
vendor files. All samples belonging to a plate – whether they came from
one multi-sample `.wiff` or many single-sample `.d`/`.raw` files – land
together in that plate’s `data/mzml/`.

**Note:** Docker Desktop must be running before calling
[`msConvertR()`](https://mstargetr.github.io/MStargetR/reference/msConvertR.md).
The function will stop with an informative error if Docker is not
detected.

------------------------------------------------------------------------

### Step 2: PeakForgeR – Peak Picking and Integration

#### Purpose

[`PeakForgeR()`](https://mstargetr.github.io/MStargetR/reference/PeakForgeR.md)
performs retention time optimisation, peak boundary detection, and
chromatographic peak integration using Skyline MS inside a Docker
container. It processes each plate in parallel and produces a CSV report
of integrated peak areas for every sample and transition.

#### Parameters

| Parameter | Type | Description |
|:---|:---|:---|
| `user_name` | Character | A user identifier string (used in logging). |
| `project_directory` | Character | Path to the project directory created by `msConvertR` (or set up manually). |
| `mrm_template_list` | List | A list of file paths to MRM transition template files (TSV or CSV). For multi-method projects, provide a named list (see the Multi-Method Analysis section). |
| `QC_sample_label` | Character | A tag used to identify QC/LTR samples within file names. Case-insensitive. For example, `"LTR"` or `"QC"`. |
| `plateID_outputs` | Character vector or NULL | Plate identifiers. Only required if you did **not** use `msConvertR` and need to specify plate names manually. Default is `NULL`. |

#### MRM transition template format

The MRM template is a tab-separated (TSV) or comma-separated (CSV) file
that defines the transitions to be monitored. The package includes
several example templates. The required columns are:

``` r

mrm_template_path <- system.file(
  "extdata", "LGW_lipid_mrm_template_v1.tsv",
  package = "MStargetR"
)
mrm_template <- readr::read_tsv(mrm_template_path, show_col_types = FALSE,
                                name_repair = "minimal")
head(mrm_template)
#> # A tibble: 6 × 10
#>   `Molecule List Name` `Precursor Name` `Precursor Mz` `Precursor Charge`
#>   <chr>                <chr>                     <dbl>              <dbl>
#> 1 CE                   CE(14:0)                   615.                  1
#> 2 CE                   CE(16:0)                   643.                  1
#> 3 CE                   CE(16:1)                   641.                  1
#> 4 CE                   CE(18:0)                   671.                  1
#> 5 CE                   CE(18:1)                   669.                  1
#> 6 CE                   CE(18:2)                   667.                  1
#> # ℹ 6 more variables: `Product Mz` <dbl>, `Product Charge` <dbl>,
#> #   `Explicit Retention Time` <dbl>, `Explicit Retention Time Window` <dbl>,
#> #   Note <chr>, control_chart <lgl>
```

Key columns include `Precursor Name`, `Precursor Mz`, `Product Mz`,
`Explicit Retention Time`, and `Note` (which links analytes to their
corresponding stable isotope-labelled internal standard).

#### Example

``` r

PeakForgeR(
  user_name         = "HSzemray",
  project_directory = "/path/to/my_project",
  mrm_template_list = list(
    system.file("extdata", "LGW_lipid_mrm_template_v1.tsv", package = "MStargetR")
  ),
  QC_sample_label   = "LTR",
  plateID_outputs   = NULL
)
```

#### Output

`PeakForgeR` writes a CSV report per plate containing integrated peak
areas. An example of this output is bundled with the package:

``` r

report_path <- system.file(
  "extdata", "Example_PeakForgeR_report.csv",
  package = "MStargetR"
)
peakforger_report <- read.csv(report_path, check.names = FALSE)
head(peakforger_report[, 1:6])
#>     FileName MoleculeListName MoleculeName PrecursorMz ProductMz RetentionTime
#> 1 PLASMA_LTR               CE     CE(14:0)       614.6     369.4      11.65722
#> 2   Sample_1               CE     CE(14:0)       614.6     369.4      11.61070
#> 3 PLASMA_LTR               CE     CE(16:0)       642.6     369.4      12.33465
#> 4   Sample_1               CE     CE(16:0)       642.6     369.4      12.33795
#> 5 PLASMA_LTR               CE     CE(16:1)       640.6     369.4      11.63252
#> 6   Sample_1               CE     CE(16:1)       640.6     369.4      11.66010
```

**Note:** Docker Desktop must be running before calling
[`PeakForgeR()`](https://mstargetr.github.io/MStargetR/reference/PeakForgeR.md).

------------------------------------------------------------------------

### Step 3: qcCheckR – Quality Control and Reporting

#### Purpose

[`qcCheckR()`](https://mstargetr.github.io/MStargetR/reference/qcCheckR.md)
is the final step in the core pipeline. It performs:

- Data transposition and sorting
- Missing value imputation
- Response concentration calculation using internal standard ratios
- Signal drift and interbatch correction (via `statTarget`)
- QC-based sample filtering
- SIL internal standard filtering
- Lipid-specific quality filters
- RSD-based feature filtering
- PCA analysis and visualisation
- Run-order plots and target control charts
- Export of an HTML report and an Excel workbook

#### Parameters

| Parameter | Type | Description |
|:---|:---|:---|
| `user_name` | Character | A user identifier string. |
| `project_directory` | Character | Path to the project directory containing `PeakForgeR` output. |
| `mrm_template_list` | Named list | A named list of lists. Each element must contain `SIL_guide` (path to the MRM template) and `conc_guide` (path to the SIL concentration guide). See the example below. |
| `QC_sample_label` | Character | Tag to identify QC samples in file names (e.g., `"LTR"`, `"qc"`). Default is `"LTR"`. |
| `sample_tags` | Character vector | Tags that identify sample types in file names (e.g., `c("sample", "control", "qc")`). |
| `mv_threshold` | Numeric | Percentage threshold for missing value filtering (0–100). Features with a higher percentage of missing values than this threshold are removed. Default is `50`. |
| `batch_method` | Character | Batch correction method: `"QCRFSC"` (random forest, default), `"ComBat"` (empirical Bayes, QC-free), or `"QCRLSC"` (robust LOESS, requires QC samples). |
| `batch_ntree` | Integer | Number of trees for random forest correction. Ignored for other methods. Default is `500`. |
| `batch_coCV` | Numeric | Coefficient of variation cutoff (%, 1–100) for feature filtering inside statTarget. Features with QC CV above this threshold are removed. Default is `100` (no filtering). |
| `batch_Frule` | Numeric | Filtering rule (0–1) for missing values inside statTarget. Default is `0`. |
| `batch_imputeM` | Character | Imputation method: `"minHalf"`, `"median"`, `"mean"`, or `"knn"`. Default is `"minHalf"`. |
| `combat_par.prior` | Logical | Use parametric priors in ComBat. Only used when `batch_method = "ComBat"`. Default is `TRUE`. |
| `combat_mean.only` | Logical | Correct only batch mean (not variance). Only used when `batch_method = "ComBat"`. Default is `FALSE`. |
| `combat_ref.batch` | Character or NULL | Reference batch for ComBat. Only used when `batch_method = "ComBat"`. Default is `NULL`. |
| `qcrlsc_method` | Character | QC-RLSC scaling: `"subtract"` (default, Dunn et al. protocol) or `"divide"` (preserves non-negativity). Only used when `batch_method = "QCRLSC"`. |
| `qcrlsc_intra` | Logical | Intra-batch (`TRUE`) vs inter-batch (`FALSE`, default) QC-RLSC. Only used when `batch_method = "QCRLSC"`. |
| `qcrlsc_opti` / `qcrlsc_log10` / `qcrlsc_outl` / `qcrlsc_shift` | Logical | Optimise LOESS span by GCV, log10-transform before fitting, QC outlier detection, and apply `batch.shift` after correction. All default `TRUE`. Only used when `batch_method = "QCRLSC"`. |

#### The mrm_template_list structure

For `qcCheckR`, the `mrm_template_list` differs from the one used in
`PeakForgeR`. It is a **named list of lists**, where each sub-list
contains two named paths:

- `SIL_guide`: The MRM transition template file (the same file used in
  `PeakForgeR`).
- `conc_guide`: A concentration guide file that maps SIL internal
  standard names to their known concentrations.

``` r

mrm_template_list <- list(
  v1 = list(
    SIL_guide  = "/path/to/LGW_lipid_mrm_template_v1.tsv",
    conc_guide = "/path/to/LGW_SIL_batch_103.tsv"
  )
)
```

#### Concentration guide format

The concentration guide is a TSV file mapping each internal standard to
its known concentration. An example is included in the package:

``` r

conc_guide_path <- system.file(
  "extdata", "LGW_SIL_batch_103.tsv",
  package = "MStargetR"
)
conc_guide <- read.delim(conc_guide_path, check.names = FALSE)
head(conc_guide)
#>   SIL_source      ISTD     MW mg/mL        uM In solution x200 diluted
#> 1  Lipidyzer dCE(16:0) 631.62  0.14  221.6523                     1.11
#> 2  Lipidyzer dCE(16:1) 629.61  0.14  222.3599                     1.11
#> 3  Lipidyzer dCE(18:1) 657.64  0.51  775.5003                     3.88
#> 4  Lipidyzer dCE(18:2) 655.62  1.47 2242.1525                    11.21
#> 5  Lipidyzer dCE(20:3) 681.64  0.16  234.7280                     1.17
#> 6  Lipidyzer dCE(20:4) 679.62  0.18  264.8539                     1.32
#>   For plasma dilution correction x9 concentration_factor
#> 1                              9.99                 9.99
#> 2                              9.99                 9.99
#> 3                             34.92                34.92
#> 4                            100.89               100.89
#> 5                             10.53                10.53
#> 6                             11.88                11.88
#>                    SIL_name
#> 1 SIL_CE(16:0)_d7_Lipidyzer
#> 2 SIL_CE(16:1)_d7_Lipidyzer
#> 3 SIL_CE(18:1)_d7_Lipidyzer
#> 4 SIL_CE(18:2)_d7_Lipidyzer
#> 5 SIL_CE(20:3)_d7_Lipidyzer
#> 6 SIL_CE(20:4)_d7_Lipidyzer
```

The `SIL_name` column must match the values in the `Note` column of the
MRM transition template.

#### Example

``` r

# Using QCRFSC (default, requires QC samples)
qcCheckR(
  user_name          = "HSzemray",
  project_directory  = "/path/to/my_project",
  mrm_template_list  = list(
    v1 = list(
      SIL_guide  = "/path/to/LGW_lipid_mrm_template_v1.tsv",
      conc_guide = "/path/to/LGW_SIL_batch_103.tsv"
    )
  ),
  QC_sample_label    = "LTR",
  sample_tags        = c("sample", "control", "qc"),
  mv_threshold       = 50,
  batch_method       = "QCRFSC"
)

# Using ComBat (does not require QC samples)
qcCheckR(
  user_name          = "HSzemray",
  project_directory  = "/path/to/my_project",
  mrm_template_list  = list(
    v1 = list(
      SIL_guide  = "/path/to/LGW_lipid_mrm_template_v1.tsv",
      conc_guide = "/path/to/LGW_SIL_batch_103.tsv"
    )
  ),
  QC_sample_label    = "LTR",
  sample_tags        = c("sample", "control", "qc"),
  mv_threshold       = 50,
  batch_method       = "ComBat",
  combat_par.prior   = TRUE,
  combat_mean.only   = FALSE
)

# Using QC-RLSC (QC-based robust LOESS signal correction; requires QC samples)
qcCheckR(
  user_name          = "HSzemray",
  project_directory  = "/path/to/my_project",
  mrm_template_list  = list(
    v1 = list(
      SIL_guide  = "/path/to/LGW_lipid_mrm_template_v1.tsv",
      conc_guide = "/path/to/LGW_SIL_batch_103.tsv"
    )
  ),
  QC_sample_label    = "LTR",
  sample_tags        = c("sample", "control", "qc"),
  mv_threshold       = 50,
  batch_method       = "QCRLSC",
  qcrlsc_method      = "subtract"
)
```

#### Outputs

`qcCheckR` generates three main outputs in the project directory:

1.  **HTML report** – An interactive report containing PCA plots,
    run-order plots, control charts, and summary statistics.
2.  **Excel workbook** – A multi-sheet workbook with final concentration
    data, filtering summaries, and a navigation guide on the first
    sheet.
3.  **qs2 file** – A saved R data object (qs2 package format, written
    with multi-threaded zstd compression) containing the full
    `master_list` for programmatic access to all intermediate results.
    Load with `qs2::qs_read("path/to/file.qs2")`.

------------------------------------------------------------------------

## Standalone Batch Correction (batchCorrectR)

The
[`batchCorrectR()`](https://mstargetr.github.io/MStargetR/reference/batchCorrectR.md)
function provides standalone signal drift and interbatch correction that
operates independently of the `qcCheckR` pipeline. It accepts a simple
data frame as input, making it suitable for users who have already
processed their data through other tools or who wish to apply batch
correction to any tabular metabolomics dataset.

### Parameters

| Parameter | Type | Default | Description |
|:---|:---|:---|:---|
| `data` | data.frame | (required) | Input data with samples as rows. Must contain columns: `sample_name`, `batch`, `sample_type`, `run_order`, plus numeric metabolite columns. |
| `qc_label` | Character | `"qc"` | String identifying QC samples in the `sample_type` column. |
| `method` | Character | `"QCRFSC"` | Correction method: `"QCRFSC"` (random forest, default), `"ComBat"` (empirical Bayes, QC-free), or `"QCRLSC"` (robust LOESS, requires QC samples). |
| `ntree` | Integer | `500` | Number of trees for the random forest method. Ignored for other methods. |
| `coCV` | Numeric | `100` | Coefficient of variation cutoff (%, 1–100) for feature filtering in statTarget. Features with QC CV above this threshold are removed. |
| `Frule` | Numeric | `0` | Filtering rule percentage for missing values in statTarget. |
| `imputeM` | Character | `"minHalf"` | Imputation method: `"minHalf"`, `"median"`, `"mean"`, or `"knn"`. |
| `combat_par.prior` | Logical | `TRUE` | Use parametric empirical Bayes priors. Only applies when `method = "ComBat"`. |
| `combat_mean.only` | Logical | `FALSE` | If TRUE, correct only batch mean (not variance). Only applies when `method = "ComBat"`. |
| `combat_ref.batch` | Character or NULL | `NULL` | Reference batch for ComBat adjustment. Only applies when `method = "ComBat"`. |
| `qcrlsc_method` | Character | `"subtract"` | QC-RLSC scaling: `"subtract"` (Dunn et al. protocol) or `"divide"` (preserves non-negativity). Only applies when `method = "QCRLSC"`. |
| `qcrlsc_intra` / `qcrlsc_opti` / `qcrlsc_log10` / `qcrlsc_outl` / `qcrlsc_shift` | Logical | `FALSE` / `TRUE` / `TRUE` / `TRUE` / `TRUE` | Intra-batch correction, LOESS span GCV optimisation, log10 transform, QC outlier detection, and `batch.shift`. Only apply when `method = "QCRLSC"`. |
| `sample_tags` | Character vector or NULL | `NULL` | Optional sample-type labels to include in correction (in addition to QC). Rows whose type does not match `qc_label` or any of `sample_tags` are dropped before correction. Useful for excluding blanks or other low-signal types. |
| `output_dir` | Character | [`tempdir()`](https://rdrr.io/r/base/tempfile.html) | Directory for statTarget intermediate files. |
| `project_dir` | Character or NULL | `NULL` | If provided, the corrected data CSV and correction summary are saved into a `batch_correction` subfolder inside this directory. |
| `plot` | Logical | `TRUE` | Whether to generate before/after correction plots. |
| `report` | Logical | `TRUE` | Whether to generate an HTML summary report. |

### Correction methods

- **QCRFSC** (QC-based Random Forest Signal Correction): Fits a random
  forest model to QC samples across run order. Robust to non-linear
  drift patterns. Implemented via
  [`statTarget::shiftCor()`](https://rdrr.io/pkg/statTarget/man/shiftCor.html).
- **ComBat** (Empirical Bayes Batch Correction): Uses empirical Bayes
  methods from the `sva` package to adjust for batch effects. Unlike the
  QC-based methods above, ComBat does **not** require QC samples – it
  estimates and removes batch effects using only the batch labels. This
  makes it suitable for studies where QC reference material is not
  available. ComBat can correct both location (mean) and scale
  (variance) batch effects, or mean-only if `combat_mean.only = TRUE`.
  Install `sva` via `BiocManager::install("sva")`.
- **QC-RLSC** (QC-based Robust LOESS Signal Correction; Dunn et al.
  2011): Fits a robust LOESS trend through the QC injections (ordered by
  acquisition) for each feature and corrects samples against it, then
  optionally re-aligns batch means with `batch.shift`. Like QCRFSC it
  **requires** QC samples. The `qcrlsc_method` argument chooses additive
  (`"subtract"`, default) or multiplicative (`"divide"`) scaling; the
  latter preserves non-negativity, useful for concentration data.
  Implemented via
  [`qcrlscR::qc.rlsc.wrap()`](https://rdrr.io/pkg/qcrlscR/man/qc.rlsc.wrap.html);
  install `qcrlscR` via `install.packages("qcrlscR")`.

### Return value

`batchCorrectR` returns a named list containing:

- `corrected_data` – A tibble with the same structure as the input but
  with corrected metabolite values.
- `correction_summary` – A tibble with per-metabolite RSD before and
  after correction.
- `qc_rsd_before` – Named numeric vector of QC %RSD values before
  correction.
- `qc_rsd_after` – Named numeric vector of QC %RSD values after
  correction.
- `failed_qc` – Character vector of sample names flagged as failed QC
  injections (signal \< 10% of batch median).
- `plots` – A list of ggplot objects (only if `plot = TRUE`).
- `report` – Logical indicating whether an HTML report was requested.
- `report_path` – Path to the rendered HTML report (only if
  `report = TRUE` and rendering succeeds).

### Example

``` r

library(MStargetR)

# Prepare a synthetic input dataset
my_data <- data.frame(
  sample_name  = paste0("S", 1:20),
  batch        = rep(c("plate1", "plate2"), each = 10),
  sample_type  = rep(c("qc", "sample", "sample", "sample", "qc"), 4),
  run_order    = 1:20,
  metabolite_A = rnorm(20, mean = 100, sd = 15),
  metabolite_B = rnorm(20, mean = 500, sd = 75)
)

# Run batch correction with default settings (QCRFSC)
result <- batchCorrectR(data = my_data)

# Access the corrected data
corrected <- result$corrected_data
head(corrected)

# View the correction summary (RSD before vs. after)
result$correction_summary
```

``` r

# ComBat correction (does not require QC samples)
my_data_no_qc <- data.frame(
  sample_name  = paste0("S", 1:20),
  batch        = rep(c("plate1", "plate2"), each = 10),
  sample_type  = rep("sample", 20),
  run_order    = 1:20,
  metabolite_A = rnorm(20, mean = 100, sd = 15),
  metabolite_B = rnorm(20, mean = 500, sd = 75)
)

result_combat <- batchCorrectR(
  data   = my_data_no_qc,
  method = "ComBat"
)

# ComBat with mean-only correction
result_combat_mean <- batchCorrectR(
  data              = my_data_no_qc,
  method            = "ComBat",
  combat_mean.only  = TRUE
)
```

``` r

# QC-RLSC correction (QC-based robust LOESS; requires QC samples)
result_qcrlsc <- batchCorrectR(
  data          = my_data,
  method        = "QCRLSC",
  qcrlsc_method = "subtract"   # or "divide" to preserve non-negativity
)
```

------------------------------------------------------------------------

## Interactive Application (launchMStargetR)

MStargetR includes a Shiny-based graphical user interface that provides
visual access to all pipeline steps. This is particularly useful for
users who prefer a point-and-click workflow or who wish to explore
results interactively.

### Launching the application

``` r

library(MStargetR)
launchMStargetR()
```

### Parameters

| Parameter | Type | Default | Description |
|:---|:---|:---|:---|
| `port` | Integer or NULL | `NULL` | Port number for the Shiny app. If `NULL`, Shiny selects an available port automatically. |
| `launch.browser` | Logical | `TRUE` | Whether to open the application in a web browser. |
| `host` | Character | `"127.0.0.1"` | Host address. Use `"0.0.0.0"` to allow access from other machines on the network. |

### Additional dependencies

The GUI requires the following packages beyond the core MStargetR
dependencies: `shiny`, `bslib`, `DT`, `shinyWidgets`, and `htmltools`.
If any are missing,
[`launchMStargetR()`](https://mstargetr.github.io/MStargetR/reference/launchMStargetR.md)
will display an informative error listing the packages to install.

``` r

install.packages(c("shiny", "bslib", "DT", "shinyWidgets", "htmltools"))
```

**Note:** The Shiny GUI exposes batch correction method selection in
both the `qcCheckR` and `batchCorrectR` tabs. All three methods
(`QCRFSC`, `ComBat`, and `QC-RLSC`) are available from the interface,
with each method’s options shown in its own panel: the ComBat-specific
parameters (`combat_par.prior`, `combat_mean.only`, `combat_ref.batch`)
and the QC-RLSC-specific parameters (scaling, intra-batch, span
optimisation, log10, outlier detection, batch shift). When `ComBat` is
selected, the QC-specific options are hidden automatically since ComBat
does not require QC samples; `QCRFSC` and `QC-RLSC` both require QC
samples.

------------------------------------------------------------------------

## Utility Functions

MStargetR provides two utility functions for validating MRM transition
templates and concentration guides before running the core pipeline.
Running these checks in advance can save considerable time by catching
template errors early.

### transition_checkR

[`transition_checkR()`](https://mstargetr.github.io/MStargetR/reference/transition_checkR.md)
verifies that all Q1/Q3 transition pairs in an MRM template are unique.
Duplicate transitions will cause ambiguous peak assignments and must be
resolved before processing.

#### Parameters

| Parameter | Type | Description |
|:---|:---|:---|
| `transition_df` | data.frame | An MRM transition template data frame. Must contain columns `Precursor Mz`, `Product Mz`, and `Precursor Name`. |

#### Example

``` r

# Load an example MRM template
mrm_template_path <- system.file(
  "extdata", "LGW_lipid_mrm_template_v1.tsv",
  package = "MStargetR"
)
mrm_template_df <- read.delim(mrm_template_path, check.names = FALSE)

# Check for duplicate transitions
transition_checkR(mrm_template_df)
# If all transitions are unique, a confirmation message is printed.
# If duplicates are found, a data frame of clashing transitions is returned.
```

### compare_mrm_template_with_guide

[`compare_mrm_template_with_guide()`](https://mstargetr.github.io/MStargetR/reference/compare_mrm_template_with_guide.md)
checks whether all internal standard identifiers in the `Note` column of
the MRM template have a corresponding entry in the `SIL_name` column of
the concentration guide. Unmatched entries indicate missing
concentration information that would prevent accurate quantification.

#### Parameters

| Parameter | Type | Description |
|:---|:---|:---|
| `mrm_template` | data.frame | An MRM transition template data frame. Must contain a `Note` column. |
| `concentration_guide` | data.frame | A concentration guide data frame. Must contain a `SIL_name` column. |

#### Example

``` r

# Load the MRM template
mrm_template_path <- system.file(
  "extdata", "LGW_lipid_mrm_template_v1.tsv",
  package = "MStargetR"
)
mrm_template_df <- read.delim(mrm_template_path, check.names = FALSE)

# Load the concentration guide
conc_guide_path <- system.file(
  "extdata", "LGW_SIL_batch_103.tsv",
  package = "MStargetR"
)
conc_guide_df <- read.delim(conc_guide_path, check.names = FALSE)

# Check for unmatched internal standards
compare_mrm_template_with_guide(mrm_template_df, conc_guide_df)
# If all entries match, a confirmation message is printed.
# If mismatches are found, a character vector of unmatched Note values is returned.
```

------------------------------------------------------------------------

## Workflow Templates

MStargetR ships with pre-configured R Markdown workflow templates that
provide ready-to-use pipelines for common use cases. Use
[`use_workflow()`](https://mstargetr.github.io/MStargetR/reference/use_workflow.md)
to list and copy templates to your working directory.

### Available workflows

| Template | Description |
|:---|:---|
| `generic` | A starter template for any user. Includes placeholders for project path, QC label, MRM templates, and sample tags. |
| `CCSM` | Pre-configured ANPC CCSM lipidomics workflow. Uses built-in MRM templates and includes both single-project and multi-project batch processing examples. |

### Usage

``` r

# List available workflow templates
use_workflow()

# Copy the generic workflow to your working directory
use_workflow("generic")

# Copy the CCSM workflow to a specific project folder
use_workflow("CCSM", output_dir = "~/my_lipidomics_project")
```

After copying, open the `.Rmd` file, edit the user parameters at the top
(project path, QC label, etc.), and knit or source the file to run the
full pipeline.

------------------------------------------------------------------------

## Multi-Method Analysis

MStargetR supports projects that span multiple MRM methods (e.g., when a
lipid panel has been updated between study batches). This is common in
longitudinal studies where the analytical method evolves over time but
the same long-term reference (LTR) material is used throughout.

### When to use multi-method analysis

Use this approach when:

- Different plates in your study were acquired using different versions
  of the MRM transition list.
- You wish to combine data across method versions into a single,
  harmonised dataset.
- A consistent long-term reference material was measured on every plate,
  regardless of which method version was used.

**Important:** The same long-term reference material must be used across
all plates. If different reference materials were used, the results of
cross-method alignment will be invalid due to inconsistencies in signal
correction.

### How it works

1.  `PeakForgeR` receives all method versions in `mrm_template_list` and
    cycles through each template on every plate until the correct match
    is found.
2.  `qcCheckR` gathers all `PeakForgeR` reports from the project
    directory. Each plate is processed using its respective transition
    list and concentration guide.
3.  During batch correction, `qcCheckR` identifies plates run on
    different method versions, aligns matching target features, and
    applies signal drift correction first within each plate and then
    across all plates collectively.

### mrm_template_list structure for multi-method projects

For `PeakForgeR`, provide a **named list of file paths**:

``` r

mrm_template_list <- list(
  v1 = "/path/to/LGW_lipid_mrm_template_v1.tsv",
  v2 = "/path/to/LGW_lipid_mrm_template_v2.tsv"
)
```

For `qcCheckR`, provide a **named list of lists**, each containing both
the MRM template and its associated concentration guide:

``` r

mrm_template_list <- list(
  v1 = list(
    SIL_guide  = "/path/to/LGW_lipid_mrm_template_v1.tsv",
    conc_guide = "/path/to/LGW_SIL_batch_103.tsv"
  ),
  v2 = list(
    SIL_guide  = "/path/to/LGW_lipid_mrm_template_v2.tsv",
    conc_guide = "/path/to/v4_ISTD_conc_updated.tsv"
  )
)
```

### Complete multi-method example

``` r

library(MStargetR)

project_directory <- "/path/to/my_multimethod_project"

# Step 1: Set up the project directory with raw files
raw_data_dir <- file.path(project_directory, "raw_data")
dir.create(raw_data_dir, recursive = TRUE)
# Place all vendor files from all plates into raw_data_dir.

# Step 2: Convert vendor files to mzML
msConvertR(
  input_directory  = project_directory,
  output_directory = project_directory
)

# Step 3: Peak picking with multiple method templates
PeakForgeR(
  user_name         = "HSzemray",
  project_directory = project_directory,
  mrm_template_list = list(
    v1 = "/path/to/LGW_lipid_mrm_template_v1.tsv",
    v2 = "/path/to/LGW_lipid_mrm_template_v2.tsv"
  ),
  QC_sample_label   = "LTR"
)

# Step 4: QC and batch correction across methods
qcCheckR(
  user_name          = "HSzemray",
  project_directory  = project_directory,
  mrm_template_list  = list(
    v1 = list(
      SIL_guide  = "/path/to/LGW_lipid_mrm_template_v1.tsv",
      conc_guide = "/path/to/LGW_SIL_batch_103.tsv"
    ),
    v2 = list(
      SIL_guide  = "/path/to/LGW_lipid_mrm_template_v2.tsv",
      conc_guide = "/path/to/v4_ISTD_conc_updated.tsv"
    )
  ),
  QC_sample_label    = "LTR",
  sample_tags        = c("sample", "control", "qc"),
  mv_threshold       = 50,
  batch_method       = "QCRFSC"
)
```

------------------------------------------------------------------------

## Output Structure

After a complete pipeline run, the project directory will contain the
following structure:

    my_project/
    |
    |-- raw_data/                          # Original vendor files (pre-conversion)
    |
    |-- PLATE_ID_1/                        # One folder per plate
    |   |-- data/
    |   |   |-- mzml/                      # Converted mzML files
    |   |   |-- raw_data/                  # Archived vendor files for this plate
    |   |
    |   |-- reports/
    |   |   |-- *_PeakForgeR_report.csv    # Integrated peak area report
    |   |
    |   |-- chromatograms/                 # Chromatogram visualisations
    |
    |-- PLATE_ID_2/
    |   |-- ...                            # Same structure as above
    |
    |-- all/                               # Combined outputs from qcCheckR
    |   |-- *_qcCheckR_report.html         # Interactive HTML QC report
    |   |-- *_qcCheckR_data.xlsx           # Excel workbook with concentration data
    |   |-- data/qs2/
    |   |   |-- *_qcCheckR.qs2             # R data object (qs2 format) with all intermediate results
    |
    |-- MStargetR_logs/                    # Per-plate processing logs
    |   |-- PLATE_ID_1_MStargetR_log.txt
    |   |-- PLATE_ID_2_MStargetR_log.txt
    |
    |-- archive/                           # Archived raw files

#### Description of key outputs

| Directory / File | Description |
|:---|:---|
| `PLATE_ID/data/mzml/` | Open-format mzML files produced by `msConvertR`. |
| `PLATE_ID/reports/` | Per-plate CSV reports from `PeakForgeR` containing integrated peak areas. |
| `PLATE_ID/chromatograms/` | Chromatogram images for visual inspection of peak quality. |
| `all/*_qcCheckR_report.html` | Interactive HTML report with PCA, run-order plots, control charts, and summary statistics. |
| `all/*_qcCheckR_data.xlsx` | Multi-sheet Excel workbook. The first sheet provides a navigation guide. Subsequent sheets contain filtered concentration data, QC summaries, and batch correction diagnostics. |
| `all/data/qs2/*_qcCheckR.qs2` | An R data object in the `qs2` package’s multi-threaded zstd format, containing the full `master_list` for programmatic access to all intermediate processing results. Load with `qs2::qs_read("path/to/file.qs2")` (not [`base::load()`](https://rdrr.io/r/base/load.html)). |
| `MStargetR_logs/` | Text log files recording the processing status and any errors for each plate. |

------------------------------------------------------------------------

## Troubleshooting

### Docker is not detected

**Symptom:**
[`msConvertR()`](https://mstargetr.github.io/MStargetR/reference/msConvertR.md)
or
[`PeakForgeR()`](https://mstargetr.github.io/MStargetR/reference/PeakForgeR.md)
stops with a Docker-related error.

**Solution:** Ensure Docker Desktop is installed and running. On Windows
and macOS, open Docker Desktop and wait for the engine to start before
calling these functions. On Linux, verify that the Docker daemon is
active with `systemctl status docker`. If you are on an HPC cluster that
forbids Docker, set `enable_HPC = TRUE` (see “Running on HPC” above).

### Apptainer pull fails on an HPC compute node

**Symptom:** With `enable_HPC = TRUE`, the function errors during the
first call with a message referencing `apptainer pull docker://...` and
`MStargetR.sif_path`.

**Solution:** Most HPC compute nodes have no outbound network, so the
auto-pull cannot reach Docker Hub. Pull the `.sif` once on a login node
(which usually does have network access) and point MStargetR at it via
`options(MStargetR.sif_path = "/path/to/mstargetr-pwiz.sif")` in your
`.Rprofile` or job script. See the “Running on HPC” section above for
the full pull command.

### Apptainer is not found on PATH

**Symptom:** With `enable_HPC = TRUE`, the function errors with
“apptainer (or singularity) not found on PATH”.

**Solution:** On most HPC sites Apptainer is provided as an environment
module that must be loaded explicitly. Add `module load apptainer` (or
`module load singularity` on older installations) to your job script
before invoking R. MStargetR accepts either name – the legacy
`singularity` binary is recognised as a fallback when `apptainer` is
absent.

### No vendor files found

**Symptom:**
[`msConvertR()`](https://mstargetr.github.io/MStargetR/reference/msConvertR.md)
reports that no supported files were found.

**Solution:** Verify that vendor raw files are placed directly inside
the `raw_data` subdirectory of your project directory. Check that the
file extensions match supported formats (`.wiff`, `.wiff.scan`, `.raw`,
`.d`, etc.).

### PeakForgeR fails with a template mismatch

**Symptom:**
[`PeakForgeR()`](https://mstargetr.github.io/MStargetR/reference/PeakForgeR.md)
cannot match the MRM template to the data on a plate.

**Solution:** Use
[`transition_checkR()`](https://mstargetr.github.io/MStargetR/reference/transition_checkR.md)
and
[`compare_mrm_template_with_guide()`](https://mstargetr.github.io/MStargetR/reference/compare_mrm_template_with_guide.md)
to validate your templates before running the pipeline. Ensure that the
transition list matches the method used to acquire the data on that
plate.

### qcCheckR reports high missing values

**Symptom:** Many features are filtered out due to exceeding the
`mv_threshold`.

**Solution:** Consider increasing `mv_threshold` if your study design
expects a high proportion of missing values. Alternatively, inspect the
`PeakForgeR` reports to determine whether missing values result from
failed integrations, which may indicate issues with the MRM template or
chromatographic quality.

### batchCorrectR returns errors about QC samples

**Symptom:**
[`batchCorrectR()`](https://mstargetr.github.io/MStargetR/reference/batchCorrectR.md)
stops with an error about missing QC samples in one or more batches.

**Solution:** Ensure that every batch contains at least two QC samples
identified by the `qc_label` string in the `sample_type` column. The
correction model requires QC samples in each batch to estimate and
correct signal drift.

### ComBat method requires the sva package

**Symptom:**
[`batchCorrectR()`](https://mstargetr.github.io/MStargetR/reference/batchCorrectR.md)
or
[`qcCheckR()`](https://mstargetr.github.io/MStargetR/reference/qcCheckR.md)
stops with an error about the `sva` package not being installed.

**Solution:** Install `sva` from Bioconductor:

``` r

BiocManager::install("sva")
```

ComBat is an optional dependency. It is only required when you select
`method = "ComBat"` or `batch_method = "ComBat"`.

### QC-RLSC method requires the qcrlscR package

**Symptom:**
[`batchCorrectR()`](https://mstargetr.github.io/MStargetR/reference/batchCorrectR.md)
or
[`qcCheckR()`](https://mstargetr.github.io/MStargetR/reference/qcCheckR.md)
stops with an error about the `qcrlscR` package not being installed.

**Solution:** Install `qcrlscR` from CRAN:

``` r

install.packages("qcrlscR")
```

`qcrlscR` is an optional dependency. It is only required when you select
`method = "QCRLSC"` or `batch_method = "QCRLSC"`.

### Shiny application fails to launch

**Symptom:**
[`launchMStargetR()`](https://mstargetr.github.io/MStargetR/reference/launchMStargetR.md)
displays an error about missing packages.

**Solution:** Install the required GUI dependencies:

``` r

install.packages(c("shiny", "bslib", "DT", "shinyWidgets", "htmltools"))
```

------------------------------------------------------------------------

## Session Information

``` r

sessionInfo()
#> R version 4.6.0 (2026-04-24 ucrt)
#> Platform: x86_64-w64-mingw32/x64
#> Running under: Windows 11 x64 (build 26200)
#> 
#> Matrix products: default
#>   LAPACK version 3.12.1
#> 
#> locale:
#> [1] LC_COLLATE=English_Australia.utf8  LC_CTYPE=English_Australia.utf8   
#> [3] LC_MONETARY=English_Australia.utf8 LC_NUMERIC=C                      
#> [5] LC_TIME=English_Australia.utf8    
#> 
#> time zone: Australia/Perth
#> tzcode source: internal
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices utils     datasets  methods   base     
#> 
#> loaded via a namespace (and not attached):
#>  [1] bit_4.6.0         jsonlite_2.0.0    compiler_4.6.0    crayon_1.5.3     
#>  [5] tidyselect_1.2.1  parallel_4.6.0    jquerylib_0.1.4   systemfonts_1.3.2
#>  [9] textshaping_1.0.5 yaml_2.3.12       fastmap_1.2.0     readr_2.2.0      
#> [13] R6_2.6.1          knitr_1.51        htmlwidgets_1.6.4 tibble_3.3.1     
#> [17] desc_1.4.3        bslib_0.11.0      pillar_1.11.1     tzdb_0.5.0       
#> [21] rlang_1.2.0       utf8_1.2.6        cachem_1.1.0      xfun_0.57        
#> [25] fs_2.1.0          sass_0.4.10       bit64_4.8.2       otel_0.2.0       
#> [29] cli_3.6.6         pkgdown_2.2.0     magrittr_2.0.5    digest_0.6.39    
#> [33] vroom_1.7.1       hms_1.1.4         lifecycle_1.0.5   vctrs_0.7.3      
#> [37] evaluate_1.0.5    glue_1.8.1        ragg_1.5.2        rmarkdown_2.31   
#> [41] tools_4.6.0       pkgconfig_2.0.3   htmltools_0.5.9
```
