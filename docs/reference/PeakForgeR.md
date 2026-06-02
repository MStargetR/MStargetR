# Peak Picking and Integration via Skyline in Docker

This function performs peak picking and integration via Skyline in a
Docker image. Allowing for usage across all major OS systems.

We strongly recommend checking your mrm transition list using
MStargetR::transition_checkR prior to using it in PeakForgeR

If the user has not used MStargetR::msConvertR to convert vendor files
please ensure each plate folder exists under the project directory with
mzML files located at `<project_directory>/<plateID>/data/mzml/*.mzML`.
The `plateID_outputs` parameter must be supplied to identify the plate
folders.

## Usage

``` r
PeakForgeR(
  user_name,
  project_directory,
  mrm_template_list = NULL,
  QC_sample_label = NULL,
  plateID_outputs = NULL,
  enable_HPC = getOption("MStargetR.enable_HPC", FALSE)
)
```

## Arguments

- user_name:

  A character string to identify user.

- project_directory:

  A path to project directory

- mrm_template_list:

  Path to MRM transition list, must be in specified format. See examples
  and run load example mrm_guide for structure. May contain more than
  one template for multi-method projects.

- QC_sample_label:

  User specified tag to filter QC samples. Character case is not
  sensitive

  E.g. "JANE_C5_URI_MS-LIPIDS_PLIP01_PLATE_3-PLASMA LTR_19.mzML"

  QC_sample_label = "LTR" to target files containing LTR for QC.

- plateID_outputs:

  A vector of character strings specifying plateIDs for project. This
  parameter must only be specified by users who have not used
  MStargetR::msConvertR..... Default is NULL

  These must match mzml files. e.g. If you have two plates:

  - JANE_DOE_C5_URI_MS-LIPIDS_PLATE_1-PLASMA_sample_1.mzML

  - JANE_DOE_C5_URI_MS-LIPIDS_PLATE_2-PLASMA_sample_1.mzML

  An appropriate input would be:

  - plateID_outputs = c("JANE_DOE_C5_URI_MS-LIPIDS_PLATE_1",
    "JANE_DOE_C5_URI_MS-LIPIDS_PLATE_2")

- enable_HPC:

  Logical. When `TRUE`, Skyline is run via Apptainer (Singularity)
  instead of Docker. This is the intended runtime on HPC clusters where
  Docker is typically forbidden. The default is
  `getOption("MStargetR.enable_HPC", FALSE)` so HPC users can set
  `options(MStargetR.enable_HPC = TRUE)` once in their `.Rprofile` and
  never pass the argument explicitly. See the "Running on HPC" section
  of the README for SIF setup instructions.

## Value

A curated project directory with sub folders for each plate containing
Skyline exports.

## Details

- **Input Validation:**

  - Validate project_directory

  - Validate mrm_template_list

- **File Handling:**

  - Set plateIDs from either plate MStargetR::msConvertR or user
    specified plateID_outputs

- **Processing Plates:**

  - For each plateID:

    - Setup project structure

    - Import mzml files

    - QC optimised retention times

    - QC optimised peak boundaries

    - Peak picking/integration with Skyline MS through docker

- **Final Cleanup:**

  - Archive raw files

  - Message about availability of chromatograms and reports

## Examples

``` r
if (FALSE) { # \dontrun{
#Load example mrm_guide
  file_path <- system.file("extdata", "LGW_lipid_mrm_template_v1.tsv", package = "MStargetR")
  example_mrm_template <- readr::read_tsv(file_path)

#Default (Docker) on a workstation
PeakForgeR(user_name = "Mad_max",
           project_directory = "USER/PATH/TO/PROJECT/DIRECTORY",
           mrm_template_list = list("User/path/to/user_mrm_guide_v1.tsv",
                                    "user/path/to/user_mrm_guide_v2.tsv"),
           QC_sample_label = "LTR",
           plateID_outputs = NULL
          )

# HPC (Apptainer): pre-pull the SIF on a login node, then run a job:
#   apptainer pull mstargetr-pwiz.sif \
#     docker://proteowizard/pwiz-skyline-i-agree-to-the-vendor-licenses:<tag>
options(
  MStargetR.enable_HPC = TRUE,
  MStargetR.sif_path   = "/scratch/me/mstargetr-pwiz.sif"
)
PeakForgeR(user_name         = "Mad_max",
           project_directory = "/scratch/me/MyProject",
           mrm_template_list = list("/scratch/me/templates/lipid_mrm_v1.tsv"),
           QC_sample_label   = "LTR")
} # }
```
