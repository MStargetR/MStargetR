# Convert Vendor Mass Spectrometry Files to mzML Format

Converts raw vendor mass spectrometry files (e.g. `.wiff`, `.raw`, `.d`)
to open `.mzML` format using ProteoWizard's `msconvert` tool running
inside a Docker container. The function validates inputs, manages Docker
execution, and organises the resulting files into a standardised project
directory structure.

## Usage

``` r
msConvertR(
  input_directory,
  output_directory,
  manifest = NULL,
  enable_HPC = getOption("MStargetR.enable_HPC", FALSE),
  ...
)
```

## Arguments

- input_directory:

  A character string specifying the path to the directory containing
  vendor files to convert.

- output_directory:

  A character string specifying the path to the directory where the
  converted `.mzML` files and project structure will be created.

- manifest:

  Optional. Either a path to a CSV file or a `data.frame` mapping each
  raw vendor file to a plate, with (case-insensitive) columns `raw_file`
  and `plateID`. Use this last-resort override when plate membership for
  one-file-per-sample formats (e.g. `.d`, `.raw`) cannot be recovered
  automatically. When `NULL` (default), plate membership is resolved
  automatically in priority order: a remembered `plate_grouping.csv` at
  the project root; per-plate subfolders under `raw_data/` (a file in
  `raw_data/<plateID>/` belongs to that plate); filename-based
  auto-discovery, which infers the plate from the filename token
  structure for sample-level files left flat in `raw_data/` and reports
  the inference (persisting it to an editable `plate_grouping.csv` for
  confirmation); and finally the bare filename. Sample-level files that
  even auto-discovery cannot group are converted with a warning (each
  becomes its own plate) rather than rejected. A multi-sample `.wiff` is
  always one plate.

- enable_HPC:

  Logical. When `TRUE`, the ProteoWizard container is invoked via
  Apptainer (Singularity) instead of Docker. This is the intended
  runtime on HPC clusters where Docker is typically forbidden. The
  default is `getOption("MStargetR.enable_HPC", FALSE)` so HPC users can
  set `options(MStargetR.enable_HPC = TRUE)` once in their `.Rprofile`
  and never pass the argument explicitly. See the "Running on HPC"
  section of the README for SIF setup instructions.

- ...:

  Reserved for forward compatibility. Any unrecognised named arguments
  trigger a warning and are otherwise ignored.

## Value

Called for its side effects. The function creates a project directory
structure containing converted `.mzML` files organised by plate.
Invisibly returns `NULL`.

## Details

- **Input Validation:**

  - Validate input_directory

  - Validate presence of supported vendor file types

- **Plate Identification:**

  - Extract plateIDs from vendor file names

  - Remove vendor-specific extensions

- **Docker Setup:**

  - Check Docker installation and running status

- **File Conversion:**

  - Convert vendor files to mzML format using ProteoWizard's msconvert

  - Handle errors gracefully with tryCatch

- **Directory Structuring:**

  - Create project structure for converted files

  - Relocate vendor files based on input/output directory configuration

- **User Messaging:**

  - Notify user of conversion status and file locations

  - Provide guidance on directory structure

## Examples

``` r
if (FALSE) { # \dontrun{
# Default (Docker) on a workstation
msConvertR(input_directory  = "path/to/input_directory",
           output_directory = "path/to/output_directory")

# HPC (Apptainer): pre-pull the SIF on a login node, then run a job:
#   apptainer pull mstargetr-pwiz.sif \
#     docker://proteowizard/pwiz-skyline-i-agree-to-the-vendor-licenses:<tag>
options(
  MStargetR.enable_HPC = TRUE,
  MStargetR.sif_path   = "/scratch/me/mstargetr-pwiz.sif"
)
msConvertR(input_directory  = "/scratch/me/MyProject/raw_data",
           output_directory = "/scratch/me/MyProject")

# Or enable HPC mode for a single call without setting the option:
msConvertR(input_directory  = "/scratch/me/MyProject/raw_data",
           output_directory = "/scratch/me/MyProject",
           enable_HPC       = TRUE)
 } # }
```
