# Convert Vendor Mass Spectrometry Files to mzML Format

Converts raw vendor mass spectrometry files (e.g. `.wiff`, `.raw`, `.d`)
to open `.mzML` format using ProteoWizard's `msconvert` tool running
inside a Docker container. The function validates inputs, manages Docker
execution, and organises the resulting files into a standardised project
directory structure.

## Usage

``` r
msConvertR(input_directory, output_directory)
```

## Arguments

- input_directory:

  A character string specifying the path to the directory containing
  vendor files to convert.

- output_directory:

  A character string specifying the path to the directory where the
  converted `.mzML` files and project structure will be created.

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
# example code
 msConvertR(input_directory = "path/to/input_directory",
            output_directory = "path/to/output_directory")
 } # }
```
