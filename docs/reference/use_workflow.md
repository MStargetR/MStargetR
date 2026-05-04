# Use an MStargetR Workflow Template

Lists available workflow templates or copies one to a specified
directory for customisation. Workflow templates are pre-configured R
Markdown files that demonstrate common MStargetR pipelines.

## Usage

``` r
use_workflow(
  workflow = NULL,
  output_dir = getwd(),
  open = interactive() && requireNamespace("rstudioapi", quietly = TRUE)
)
```

## Arguments

- workflow:

  Character string specifying which workflow to use. Use `NULL` (the
  default) to list all available workflows.

- output_dir:

  Character string specifying the directory to copy the workflow into.
  Defaults to the current working directory.

- open:

  Logical indicating whether to open the file after copying. Defaults to
  `TRUE` if running in RStudio, `FALSE` otherwise.

## Value

If `workflow` is `NULL`, returns a character vector of available
workflow names (invisibly) and prints them. If a workflow is specified,
returns the path to the copied file (invisibly).

## Details

Available workflows:

- `"generic"`:

  A template for any user with placeholder parameters for project path,
  QC label, MRM templates, and sample tags.

- `"CCSM"`:

  Pre-configured workflow for ANPC CCSM lipidomics with built-in MRM
  templates. Includes single-project and multi-project batch processing
  examples.

## Examples

``` r
if (FALSE) { # \dontrun{
# List available workflows
use_workflow()

# Copy the generic workflow to current directory
use_workflow("generic")

# Copy the CCSM workflow to a specific directory
use_workflow("CCSM", output_dir = "~/my_project")
} # }
```
