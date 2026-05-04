# Launch MStargetR Shiny Application

Opens the MStargetR interactive Shiny application for targeted MRM mass
spectrometry data processing and quality control.

## Usage

``` r
launchMStargetR(port = NULL, launch.browser = TRUE, host = "127.0.0.1")
```

## Arguments

- port:

  Integer specifying the port number. Default is determined by Shiny.

- launch.browser:

  Logical indicating whether to open the app in a web browser. Default
  is `TRUE`.

- host:

  Character string specifying the host address. Default is `"127.0.0.1"`
  for local-only access. Use `"0.0.0.0"` to allow connections from other
  machines on the network.

## Value

This function does not return a value. It launches a Shiny application
and blocks the R session until the app is closed.

## Details

The GUI requires several additional packages beyond the core MStargetR
dependencies: shiny, bslib, DT, shinyWidgets, and htmltools. If any are
missing, the function will display an informative message listing the
packages to install.

## Examples

``` r
if (FALSE) { # \dontrun{
launchMStargetR()
launchMStargetR(host = "0.0.0.0", port = 3838)
} # }
```
