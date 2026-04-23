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

## Network deployment

When using `host = "0.0.0.0"` to expose the app on a network, be aware
that Shiny serves plain HTTP with no built-in authentication. For
production or multi-user deployments you should:

- Place the app behind a reverse proxy (e.g. nginx, Apache) that
  provides HTTPS/TLS encryption.

- Configure authentication on the reverse proxy or restrict access via
  VPN. Without this, anyone on the network can access the pipeline and
  upload files.

- The upload size limit is automatically reduced to 500 MB when
  `host = "0.0.0.0"` (versus 2 GB for localhost) as an additional
  safeguard.

## Examples

``` r
if (FALSE) { # \dontrun{
launchMStargetR()
launchMStargetR(host = "0.0.0.0", port = 3838)
} # }
```
