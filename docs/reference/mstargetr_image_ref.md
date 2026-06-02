# Full ProteoWizard image reference (`name:tag`).

Single source of truth for the image string used by Docker and as the
source for `apptainer pull docker://...`. Reads the tag resolved at
package load (see `MSTARGETR_DOCKER_IMAGE_TAG` in `R/config.R`).

## Usage

``` r
mstargetr_image_ref()
```
