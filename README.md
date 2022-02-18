
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![R-CMD-check](https://github.com/SDCA-tool/sdca-package/workflows/R-CMD-check/badge.svg)](https://github.com/SDCA-tool/sdca-package/actions)
<!-- badges: end -->

# SDCA

The R Package for the SDCA Project

## Installation

Ensure you have system dependencies:

`sudo apt-get install -y libssl-dev libcurl4-openssl-dev libxml2-dev
libudunits2-dev libgdal-dev`

and R dependences:

    R -e 'if (!require("units")) install.packages("units");'
    R -e 'if (!require("sf")) install.packages("sf");'

Install the package with **remotes** as follows:

``` r
install.packages("remotes")
remotes::install_github("SDCA-tool/sdca-package")
```

## Use

The primary use case of the package is working within the SDCA website.
However users can test the code locally by drawing an intervention on
the [website](dev.carbon.place), downloading the GeoJSON then.

``` r
library(sdca)
dat <- geojson_api(path = "carbon-calculator-scheme-intervention.geojson")

# Change the paths to the rasters
# Note that the package only comes with sample rasters for Bristol
# Full rasters are at https://github.com/SDCA-tool/sdca-data/releases/tag/map_data
dat$path_dem <- paste0(.libPaths()[1],"/sdca/tests/testthat/dem.tif")
dat$path_landcover <- paste0(.libPaths()[1],"/sdca/tests/testthat/landcover.tif")
dat$path_bedrock <- paste0(.libPaths()[1],"/sdca/tests/testthat/bedrock.tif")
dat$path_superficial <- paste0(.libPaths()[1],"/sdca/tests/testthat/superficial.tif")

# Process the input data using local = TRUE
results <- process_results(dat, local = TRUE)
```
