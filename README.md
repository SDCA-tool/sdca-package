
<!-- README.md is generated from README.Rmd. Please edit that file -->

# SDCA

The R Package for the SDCA Project

## Installation

Ensure you have system dependencies:

```sudo apt-get install -y libssl-dev libcurl4-openssl-dev libxml2-dev```

Install the package with **remotes** as follows:

``` r
install.packages("remotes")
remotes::install_github("SDCA-tool/sdca-package")
```

## Use

``` r
library(sdca)
test_function(5)
# 25
test_function_fail(5)
# Error in test_function_fail(5) : This function will fail
```
