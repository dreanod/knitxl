
# knitxl

<!-- badges: start -->
[![R-CMD-check](https://github.com/dreanod/knitxl/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/dreanod/knitxl/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of knitxl is to transform an Rmarkdown file into an xlsx file. It is 
based on the `knitr` package to parse the Rmd files and evaluate code chunks.
The package comes with the possibility to fully customize the style of the
xlsx file.

## Installation

You can install the development version of knitxl from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("dreanod/knitxl")
```

## Example

This knit an example Rmd file into an excel file.

``` r
library(knitxl)
knitxl(system.file("examples/minimal-example.Rmd"), package = "knitxl")
```

