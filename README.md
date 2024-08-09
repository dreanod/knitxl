
# knitxl

<!-- badges: start -->
[![R-CMD-check](https://github.com/dreanod/knitxl/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/dreanod/knitxl/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`knitxl` converts an Rmarkdown file into an `.xlsx` file. It uses 
[`knitr`](https://yihui.org/knitr/) to parse the Rmd files and evaluate code 
chunks. `knitx` comes with features to customize the output `.xlsx` file.

## Example

This will convert an `.Rmd` file into an `.xlsx` file:

```r
library(knitxl)
path_to_rmd_file <- system.file("examples/knitxl-minimal.Rmd", package = "knitxl")
knitxl(path_to_rmd_file)
```

## Installation

### From CRAN

Most users should install the latest release version of `knitxl` from 
[CRAN](https://cran.r-project.org) with:

```r
install.packages("knitxl")
```

### From GitHub

You can install the development version of knitxl from 
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("dreanod/knitxl")
```

## How `knitxl` works

`knitxl` relies on [`knitr`](https://yihui.org/knitr/) to parse an `.Rmd` file 
and evaluate the code chunks. `knitxl` attaches special 
[hooks](https://yihui.org/knitr/hooks/)  to `knitr`, which will save the 
text, source code, and results of `knitr` evaluation to an `R6` object that 
represents the content of an `.xlsx` file. 
[`openxlsx`](https://ycphs.github.io/openxlsx/index.html) is used for creating, 
writing, styling and saving this object to the `.xlsx` output. 

