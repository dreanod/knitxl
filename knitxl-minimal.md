# A Minimal Example for Markdown

This is a minimal example of using **knitxl** to produce an _XLSX_ spreadsheet
from _Markdown_.

## R code chunks

# set global chunk options: images will be 7x5 inchesknitr::opts_chunk$set(fig.width=7, fig.height=5)options(digits = 4)

Now we write some code chunks in this markdown file:

x <- 1+1 # a simple calculatorset.seed(123)rnorm(5)  # boring random numbers## [1] -0.56048 -0.23018  1.55871  0.07051  0.12929


We can also produce plots:

par(mar = c(4, 4, .1, .1))with(mtcars, {
  plot(mpg~hp, pch=20, col='darkgray')
  lines(lowess(hp, mpg))
})figure/graphics-1.pdf

## Inline code

Inline R code is also supported, e.g. the value of `x` is 2, and 2 &times; &pi;
= 6.28318530717959.

## Math

LaTeX math as usual: $f(\alpha, \beta) \propto x^{\alpha-1}(1-x)^{\beta-1}$.

## Misc

You can indent code chunks so they can nest within other environments such as lists.

1. the area of a circle with radius x
pi * x^2## [1] 12.57

2. OK, that is great

To compile me, use

library(knitxl)knitxl('knitxl-minimal.Rmd')

## Conclusion

Markdown is super easy to write. Go to **knitr** [homepage](https://yihui.org/knitr/) for details.
