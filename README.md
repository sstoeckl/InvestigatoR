
<!-- README.md is generated from README.Rmd. Please edit that file -->

# InvestigatoR

<!-- badges: start -->
<!-- badges: end -->

The goal of InvestigatoR is to …

## Installation

You can install the development version of InvestigatoR from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ericschumann12/InvestigatoR")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(InvestigatoR)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
performance(c(1,2,NA,3))
#> [1] 2
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
