
<!-- README.md is generated from README.Rmd. Please edit that file -->

# nlmixrtargets

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/nlmixrtargets)](https://CRAN.R-project.org/package=nlmixrtargets)
[![R
Targetopia](https://img.shields.io/badge/R_Targetopia-member-blue?style=flat&labelColor=gray)](https://wlandau.github.io/targetopia/)
[![R-CMD-check](https://github.com/nlmixrdevelopment/nlmixrtargets/workflows/R-CMD-check/badge.svg)](https://github.com/nlmixrdevelopment/nlmixrtargets/actions)
<!-- badges: end -->

The goal of nlmixrtargets is to simplify the use of the
[nlmixr](https://github.com/nlmixrdevelopment/nlmixr) package with the
[targets](https://docs.ropensci.org/targets/) package. nlmixrtargets is
part of the
[targetopeia](https://wlandau.github.io/targetopia/packages.html).

## Installation

When available on CRAN, you can install the released version of
nlmixrtargets from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("nlmixrtargets")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("nlmixrdevelopment/nlmixrtargets")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(targets)
targets::tar_script({
  library(nlmixrtargets)
  pheno <- function() {
    ini({
      tcl <- log(0.008) # typical value of clearance
      tv <-  log(0.6)   # typical value of volume
      ## var(eta.cl)
      eta.cl + eta.v ~ c(1,
                         0.01, 1) ## cov(eta.cl, eta.v), var(eta.v)
      # interindividual variability on clearance and volume
      add.err <- 0.1    # residual variability
    })
    model({
      cl <- exp(tcl + eta.cl) # individual value of clearance
      v <- exp(tv + eta.v)    # individual value of volume
      ke <- cl / v            # elimination rate constant
      d/dt(A1) = - ke * A1    # model differential equation
      cp = A1 / v             # concentration in plasma
      cp ~ add(add.err)       # define error model
    })
  }
  list(
    tar_nlmixr(name=pheno_model, object=pheno, data=nlmixr::pheno_sd, est="saem")
  )
})
targets::tar_make()
```
