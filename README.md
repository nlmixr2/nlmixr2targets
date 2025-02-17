
<!-- README.md is generated from README.Rmd. Please edit that file -->

# nlmixr2targets

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/nlmixr2targets)](https://CRAN.R-project.org/package=nlmixr2targets)
[![R
Targetopia](https://img.shields.io/badge/R_Targetopia-member-blue?style=flat&labelColor=gray)](https://wlandau.github.io/targetopia/)
[![R-CMD-check](https://github.com/nlmixr2/nlmixr2targets/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/nlmixr2/nlmixr2targets/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/nlmixr2/nlmixr2targets/graph/badge.svg)](https://app.codecov.io/gh/nlmixr2/nlmixr2targets)
<!-- badges: end -->

The goal of nlmixr2targets is to simplify the use of the
[nlmixr2](https://github.com/nlmixr2/nlmixr2) package with the
[targets](https://docs.ropensci.org/targets/) package. nlmixr2targets is
part of the
[targetopeia](https://wlandau.github.io/targetopia/packages.html).

## Installation

When available on CRAN, you can install the released version of
nlmixr2targets from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("nlmixr2targets")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("nlmixr2/nlmixr2targets")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(targets)
targets::tar_script({
  library(nlmixr2targets)
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
    tar_nlmixr(name=pheno_model, object=pheno, data=nlmixr2data::pheno_sd, est="saem")
  )
})
targets::tar_make()
```
