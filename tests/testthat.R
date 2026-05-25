library(testthat)
# Load targets to work around https://github.com/ropensci/targets/issues/642
library(targets)
library(nlmixr2targets)

# Remove to replace with on_cran if exported,
# https://github.com/r-lib/testthat/issues/2336
on_cran_local <- function () {
    env <- Sys.getenv("NOT_CRAN")
    if (identical(env, "")) {
        !interactive()
    }
    else {
        !isTRUE(as.logical(env))
    }
}

if (on_cran_local()) {
  rxode2::setRxThreads(1L)
}
test_check("nlmixr2targets")
