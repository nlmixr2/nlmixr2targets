library(testthat)
# Load targets to work around https://github.com/ropensci/targets/issues/642
library(targets)
library(nlmixr2targets)

# Remove to replace with on_cran if exported,
# https://github.com/r-lib/testthat/issues/2336
on_cran_local <- function() {
  env <- Sys.getenv("NOT_CRAN")
  if (identical(env, "")) {
    !interactive()
  } else {
    !isTRUE(as.logical(env))
  }
}

on_ci <- function() {
  isTRUE(as.logical(Sys.getenv("CI")))
}

# Limit rxode2/nlmixr2est to a single thread on CRAN and on CI. nlmixr2est
# 6.0.1 (the current CRAN binary) has a Windows cores>=2 cross-DLL OpenMP
# heap-corruption segfault, fixed upstream in nlmixr2est >= 6.0.2, that
# hard-crashes the test run on windows-latest. Single-threaded estimation
# avoids the parallel region entirely. CRAN already limits cores, so this
# only changes behaviour on CI (where NOT_CRAN is set, so on_cran_local() is
# FALSE); local interactive runs are unaffected. Remove the on_ci() guard
# once CRAN ships nlmixr2est >= 6.0.2.
if (on_cran_local() || on_ci()) {
  rxode2::setRxThreads(1L)
}
test_check("nlmixr2targets")
