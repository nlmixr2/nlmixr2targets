## Responses to prior submission

* I updated testing so that it should run serially rather than with multiple
  threads. This should fix the 3.2-fold number of cores issue found with the
  prior submission.

## Test environments

* Local: Linux (Ubuntu) with the current R release
* GitHub Actions (https://github.com/nlmixr2/nlmixr2targets/actions):
  - ubuntu-latest, R-release
  - ubuntu-latest, R-devel
  - ubuntu-latest, R-oldrel-1
  - macos-latest, R-release
  - windows-latest, R-release

## R CMD check results

0 errors | 0 warnings | 0 notes

* The user-facing examples (`tar_nlmixr()`, `tar_nlmixr_multimodel()`) only
  exercise the target construction step; they capture the `data` and `est`
  arguments as expressions and return a list of targets without running
  estimation. Estimation only happens when the user calls
  `targets::tar_make()` from a project whose targets store they have
  configured (e.g. via `targets::tar_config_set()` pointing at a directory
  under `tempdir()`). Comprehensive run-through examples live in the
  vignettes.
* This is a new release.
