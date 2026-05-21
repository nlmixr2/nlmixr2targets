## Test environments

* Local: Linux (Ubuntu) with the current R release
* GitHub Actions (https://github.com/nlmixr2/nlmixr2targets/actions):
  - ubuntu-latest, R-release
  - ubuntu-latest, R-devel
  - ubuntu-latest, R-oldrel-1
  - macos-latest, R-release
  - windows-latest, R-release
* Win-builder via `devtools::check_win_devel()` and `devtools::check_win_release()`
  (URLs to be appended immediately before submission)

## R CMD check results

0 errors | 0 warnings | 0 notes

* Even simple workflows for the `nlmixr2targets` library take significant
  runtime, more than the few seconds allowed for examples. The user-facing
  function examples are therefore wrapped in `\dontrun{}` and the
  comprehensive examples live in the vignettes.
* This is a new release.
