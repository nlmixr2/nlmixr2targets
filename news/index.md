# Changelog

## nlmixr2targets 0.1.0.9000

### Bug fixes

- [`tar_nlmixr_multimodel()`](https://nlmixr2.github.io/nlmixr2targets/reference/tar_nlmixr_multimodel.md)
  no longer fails when a model function that declares compartment
  initial conditions with `cmt(0) <- value` is both fit directly and
  piped through `ini()`/`model()` in the same call (e.g. `"myfit" = mod`
  alongside `"myfit pipe" = mod |> ini(a <- 2)`). Sharing the model
  function between entries previously left the piped entry’s command
  referencing the internal `cmt(initial)` form, which nlmixr2 rejects
  ([\#37](https://github.com/nlmixr2/nlmixr2targets/issues/37)).

### New features

- [`tar_nlmixr()`](https://nlmixr2.github.io/nlmixr2targets/reference/tar_nlmixr.md)
  and
  [`tar_nlmixr_multimodel()`](https://nlmixr2.github.io/nlmixr2targets/reference/tar_nlmixr_multimodel.md)
  gain an `error` argument. With the default `error = "stop"`, a model
  that fails during estimation halts
  [`targets::tar_make()`](https://docs.ropensci.org/targets/reference/tar_make.html)
  exactly as before. With `error = "continue"`, the failure is caught
  and the target stores a failure sentinel (an object of class
  `nlmixr2targetsError`, which also inherits from `"try-error"`)
  carrying the error message, so one failed model does not stop the rest
  of the pipeline
  ([\#35](https://github.com/nlmixr2/nlmixr2targets/issues/35)).

## nlmixr2targets 0.1.0

CRAN release: 2026-06-05

### Breaking changes from unreleased development version

- [`tar_nlmixr()`](https://nlmixr2.github.io/nlmixr2targets/reference/tar_nlmixr.md)
  now produces intermediate targets named `<name>_object_simple`,
  `<name>_data_simple`, and `<name>_fit_simple` (single-underscore
  separator). Prior versions used the longer
  `<name>_tar_object_simple`/`_tar_data_simple`/`_tar_fit_simple`
  pattern. The new names align with the convention already used by
  [`tar_nlmixr_multimodel()`](https://nlmixr2.github.io/nlmixr2targets/reference/tar_nlmixr_multimodel.md).
  Users with cached pipelines from earlier versions will see one rebuild
  (the old targets become orphans);
  [`targets::tar_destroy()`](https://docs.ropensci.org/targets/reference/tar_destroy.html)
  will remove them if desired.
