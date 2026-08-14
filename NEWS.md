# nlmixr2targets 0.1.0.9000

## New features

* `tar_nlmixr_multimodel()` estimation targets now announce themselves with
  `Model description: <list name>` before estimation starts, so `tar_make()`
  output identifies which model is running (the targets themselves are named
  after a hash of the model). Renaming a model does not invalidate its fit,
  and existing pipelines are not re-run by this change.

## Bug fixes

* Fits with `est = "vae"` are now cached correctly: the simplified dataset
  keeps the subject-constant columns that vae's automated covariate selection
  searches (as reported by `nlmixr2est::vaeCovariates()`, honoring the
  search-related `control` options), not just the covariates named in the
  model. Previously those candidate columns were dropped, so the covariate
  search ran on a reduced candidate set and editing such a column did not
  invalidate the cached fit (#39). Existing `_data_simple` targets will
  re-run once after upgrading because their command changed; downstream fit
  targets only re-run if the simplified data actually change.

* Parameter labels set with `label()` now show when printing a fit produced
  by `tar_nlmixr()` or `tar_nlmixr_multimodel()`. The labels are stripped
  before estimation (so that label-only edits do not invalidate the cached
  fit), and nlmixr2est bakes the printed parameter table at estimation time,
  so the final fit's table lacked its `Parameter` column even though the
  labels were restored on `fit$ui`. `nlmixr_object_complicate()` now also
  rebuilds the label column on the cached `parFixed`/`parFixedDf` tables.

* `tar_nlmixr_multimodel()` no longer fails when a model function that
  declares compartment initial conditions with `cmt(0) <- value` is both
  fit directly and piped through `ini()`/`model()` in the same call (e.g.
  `"myfit" = mod` alongside `"myfit pipe" = mod |> ini(a <- 2)`). Sharing the
  model function between entries previously left the piped entry's command
  referencing the internal `cmt(initial)` form, which nlmixr2 rejects (#37).

## New features

* `tar_nlmixr()` and `tar_nlmixr_multimodel()` gain an `error` argument. With
  the default `error = "stop"`, a model that fails during estimation halts
  `targets::tar_make()` exactly as before. With `error = "continue"`, the
  failure is caught and the target stores a failure sentinel (an object of
  class `nlmixr2targetsError`, which also inherits from `"try-error"`) carrying
  the error message, so one failed model does not stop the rest of the
  pipeline (#35).

# nlmixr2targets 0.1.0

## Breaking changes from unreleased development version

* `tar_nlmixr()` now produces intermediate targets named
  `<name>_object_simple`, `<name>_data_simple`, and `<name>_fit_simple`
  (single-underscore separator). Prior versions used the longer
  `<name>_tar_object_simple`/`_tar_data_simple`/`_tar_fit_simple`
  pattern. The new names align with the convention already used by
  `tar_nlmixr_multimodel()`. Users with cached pipelines from earlier
  versions will see one rebuild (the old targets become orphans);
  `targets::tar_destroy()` will remove them if desired.
