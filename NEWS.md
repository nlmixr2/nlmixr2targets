# nlmixr2targets 0.1.0.9000

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
