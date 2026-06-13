# Changelog

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
