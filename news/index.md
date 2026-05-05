# Changelog

## nlmixr2targets 0.0.0.9000

- Setting initial conditions can now be done with `cmt(initial)` which
  will be automatically recoded to `cmt(0)`.
- Added “cens” and “limit” columns as nlmixr2 columns.
- Keep columns from the `table` argument’s `keep` element.
- Indirect cache (`save_nlmixr2obj_indirect()` /
  `read_nlmixr2obj_indirect()`) now reports a clear error mentioning the
  missing hash and cache directory on cache misses, instead of bubbling
  up a [`readRDS()`](https://rdrr.io/r/base/readRDS.html) failure.
- [`tar_nlmixr()`](https://nlmixr2.github.io/nlmixr2targets/reference/tar_nlmixr.md),
  [`tar_nlmixr_multimodel()`](https://nlmixr2.github.io/nlmixr2targets/reference/tar_nlmixr_multimodel.md),
  [`assign_origData()`](https://nlmixr2.github.io/nlmixr2targets/reference/assign_origData.md),
  and
  [`nlmixr_data_simplify()`](https://nlmixr2.github.io/nlmixr2targets/reference/nlmixr_data_simplify.md)
  now validate their key arguments at function entry so misuse fails
  loudly at the call site.
- Object metadata (`ui$meta`) and parameter labels (`ui$iniDf$label`)
  are now stripped from the simplified model before fitting and
  re-attached to the final fit by the new
  [`nlmixr_object_complicate()`](https://nlmixr2.github.io/nlmixr2targets/reference/nlmixr_object_complicate.md)
  helper, which also re-attaches the original data. Editing only labels
  or metadata in the source model no longer invalidates the cached
  `_fit_simple` target.
  ([\#28](https://github.com/nlmixr2/nlmixr2targets/issues/28))
