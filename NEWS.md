# nlmixr2targets 0.0.0.9000

* Setting initial conditions can now be done with `cmt(initial)` which will be
  automatically recoded to `cmt(0)`.
* Added "cens" and "limit" columns as nlmixr2 columns.
* Keep columns from the `table` argument's `keep` element.
* Indirect cache (`save_nlmixr2obj_indirect()` / `read_nlmixr2obj_indirect()`)
  now reports a clear error mentioning the missing hash and cache directory
  on cache misses, instead of bubbling up a `readRDS()` failure.
* `tar_nlmixr()`, `tar_nlmixr_multimodel()`, `assign_origData()`, and
  `nlmixr_data_simplify()` now validate their key arguments at function
  entry so misuse fails loudly at the call site.
