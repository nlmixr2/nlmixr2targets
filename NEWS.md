# nlmixr2targets 0.0.0.9000

* Setting initial conditions can now be done with the natural
  `cmt(0) <- value` form directly inside `model({...})` blocks.
  `tar_nlmixr()` rewrites the user's model function body in env to
  use `cmt(initial) <- value` (codetools-safe) at construction time
  and restores `cmt(0) <- value` before nlmixr2 sees the model. The
  pipe forms `pheno |> model({...})` and `pheno |> ini(...)` are
  also supported via a runtime delayed-eval wrapper. Note: the
  in-env rewrite mutates the user's binding, so calling the model
  function directly (outside `tar_make()`) after `tar_nlmixr()`
  will see the rewritten body. Functions with `cmt(0)` declared in
  env but never routed through `tar_nlmixr()` still trigger
  `codetools::findGlobals()` errors during `targets` analysis --
  this is a known limitation, pinned by a test.
* Setting initial conditions can also still be done with
  `cmt(initial)` directly, which is automatically recoded to `cmt(0)`.
* Added "cens" and "limit" columns as nlmixr2 columns.
* Keep columns from the `table` argument's `keep` element.
* Indirect cache (`save_nlmixr2obj_indirect()` / `read_nlmixr2obj_indirect()`)
  now reports a clear error mentioning the missing hash and cache directory
  on cache misses, instead of bubbling up a `readRDS()` failure.
* `tar_nlmixr()`, `tar_nlmixr_multimodel()`, `assign_origData()`, and
  `nlmixr_data_simplify()` now validate their key arguments at function
  entry so misuse fails loudly at the call site.
* Object metadata (`ui$meta`) and parameter labels (`ui$iniDf$label`)
  are now stripped from the simplified model before fitting and
  re-attached to the final fit by the new `nlmixr_object_complicate()`
  helper, which also re-attaches the original data. Editing only labels
  or metadata in the source model no longer invalidates the cached
  `_fit_simple` target. (#28)
