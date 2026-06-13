# Simplify an nlmixr object

This function is typically not needed by end users.

## Usage

``` r
nlmixr_object_simplify(
  object,
  directory = file.path(targets::tar_config_get("store"), "user/nlmixr2")
)
```

## Arguments

- object:

  Fitted object or function specifying the model.

- directory:

  Cache directory to load the simplified `nlmixrui` from. Defaults to
  `file.path(targets::tar_config_get("store"), "user/nlmixr2")`,
  mirroring the convention used by `targets`' own
  `store = targets::tar_config_get("store")` defaults — the path
  resolves at call time against whatever store the user has configured
  for the running
  [`tar_make()`](https://docs.ropensci.org/targets/reference/tar_make.html)
  (e.g. a [`tempdir()`](https://rdrr.io/r/base/tempfile.html) location
  set via
  [`targets::tar_config_set()`](https://docs.ropensci.org/targets/reference/tar_config_set.html)).

## Value

The MD5 hash used to load the simplified `nlmixrui` object back from the
`nlmixr2targets` indirect cache.

## Details

The object simplification removes comments (so please use `label()`
instead of comments to label parameters) and then converts the `object`
to a "nlmixrui" object.

Object metadata (`ui$meta`) and parameter labels (`ui$iniDf$label`) are
also stripped from the simplified object before it is written to the
indirect cache. They do not affect estimation, and stripping them keeps
the cache hash stable across edits to either, so editing only labels or
metadata will not invalidate the cached fit. The stripped values are
restored on the final fit by
[`nlmixr_object_complicate()`](https://nlmixr2.github.io/nlmixr2targets/reference/nlmixr_object_complicate.md),
which reads them straight back off the original model.

The natural nlmixr2 DSL form for compartment initial conditions
(`cmt(0) <- value` inside a `model({...})` block) trips `targets`'
static analysis because
[`codetools::findGlobals()`](https://rdrr.io/pkg/codetools/man/findGlobals.html)
interprets it as a replacement-function assignment with a non-symbol
target.
[`tar_nlmixr()`](https://nlmixr2.github.io/nlmixr2targets/reference/tar_nlmixr.md)
auto-rewrites `cmt(0) <- value` to `cmt(initial) <- value` inside
`model({...})` blocks at construction time (mutating the user's model
function in env), and converts back to `cmt(0) <- value` before nlmixr2
sees the model. The user can therefore write `cmt(0) <- value` directly.

Manual `cmt(initial) <- value` is also accepted, but it is a
`nlmixr2targets`-only workaround: bare nlmixr2 does not understand the
`cmt(initial)` form, so a model function written this way only fits when
routed through
[`tar_nlmixr()`](https://nlmixr2.github.io/nlmixr2targets/reference/tar_nlmixr.md)
/
[`tar_nlmixr_multimodel()`](https://nlmixr2.github.io/nlmixr2targets/reference/tar_nlmixr_multimodel.md).
Note: because the rewrite mutates the function in env, calling the model
function directly (outside
[`tar_make()`](https://docs.ropensci.org/targets/reference/tar_make.html))
after
[`tar_nlmixr()`](https://nlmixr2.github.io/nlmixr2targets/reference/tar_nlmixr.md)
will see `cmt(initial)` in its body.

The simplified model's `model.name` is always set to `"object"`. This
keeps the simplified output stable so that the MD5 hash used by the
`targets` indirect cache is independent of the symbol the caller bound
the model function to.

## See also

[`nlmixr_object_complicate()`](https://nlmixr2.github.io/nlmixr2targets/reference/nlmixr_object_complicate.md)
for the inverse operation that re-attaches labels, metadata, and the
original data on the final fit.

Other Simplifiers:
[`nlmixr_data_simplify()`](https://nlmixr2.github.io/nlmixr2targets/reference/nlmixr_data_simplify.md),
[`nlmixr_object_complicate()`](https://nlmixr2.github.io/nlmixr2targets/reference/nlmixr_object_complicate.md)
