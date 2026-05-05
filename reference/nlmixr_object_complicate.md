# Re-attach labels, metadata, and original data to a simplified fit

The inverse of
[`nlmixr_object_simplify()`](https://nlmixr2.github.io/nlmixr2targets/reference/nlmixr_object_simplify.md).
Given a fit produced from the simplified, label-and-meta-stripped model,
plus the original model function and the original data, this function:

## Usage

``` r
nlmixr_object_complicate(fit, object, data)
```

## Arguments

- fit:

  An estimated nlmixr2 fit produced from the simplified model.

- object:

  The original model function (or object) the fit was derived from. Its
  labels and metadata are read back onto `fit`.

- data:

  The original data the fit corresponds to (before
  [`nlmixr_data_simplify()`](https://nlmixr2.github.io/nlmixr2targets/reference/nlmixr_data_simplify.md)
  reduced its column set). Must have the same number of rows as the data
  currently stored on the fit, mirroring
  [`assign_origData()`](https://nlmixr2.github.io/nlmixr2targets/reference/assign_origData.md).

## Value

The modified `fit`.

## Details

- re-derives parameter labels and the metadata environment from `object`
  and writes them back onto `fit$ui$iniDf$label` and `fit$ui$meta`, and

- replaces `fit$env$origData` with the original `data`.

This is what makes label/meta edits in the source model cheap under
`targets`: the cache hash for the simplified model object is independent
of labels and metadata, so
[`tar_make()`](https://docs.ropensci.org/targets/reference/tar_make.html)
only re-runs this re-attachment step (and the cheap `_object_simple`
step) when only labels or metadata change.

This function is typically not invoked directly by end users; it is the
command for the final target produced by
[`tar_nlmixr()`](https://nlmixr2.github.io/nlmixr2targets/reference/tar_nlmixr.md).

## See also

[`nlmixr_object_simplify()`](https://nlmixr2.github.io/nlmixr2targets/reference/nlmixr_object_simplify.md),
[`assign_origData()`](https://nlmixr2.github.io/nlmixr2targets/reference/assign_origData.md).

Other Simplifiers:
[`nlmixr_data_simplify()`](https://nlmixr2.github.io/nlmixr2targets/reference/nlmixr_data_simplify.md),
[`nlmixr_object_simplify()`](https://nlmixr2.github.io/nlmixr2targets/reference/nlmixr_object_simplify.md)
