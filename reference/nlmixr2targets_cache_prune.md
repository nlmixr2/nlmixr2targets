# Drop orphaned entries from the `nlmixr2targets` indirect cache

Removes cache entries whose md5 hash is not in `keep`. By default
`dry_run = TRUE`, so the function reports what would be removed without
actually deleting anything; pass `dry_run = FALSE` to delete.

## Usage

``` r
nlmixr2targets_cache_prune(
  keep = NULL,
  dry_run = TRUE,
  directory = file.path(targets::tar_config_get("store"), "user/nlmixr2")
)
```

## Arguments

- keep:

  Character vector of md5 hashes to retain. If `NULL` (default), the
  helper auto-discovers reachable hashes from the `targets` metadata in
  the current store. Set to
  [`character()`](https://rdrr.io/r/base/character.html) to treat every
  cached entry as an orphan.

- dry_run:

  Logical. If `TRUE` (default), do not delete anything; just return the
  hashes that would be removed. If `FALSE`, delete.

- directory:

  Cache directory; same default as
  [`nlmixr2targets_cache_status()`](https://nlmixr2.github.io/nlmixr2targets/reference/nlmixr2targets_cache_status.md).

## Value

Invisibly, a character vector of the orphan hashes (those that were
removed when `dry_run = FALSE`, or would be removed otherwise).

## Details

When `keep = NULL`, the function reads the `targets` metadata in the
current store and treats every value of a built target whose name ends
in `_object_simple` (the hashes produced by
[`nlmixr_object_simplify()`](https://nlmixr2.github.io/nlmixr2targets/reference/nlmixr_object_simplify.md))
as reachable. Pass an explicit character vector if you want to override
that, e.g. before calling
[`targets::tar_destroy()`](https://docs.ropensci.org/targets/reference/tar_destroy.html)
yourself.

## See also

[`nlmixr2targets_cache_status()`](https://nlmixr2.github.io/nlmixr2targets/reference/nlmixr2targets_cache_status.md).
