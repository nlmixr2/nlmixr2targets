# Inspect the `nlmixr2targets` indirect cache

The indirect cache is a directory of serialized, simplified `nlmixrui`
objects keyed by md5. It lives under `<targets store>/user/nlmixr2/` and
is normally invisible to
[`targets::tar_destroy()`](https://docs.ropensci.org/targets/reference/tar_destroy.html);
these helpers let you inspect and prune it without losing the rest of
the `targets` store.

## Usage

``` r
nlmixr2targets_cache_status(
  directory = file.path(targets::tar_config_get("store"), "user/nlmixr2")
)
```

## Arguments

- directory:

  Cache directory. Defaults to
  `<targets::tar_config_get("store")>/user/nlmixr2`.

## Value

A data frame with one row per cached object and columns `hash` (md5 file
name), `size_bytes` (file size), and `mtime` (modification time). Empty
data frame with the same columns if the cache directory does not exist
or is empty.

## See also

[`nlmixr2targets_cache_prune()`](https://nlmixr2.github.io/nlmixr2targets/reference/nlmixr2targets_cache_prune.md)
for removing orphaned entries.
