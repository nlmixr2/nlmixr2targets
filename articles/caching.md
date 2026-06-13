# Caching strategy and reruns

``` r

library(nlmixr2targets)
```

## What triggers a re-fit?

`nlmixr2targets` builds on `targets`, which caches every target keyed by
a hash of its command, its dependencies, and its inputs. When you call
[`tar_make()`](https://docs.ropensci.org/targets/reference/tar_make.html),
only targets whose hash has changed (directly or transitively) are
rebuilt.

For a single `tar_nlmixr(name = <name>, ...)` call, the four targets
generated are:

| Target | Stores | Hashed inputs |
|----|----|----|
| `<name>_object_simple` | md5 string identifying the simplified ui in the indirect cache | the captured `object` expression |
| `<name>_data_simple` | simplified data frame | `<name>_object_simple`, the captured `data` expression, the `table$keep` columns |
| `<name>_fit_simple` | fitted nlmixr2 object | `<name>_object_simple`, `<name>_data_simple`, `est`, `control` |
| `<name>` | final fit, with labels and metadata restored | `<name>_fit_simple`, the captured `object` (re-read for labels), `data` |

The single most important property of this layout is that
`<name>_fit_simple` depends only on the **structural** content of the
model.
[`nlmixr_object_simplify()`](https://nlmixr2.github.io/nlmixr2targets/reference/nlmixr_object_simplify.md)
strips parameter labels and the model’s metadata environment before
caching, so cosmetic edits to either do not invalidate the expensive
estimation step.

## Examples of common edits

``` r

# Adding a comment, changing a label, or editing meta:
#   only re-runs the cheap `_object_simple` and the final `<name>` target
#   (the relabel/meta-restore step). The expensive `_fit_simple` is reused.

# Changing initial values (`ini({...})`) or the model structure
# (`model({...})`):
#   re-runs everything from `_object_simple` onward.

# Adding/removing a covariate column from the dataset:
#   re-runs `_data_simple` and `_fit_simple`. `_object_simple` is reused
#   if the model didn't change.

# Changing `est` or `control`:
#   re-runs `_fit_simple` and the final target. `_object_simple` and
#   `_data_simple` are reused.
```

## Inspecting and pruning the indirect cache

The simplified-model md5 cache lives under
`<tar_config_get("store")>/user/nlmixr2/`. It is **not** removed by
[`targets::tar_destroy()`](https://docs.ropensci.org/targets/reference/tar_destroy.html),
so it can accumulate orphan files over time (e.g. after model
iterations).

``` r

# Quick inventory: hashes, sizes, last-modified
nlmixr2targets_cache_status()
#>      hash size_bytes               mtime
#> 1 aaaaaa..       1234 2026-05-21 12:00:00
#> 2 bbbbbb..       1567 2026-05-21 13:30:00

# Find orphans (dry run by default)
nlmixr2targets_cache_prune()

# Delete them
nlmixr2targets_cache_prune(dry_run = FALSE)
```

[`nlmixr2targets_cache_prune()`](https://nlmixr2.github.io/nlmixr2targets/reference/nlmixr2targets_cache_prune.md)
with no `keep` argument reads
[`targets::tar_meta()`](https://docs.ropensci.org/targets/reference/tar_meta.html)
from the current store, finds every built target whose name ends in
`_object_simple`, and treats those hash values as reachable. Anything
else in the cache directory is reported (and, with `dry_run = FALSE`,
removed). Pass `keep = character()` to wipe the cache unconditionally.

## `tar_outdated()` is a quick way to preview what would rerun

If you are about to edit a model and are unsure how invasive the change
is, run

``` r

targets::tar_outdated()
```

after the edit but before
[`tar_make()`](https://docs.ropensci.org/targets/reference/tar_make.html).
The list of outdated targets is exactly the set that would re-execute.
Three or four entries means `_fit_simple` is in play; one or two usually
means only the cheap relabel/restore step is.
