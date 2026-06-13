# Initial conditions cheatsheet

``` r

library(nlmixr2targets)
```

## Two forms accepted inside `nlmixr2targets`

The native nlmixr2 DSL syntax for a compartment initial value is
`cmt(0) <- value` inside a `model({...})` block. Inside
[`tar_nlmixr()`](https://nlmixr2.github.io/nlmixr2targets/reference/tar_nlmixr.md)
and
[`tar_nlmixr_multimodel()`](https://nlmixr2.github.io/nlmixr2targets/reference/tar_nlmixr_multimodel.md)
you may also write `cmt(initial) <- value`, which **`nlmixr2targets`
translates back to `cmt(0) <- value` before nlmixr2 ever sees the
model**. The `cmt(initial)` form is a workaround that exists only for
use through `nlmixr2targets`; it is **not** a form that nlmixr2 itself
understands, so it will fail if you call
[`nlmixr2est::nlmixr()`](https://nlmixr2.github.io/nlmixr2est/reference/nlmixr2.html)
directly on a model that still contains `cmt(initial)`.

Both forms below produce the same fit when routed through
[`tar_nlmixr()`](https://nlmixr2.github.io/nlmixr2targets/reference/tar_nlmixr.md):

``` r

# Form 1: native nlmixr2 DSL syntax (works in nlmixr2 directly and
# inside nlmixr2targets)
model({
  d / dt(central) <- -kel * central
  central(0) <- 0          # initial value at time 0
  cp <- central / vc
  cp ~ add(cpaddSd)
})

# Form 2: `cmt(initial) <- value` -- a `nlmixr2targets`-only
# workaround. Use this only when something prevents the natural
# `central(0) <- 0` form from being walked by `targets` (see the
# "Known limitation" section below). `nlmixr2targets` rewrites this
# back to `central(0) <- 0` before passing the model to nlmixr2.
model({
  d / dt(central) <- -kel * central
  # nlmixr2targets-only; not understood by bare nlmixr2
  central(initial) <- 0
  cp <- central / vc
  cp ~ add(cpaddSd)
})
```

## What `tar_nlmixr()` does behind the scenes

`targets` walks every function in the user’s environment with
[`codetools::findGlobals()`](https://rdrr.io/pkg/codetools/man/findGlobals.html)
and rejects replacement-style assignments whose target is not a symbol –
which is exactly what `central(0) <- 0` looks like. To allow the natural
DSL syntax,
[`tar_nlmixr()`](https://nlmixr2.github.io/nlmixr2targets/reference/tar_nlmixr.md)
walks the captured `object` expression at construction time and rewrites
every `cmt(0) <- value` inside `model({...})` to
`cmt(initial) <- value`. The rewrite is reversed at runtime before the
model is handed to nlmixr2, so the fit you get is identical.

The rewrite mutates the function binding **in place** in the environment
where it was found, so if you later print

``` r

body(my_model)
```

at the REPL after calling
[`tar_nlmixr()`](https://nlmixr2.github.io/nlmixr2targets/reference/tar_nlmixr.md),
you will see `central(initial) <- 0` rather than the originally-typed
`central(0) <- 0`. This is the only user-visible side effect.

## Pipe forms

Both pipe forms below also work, including with `cmt(0)`:

``` r

tar_nlmixr(
  name = m1,
  object = pheno |> model({
    central(0) <- 0
  }, append = TRUE),
  data  = nlmixr2data::pheno_sd,
  est   = "saem"
)

tar_nlmixr(
  name = m2,
  object = pheno |> ini(cpaddSd = 0.3),
  data  = nlmixr2data::pheno_sd,
  est   = "saem"
)
```

A runtime delayed-eval wrapper restores `cmt(0)` before nlmixr2’s pipe
handlers see the parsed body.

## Known limitation: env functions never routed through `tar_nlmixr()`

`targets` walks **every** function in the analysed environment, not just
the ones referenced by a target. If you define

``` r

sketch <- function() {
  ini({
    a <- 1
  })
  model({
    central(0) <- 0
  })
}
```

in the same environment but never pass `sketch` to
[`tar_nlmixr()`](https://nlmixr2.github.io/nlmixr2targets/reference/tar_nlmixr.md),
`targets`’ static analysis will still try to walk its body and will
report a
[`codetools::findGlobals()`](https://rdrr.io/pkg/codetools/man/findGlobals.html)
error. The workaround is either:

- pass the function through
  [`tar_nlmixr()`](https://nlmixr2.github.io/nlmixr2targets/reference/tar_nlmixr.md)
  (the construction-time rewriter cleans it up), or
- write the manual `central(initial) <- 0` form by hand. Because
  `cmt(initial)` is only meaningful when `nlmixr2targets` is the one
  parsing the model, a function written this way is **not usable in a
  bare nlmixr2 workflow** – it only fits if it is routed through
  [`tar_nlmixr()`](https://nlmixr2.github.io/nlmixr2targets/reference/tar_nlmixr.md)
  /
  [`tar_nlmixr_multimodel()`](https://nlmixr2.github.io/nlmixr2targets/reference/tar_nlmixr_multimodel.md).

This case is pinned by a test in `tests/testthat/test-tar_nlmixr.R`.

## Summary

- `cmt(0) <- value` is the native nlmixr2 DSL syntax. Prefer it.
- `cmt(initial) <- value` is a `nlmixr2targets`-only workaround:
  `nlmixr2targets` rewrites it back to `cmt(0) <- value` before nlmixr2
  sees the model. Bare nlmixr2 does **not** understand `cmt(initial)`; a
  model written that way only fits when routed through
  [`tar_nlmixr()`](https://nlmixr2.github.io/nlmixr2targets/reference/tar_nlmixr.md)
  /
  [`tar_nlmixr_multimodel()`](https://nlmixr2.github.io/nlmixr2targets/reference/tar_nlmixr_multimodel.md).
- Inside
  [`tar_nlmixr()`](https://nlmixr2.github.io/nlmixr2targets/reference/tar_nlmixr.md),
  the package quietly rewrites `cmt(0)` forms in env to `cmt(initial)`
  (to survive `targets`’ static analysis) and back again at evaluation
  time.
- After
  [`tar_nlmixr()`](https://nlmixr2.github.io/nlmixr2targets/reference/tar_nlmixr.md),
  `body(my_model)` will show the rewritten form; this is cosmetic and
  reversed at runtime.
- Pipe forms work too via a runtime wrapper.
- Functions in env that contain `cmt(0)` but are never routed through
  [`tar_nlmixr()`](https://nlmixr2.github.io/nlmixr2targets/reference/tar_nlmixr.md)
  need the manual `cmt(initial)` workaround – with the caveat that they
  then only fit through `nlmixr2targets`.
