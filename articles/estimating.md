# Estimating nlmixr2 models with 'nlmixr2targets'

``` r

library(nlmixr2targets)
```

## Introduction to `nlmixr2targets`

The `nlmixr2targets` package improves reproducibility by ensuring that
your model is up-to-date with your data, and it speeds your workflow
using the `targets` package to only run models when the model or data
have changed.

There are two main functions that are used within the package:

- [`tar_nlmixr()`](https://nlmixr2.github.io/nlmixr2targets/reference/tar_nlmixr.md)
  which runs a single model, and
- [`tar_nlmixr_multimodel()`](https://nlmixr2.github.io/nlmixr2targets/reference/tar_nlmixr_multimodel.md)
  which runs multiple models for a single dataset.

Using `nlmixr2targets` requires the use of the `targets` package. To
learn about the `targets` package, see [the targets
website](https://docs.ropensci.org/targets/).

## Initial conditions

The native nlmixr2 DSL form for a compartment initial value is
`cmt(0) <- value` inside a `model({...})` block. Inside
[`tar_nlmixr()`](https://nlmixr2.github.io/nlmixr2targets/reference/tar_nlmixr.md)
and
[`tar_nlmixr_multimodel()`](https://nlmixr2.github.io/nlmixr2targets/reference/tar_nlmixr_multimodel.md)
you may also write the `nlmixr2targets`-only workaround
`cmt(initial) <- value`, which is rewritten back to `cmt(0) <- value`
before nlmixr2 ever sees the model. The `cmt(initial)` form is **not**
understood by bare nlmixr2 and only fits when routed through
`nlmixr2targets`. Internally,
[`tar_nlmixr()`](https://nlmixr2.github.io/nlmixr2targets/reference/tar_nlmixr.md)
also rewrites `cmt(0)` to `cmt(initial)` in env so that `targets`’
static analysis can walk the model body, then restores `cmt(0)` at
runtime.

See
[`vignette("initial-conditions", package = "nlmixr2targets")`](https://nlmixr2.github.io/nlmixr2targets/articles/initial-conditions.md)
for the full cheatsheet, including the pipe forms
`pheno |> model({...})` and `pheno |> ini(...)`, and the known
[`codetools::findGlobals()`](https://rdrr.io/pkg/codetools/man/findGlobals.html)
edge case for functions in env that are never routed through
[`tar_nlmixr()`](https://nlmixr2.github.io/nlmixr2targets/reference/tar_nlmixr.md).

## Running one model with one dataset (`tar_nlmixr()`)

The
[`tar_nlmixr()`](https://nlmixr2.github.io/nlmixr2targets/reference/tar_nlmixr.md)
function allows you to estimate one model with one dataset. It will
generate three targets: a simplified version of the model, a minimal
version of the dataset, and the estimation step.

The simplified version of the model removes parts that are less
reproducible but changes none of the model intent. (Advanced
information: The parts that are removed are that the source references
and the model name. Also, the model is modified at this step for setting
initial values as described in the previous section of this vignette.)

``` r

library(targets)
library(tarchetypes)
library(nlmixr2targets)

pheno <- function() {
  ini({
    lcl <- log(0.008); label("Typical value of clearance")
    lvc <- log(0.6); label("Typical value of volume of distribution")
    etalcl + etalvc ~ c(1,
                        0.01, 1)
    cpaddSd <- 0.1; label("residual variability")
  })
  model({
    cl <- exp(lcl + etalcl)
    vc <- exp(lvc + etalvc)
    kel <- cl / vc
    d / dt(central) <- -kel * central
    cp <- central / vc
    cp ~ add(cpaddSd)
  })
}

plan_model <-
  tar_plan(
    myData = nlmixr2data::pheno_sd,
    tar_nlmixr(
      model_pheno,
      object = pheno,
      data = myData,
      est = "saem"
    )
  )

list(
  plan_model
)
```

## Running multiple models with one dataset (`tar_nlmixr_multimodel()`)

A common use case is to generate multiple models using a single dataset
and estimation method.
[`tar_nlmixr_multimodel()`](https://nlmixr2.github.io/nlmixr2targets/reference/tar_nlmixr_multimodel.md)
allows the generation of a named list of models to allow subsequent
analysis of all models.

Internally,
[`tar_nlmixr_multimodel()`](https://nlmixr2.github.io/nlmixr2targets/reference/tar_nlmixr_multimodel.md)
passes all the models to
[`tar_nlmixr()`](https://nlmixr2.github.io/nlmixr2targets/reference/tar_nlmixr.md)
so that the data set simplification and equivalent steps run once per
model, and no model is run more often than required for dataset or model
changes.

``` r

library(targets)
library(tarchetypes)
library(nlmixr2targets)

pheno <- function() {
  ini({
    lcl <- log(0.008); label("Typical value of clearance")
    lvc <- log(0.6); label("Typical value of volume of distribution")
    etalcl + etalvc ~ c(1,
                        0.01, 1)
    cpaddSd <- 0.1; label("residual variability")
  })
  model({
    cl <- exp(lcl + etalcl)
    vc <- exp(lvc + etalvc)
    kel <- cl / vc
    d / dt(central) <- -kel * central
    cp <- central / vc
    cp ~ add(cpaddSd)
  })
}

pheno2 <- function() {
  ini({
    lcl <- log(0.008); label("Typical value of clearance")
    lvc <- log(0.6); label("Typical value of volume of distribution")
    etalcl + etalvc ~ c(2,
                        0.01, 2)
    cpaddSd <- 3.0; label("residual variability")
  })
  model({
    cl <- exp(lcl + etalcl)
    vc <- exp(lvc + etalvc)
    kel <- cl / vc
    d / dt(central) <- -kel * central
    cp <- central / vc
    cp ~ add(cpaddSd)
  })
}

plan_model <-
  tar_nlmixr_multimodel(
    all_models,
    data = nlmixr2data::pheno_sd,
    est = "saem",
    "Base model; additive residual error = 1" = pheno,
    "Base model; additive residual error = 3" = pheno2
  )

plan_report <-
  tar_plan(
    # Determine the AIC for all tested models
    aic_list = sapply(X = all_models, FUN = AIC)
  )

list(
  plan_model,
  plan_report
)
```

### Model piping for multiple models estimated with one dataset

Model piping for `nlmixr2` models (see
[`vignette("modelPiping", package = "nlmixr2")`](https://nlmixr2.github.io/nlmixr2/articles/modelPiping.html))
is possible within the multiple models being estimated with
[`tar_nlmixr_multimodel()`](https://nlmixr2.github.io/nlmixr2targets/reference/tar_nlmixr_multimodel.md).
It simplifies examples like the one above so that you can focus on the
model content and avoid rewriting models, as with all `nlmixr2` model
piping.

To use model piping, simply refer to the model by its name like a named
list. Behind the scenes, `nlmixr2targets` will work out the dependencies
between the models and only rerun the dependent model if it or the
dependent model changes.

``` r

library(targets)
library(tarchetypes)
library(nlmixr2targets)
library(nlmixr2)

pheno <- function() {
  ini({
    lcl <- log(0.008); label("Typical value of clearance")
    lvc <- log(0.6); label("Typical value of volume of distribution")
    etalcl + etalvc ~ c(1,
                        0.01, 1)
    cpaddSd <- 0.1; label("residual variability")
  })
  model({
    cl <- exp(lcl + etalcl)
    vc <- exp(lvc + etalvc)
    kel <- cl / vc
    d / dt(central) <- -kel * central
    cp <- central / vc
    cp ~ add(cpaddSd)
  })
}

plan_model <-
  tar_nlmixr_multimodel(
    all_models,
    data = nlmixr2data::pheno_sd,
    est = "saem",
    "Base model; additive residual error = 1" = pheno,
    "Base model; additive residual error = 3" =
    all_models[["Base model; additive residual error = 1"]] |> ini(cpaddSd = 3)
  )

list(
  plan_model
)
```

## Continuing the pipeline when a model fails

By default, if a model fails during estimation the error propagates and
[`targets::tar_make()`](https://docs.ropensci.org/targets/reference/tar_make.html)
stops, just as any other target error would. This is usually what you
want for a single model, but when you are fitting many models at once
(for example with
[`tar_nlmixr_multimodel()`](https://nlmixr2.github.io/nlmixr2targets/reference/tar_nlmixr_multimodel.md))
one failing model would otherwise halt the whole pipeline and prevent
you from seeing the models that did succeed.

Both
[`tar_nlmixr()`](https://nlmixr2.github.io/nlmixr2targets/reference/tar_nlmixr.md)
and
[`tar_nlmixr_multimodel()`](https://nlmixr2.github.io/nlmixr2targets/reference/tar_nlmixr_multimodel.md)
accept an `error` argument to control this:

- `error = "stop"` (the default) lets the estimation error propagate and
  halt the pipeline.
- `error = "continue"` catches the estimation error and stores a failure
  sentinel on the target instead, so the rest of the pipeline still
  runs.

The sentinel is an object of class `nlmixr2targetsError` that also
inherits from `"try-error"`, and it carries the original error message.
Because it is clearly **not** an `nlmixr2` fit object, you can detect a
failed model with a simple
[`inherits()`](https://rdrr.io/r/base/class.html) check.

``` r

library(targets)
library(tarchetypes)
library(nlmixr2targets)

plan_model <-
  tar_nlmixr_multimodel(
    all_models,
    data = nlmixr2data::pheno_sd,
    est = "saem",
    error = "continue",
    "Base model; additive residual error = 1" = pheno,
    "Base model; additive residual error = 3" = pheno2
  )

plan_report <-
  tar_plan(
    # Keep only the models that estimated successfully
    successful_models = Filter(
      f = function(fit) !inherits(fit, "try-error"),
      x = all_models
    ),
    # Compute AIC for the successful models only
    aic_list = sapply(X = successful_models, FUN = AIC)
  )

list(
  plan_model,
  plan_report
)
```

After
[`tar_make()`](https://docs.ropensci.org/targets/reference/tar_make.html),
a target that failed to estimate can be inspected directly:

``` r

fit <- tar_read(all_models)[["Base model; additive residual error = 3"]]
inherits(fit, "try-error")  # TRUE if this model failed
print(fit)                  # shows the captured error message
```

## What `nlmixr2targets` passes to `nlmixr2est::nlmixr()` – and what it omits

The estimation target generated by
[`tar_nlmixr()`](https://nlmixr2.github.io/nlmixr2targets/reference/tar_nlmixr.md)
and
[`tar_nlmixr_multimodel()`](https://nlmixr2.github.io/nlmixr2targets/reference/tar_nlmixr_multimodel.md)
calls
[`nlmixr2est::nlmixr()`](https://nlmixr2.github.io/nlmixr2est/reference/nlmixr2.html)
with `object`, `data`, `est`, `control`, and – when you supply one –
`table`.
[`nlmixr2est::nlmixr()`](https://nlmixr2.github.io/nlmixr2est/reference/nlmixr2.html)
has three further arguments, and `nlmixr2targets` **deliberately does
not pass any of them**.

### `table` (passed, but only when you change it)

`table` does two jobs. `table$keep` names data columns to carry through
into the simplified dataset, and the `tableControl()` itself is handed
to
[`nlmixr2est::nlmixr()`](https://nlmixr2.github.io/nlmixr2est/reference/nlmixr2.html)
so that the residual/table step honors it.

If you leave `table` at its default it is omitted from the generated
target command entirely. That is deliberate, for two reasons: the
command stays byte-identical to what earlier versions of
`nlmixr2targets` produced, so upgrading does not re-run your cached
fits; and `nlmixr2est` merges the table settings carried on the model’s
`meta` (`addDosing`, `subsetNonmem`, `cores`, `keep`, `drop`) into the
table control *only* when its own `table` argument is missing, so
passing an explicit default would silently switch that merge off.

Supplying any other `tableControl()` changes the command and re-runs the
fit, exactly as a changed `control` would.

### `...` – nothing to pass

Each
[`nlmixr2est::nlmixr()`](https://nlmixr2.github.io/nlmixr2est/reference/nlmixr2.html)
method captures `...` with `match.call(expand.dots = TRUE)` and then
dispatches without it, so nothing passed through `...` reaches an
estimator. There is nothing for `nlmixr2targets` to forward.

### `save` – the `targets` store already does this

`save` is accepted by
[`nlmixr2est::nlmixr()`](https://nlmixr2.github.io/nlmixr2est/reference/nlmixr2.html)’s
methods but not acted on by any of them. Even if it were, it writes an
RDS copy of the fit into the working directory. The `targets` store
already persists every fit and knows how to invalidate it; a second copy
would be a duplicate produced as an untracked side effect of a target,
which is the kind of thing a `targets` pipeline exists to avoid.

### `envir` – explicitly omitted

`envir` is the environment `nlmixr2est` hands to
[`rxode2::.udfEnvSet()`](https://nlmixr2.github.io/rxode2/reference/dot-udfEnvSet.html)
to resolve R user-defined functions, and the one it uses to evaluate
back-transformation expressions.

`nlmixr2targets` leaves it at its default. That default resolves to an
environment inside `nlmixr2targets` itself, from which objects defined
in your `_targets.R` are **not** visible. The practical consequence is
that a model calling an R user-defined function may fail to resolve that
function when it is fitted through
[`tar_nlmixr()`](https://nlmixr2.github.io/nlmixr2targets/reference/tar_nlmixr.md),
even though the same model fits when called directly. If you run into
this, please open an issue describing the model – the fix has trade-offs
(a function reached this way is not necessarily something `targets`
knows to watch for changes) and is worth designing against a real
example.
