# Generate a set of targets for nlmixr estimation

The targets generated will include the `name` as the final estimation
step, `paste(name, "object_simple", sep = "_")` (e.g.
`"pheno_object_simple"`) as the simplified model object, and
`paste(name, "data_simple", sep = "_")` (e.g. `"pheno_data_simple"`) as
the simplified data object.

## Usage

``` r
tar_nlmixr(
  name,
  object,
  data,
  est = NULL,
  control = list(),
  table = nlmixr2est::tableControl(),
  env = parent.frame(),
  error = c("stop", "continue")
)

tar_nlmixr_raw(
  name,
  object,
  data,
  est,
  control,
  table,
  object_simple_name,
  data_simple_name,
  fit_simple_name,
  env,
  error = "stop",
  description = NULL
)
```

## Arguments

- name:

  Symbol, name of the target. In
  [`tar_target()`](https://docs.ropensci.org/targets/reference/tar_target.html),
  `name` is an unevaluated symbol, e.g. `tar_target(name = data)`. In
  [`tar_target_raw()`](https://docs.ropensci.org/targets/reference/tar_target.html),
  `name` is a character string, e.g. `tar_target_raw(name = "data")`.

  A target name must be a valid name for a symbol in R, and it must not
  start with a dot. Subsequent targets can refer to this name
  symbolically to induce a dependency relationship: e.g.
  `tar_target(downstream_target, f(upstream_target))` is a target named
  `downstream_target` which depends on a target `upstream_target` and a
  function `f()`.

  In most cases, The target name is the name of its local data file in
  storage. Some file systems are not case sensitive, which means
  converting a name to a different case may overwrite a different
  target. Please ensure all target names have unique names when
  converted to lower case.

  In addition, a target's name determines its random number generator
  seed. In this way, each target runs with a reproducible seed so
  someone else running the same pipeline should get the same results,
  and no two targets in the same pipeline share the same seed. (Even
  dynamic branches have different names and thus different seeds.) You
  can recover the seed of a completed target with
  `tar_meta(your_target, seed)` and run
  [`tar_seed_set()`](https://docs.ropensci.org/targets/reference/tar_seed_set.html)
  on the result to locally recreate the target's initial RNG state.

- object:

  Fitted object or function specifying the model.

- data:

  nlmixr data

- est:

  estimation method (all methods are shown by \`nlmixr2AllEst()\`).
  Methods can be added for other tools

- control:

  The estimation control object. These are expected to be different for
  each type of estimation method

- table:

  The output table control object (like \`tableControl()\`)

- env:

  The environment where the model is setup (not needed for typical use)

- error:

  What should happen if the estimation step throws an error? `"stop"`
  (the default) lets the error propagate, halting
  [`targets::tar_make()`](https://docs.ropensci.org/targets/reference/tar_make.html)
  as usual. `"continue"` catches the error and stores a failure sentinel
  (an object of class `nlmixr2targetsError`, which also inherits from
  `"try-error"`) carrying the error message, so a single failed model
  does not stop the rest of the pipeline. Detect a failed fit with
  `inherits(fit, "nlmixr2targetsError")` or the broader
  `inherits(fit, "try-error")`.

- object_simple_name, data_simple_name, fit_simple_name:

  target names to use for the simplified object, simplified data, fit of
  the simplified object with the simplified data, and fit with the
  original data re-inserted.

- description:

  Human-readable name for the model, announced by the estimation target
  when it starts to run. `NULL` (the default) announces nothing.

## Value

A list of targets for the model simplification, data simplification, and
model estimation.

## Details

For the way that the objects are simplified, see
[`nlmixr_object_simplify()`](https://nlmixr2.github.io/nlmixr2targets/reference/nlmixr_object_simplify.md)
and
[`nlmixr_data_simplify()`](https://nlmixr2.github.io/nlmixr2targets/reference/nlmixr_data_simplify.md).
To see how to write initial conditions to work with targets, see
[`nlmixr_object_simplify()`](https://nlmixr2.github.io/nlmixr2targets/reference/nlmixr_object_simplify.md).

The simplified data keep the columns that the estimation method reads.
For most methods that is the standard event columns plus the covariates
named in the model; `est = "vae"` additionally searches subject-constant
data columns during its automated covariate selection, so those
candidate columns are kept, too (see
[`nlmixr_data_simplify()`](https://nlmixr2.github.io/nlmixr2targets/reference/nlmixr_data_simplify.md)).
Changes to any kept column invalidate the cached fit; changes to dropped
columns do not.

`table` does two jobs: `table$keep` names data columns to carry through
to the simplified data, and a non-default `tableControl()` is handed to
[`nlmixr2est::nlmixr()`](https://nlmixr2.github.io/nlmixr2est/reference/nlmixr2.html)
so the residual/table step honors it. Left at its default, `table` is
omitted from the generated command entirely. That keeps the command
byte-identical to what earlier versions produced, so upgrading does not
re-run cached fits, and it preserves the merge
[`nlmixr2est::nlmixr()`](https://nlmixr2.github.io/nlmixr2est/reference/nlmixr2.html)
performs of the table settings carried on `ui$meta` (`addDosing`,
`subsetNonmem`, `cores`, `keep`, `drop`), which happens only when its
own `table` argument is missing. Supplying any other `tableControl()`
changes the command and re-runs the fit, as a changed `control` would.

## Functions

- `tar_nlmixr_raw()`: An internal function to generate the targets

## Arguments not forwarded to nlmixr2est

:nlmixr(): The generated estimation target calls
[`nlmixr2est::nlmixr()`](https://nlmixr2.github.io/nlmixr2est/reference/nlmixr2.html)
with `object`, `data`, `est`, `control`, and – when you supply one –
`table`. Its three remaining arguments are deliberately left out:

- `...` is inert. Each
  [`nlmixr2est::nlmixr()`](https://nlmixr2.github.io/nlmixr2est/reference/nlmixr2.html)
  method captures it with `match.call(expand.dots = TRUE)` and then
  dispatches to `nlmixr2Est0()` without it, so nothing passed through
  `...` reaches an estimator. There is nothing to forward.

- `save` is accepted by those methods but not acted on by any of them.
  Even if it were, it writes an RDS copy of the fit to the working
  directory; the `targets` store already persists every fit, so the copy
  would be a duplicate produced as an untracked side effect of a target.

- `envir` is the environment `nlmixr2est` passes to
  [`rxode2::.udfEnvSet()`](https://nlmixr2.github.io/rxode2/reference/dot-udfEnvSet.html)
  to resolve R user-defined functions, and uses to evaluate
  back-transformation expressions. `nlmixr2targets` leaves it at its
  default, which resolves to a `nlmixr2targets` internal frame – objects
  defined in your `_targets.R` are not visible from there. A model that
  calls an R user-defined function may therefore fail to resolve it;
  please report it if you hit this.

## Side effects

When the user's model function body contains `cmt(0) <- value` inside a
`model({...})` block, `tar_nlmixr()` rewrites those lines to
`cmt(initial) <- value` directly in the function's binding in `env` so
that `targets`' static analysis (which walks every function in env via
[`codetools::findGlobals()`](https://rdrr.io/pkg/codetools/man/findGlobals.html))
accepts the model. The rewrite is reversed at evaluation time, so
fitting and downstream behaviour are unchanged. The user-visible
consequence is that printing `body(my_model)` at the REPL after a call
to `tar_nlmixr()` will show `cmt(initial)` rather than the
originally-written `cmt(0)`.

Manual `cmt(initial) <- value` written by the user is also accepted, but
it is a `nlmixr2targets`-only workaround: bare nlmixr2 does not
understand the `cmt(initial)` form, so a model function written that way
only fits when routed through `tar_nlmixr()` (or
[`tar_nlmixr_multimodel()`](https://nlmixr2.github.io/nlmixr2targets/reference/tar_nlmixr_multimodel.md)).

## See also

[`tar_nlmixr_multimodel()`](https://nlmixr2.github.io/nlmixr2targets/reference/tar_nlmixr_multimodel.md)
for fitting many models against one dataset.

## Examples

``` r
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

# Build the four targets that estimate `pheno`. `data` and `est` are
# captured as expressions, so this just returns the target list; the
# estimation step runs only when you call `targets::tar_make()` from a
# project whose targets store you have configured (for example, with
# `targets::tar_config_set(store = file.path(tempdir(), "_targets"))`
# or by running inside a project directory you own).
tar_nlmixr(
  name = pheno_model,
  object = pheno,
  data = nlmixr2data::pheno_sd,
  est = "saem"
)
#> $object_simple
#> <tar_stem> 
#>   name: pheno_model_object_simple 
#>   description:  
#>   command:
#>     nlmixr_object_simplify(object = pheno, directory = file.path(targets::tar_config_get("store"), 
#>         "user/nlmixr2")) 
#>   format: rds 
#>   repository: local 
#>   iteration method: vector 
#>   error mode: stop 
#>   memory mode: auto 
#>   storage mode: worker 
#>   retrieval mode: auto 
#>   deployment mode: worker 
#>   priority: 0 
#>   resources:
#>     list() 
#>   cue:
#>     seed: TRUE
#>     file: TRUE
#>     iteration: TRUE
#>     repository: TRUE
#>     format: TRUE
#>     depend: TRUE
#>     command: TRUE
#>     mode: thorough 
#>   packages:
#>     nlmixr2targets
#>     nlmixr2est 
#>   library:
#>     NULL
#> $data_simple
#> <tar_stem> 
#>   name: pheno_model_data_simple 
#>   description:  
#>   command:
#>     nlmixr_data_simplify(object = pheno_model_object_simple, 
#>         data = nlmixr2data::pheno_sd, table = nlmixr2est::tableControl(), 
#>         directory = file.path(targets::tar_config_get("store"), "user/nlmixr2"), 
#>         est = "saem", control = list()) 
#>   format: rds 
#>   repository: local 
#>   iteration method: vector 
#>   error mode: stop 
#>   memory mode: auto 
#>   storage mode: worker 
#>   retrieval mode: auto 
#>   deployment mode: worker 
#>   priority: 0 
#>   resources:
#>     list() 
#>   cue:
#>     seed: TRUE
#>     file: TRUE
#>     iteration: TRUE
#>     repository: TRUE
#>     format: TRUE
#>     depend: TRUE
#>     command: TRUE
#>     mode: thorough 
#>   packages:
#>     nlmixr2targets
#>     nlmixr2est 
#>   library:
#>     NULL
#> $fit_simple
#> <tar_stem> 
#>   name: pheno_model_fit_simple 
#>   description:  
#>   command:
#>     nlmixr2_indirect(object = pheno_model_object_simple, 
#>         data = pheno_model_data_simple, est = "saem", control = list(), 
#>         directory = file.path(targets::tar_config_get("store"), "user/nlmixr2"), 
#>         error = "stop") 
#>   format: rds 
#>   repository: local 
#>   iteration method: vector 
#>   error mode: stop 
#>   memory mode: auto 
#>   storage mode: worker 
#>   retrieval mode: auto 
#>   deployment mode: worker 
#>   priority: 0 
#>   resources:
#>     list() 
#>   cue:
#>     seed: TRUE
#>     file: TRUE
#>     iteration: TRUE
#>     repository: TRUE
#>     format: TRUE
#>     depend: TRUE
#>     command: TRUE
#>     mode: thorough 
#>   packages:
#>     nlmixr2est 
#>   library:
#>     NULL
#> $fit
#> <tar_stem> 
#>   name: pheno_model 
#>   description:  
#>   command:
#>     nlmixr_object_complicate(fit = pheno_model_fit_simple, 
#>         object = pheno, data = nlmixr2data::pheno_sd) 
#>   format: rds 
#>   repository: local 
#>   iteration method: vector 
#>   error mode: stop 
#>   memory mode: auto 
#>   storage mode: worker 
#>   retrieval mode: auto 
#>   deployment mode: worker 
#>   priority: 0 
#>   resources:
#>     list() 
#>   cue:
#>     seed: TRUE
#>     file: TRUE
#>     iteration: TRUE
#>     repository: TRUE
#>     format: TRUE
#>     depend: TRUE
#>     command: TRUE
#>     mode: thorough 
#>   packages:
#>     nlmixr2targets 
#>   library:
#>     NULL
```
