# Generate a list of models based on a single dataset and estimation method

Generate a list of models based on a single dataset and estimation
method

## Usage

``` r
tar_nlmixr_multimodel(
  name,
  ...,
  data,
  est,
  control = list(),
  table = nlmixr2est::tableControl(),
  env = parent.frame(),
  error = c("stop", "continue")
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

- ...:

  Named arguments with the format `"Model description" = modelFunction`

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

## Value

A list of targets for the model simplification, data simplification, and
model estimation.

## See also

[`tar_nlmixr()`](https://nlmixr2.github.io/nlmixr2targets/reference/tar_nlmixr.md)
for fitting a single model.

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

# Build the per-model target chains plus the combined list target.
# Estimation runs only when `targets::tar_make()` is invoked from a
# project whose store you have configured (see `?tar_nlmixr` for one
# tempdir-based setup).
tar_nlmixr_multimodel(
  name = all_models,
  data = nlmixr2data::pheno_sd,
  est = "saem",
  "Base model" = pheno,
  "Alternative residual error" = pheno2
)
#> [[1]]
#> [[1]]$object_simple
#> <tar_stem> 
#>   name: all_models_8ae20c5c_object_simple 
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
#> [[1]]$data_simple
#> <tar_stem> 
#>   name: all_models_8ae20c5c_data_simple 
#>   description:  
#>   command:
#>     nlmixr_data_simplify(object = all_models_8ae20c5c_object_simple, 
#>         data = nlmixr2data::pheno_sd, table = nlmixr2est::tableControl(), 
#>         directory = file.path(targets::tar_config_get("store"), "user/nlmixr2")) 
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
#> [[1]]$fit_simple
#> <tar_stem> 
#>   name: all_models_8ae20c5c_fit_simple 
#>   description:  
#>   command:
#>     nlmixr2_indirect(object = all_models_8ae20c5c_object_simple, 
#>         data = all_models_8ae20c5c_data_simple, est = "saem", control = list(), 
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
#> [[1]]$fit
#> <tar_stem> 
#>   name: all_models_8ae20c5c 
#>   description:  
#>   command:
#>     nlmixr_object_complicate(fit = all_models_8ae20c5c_fit_simple, 
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
#> 
#> [[2]]
#> [[2]]$object_simple
#> <tar_stem> 
#>   name: all_models_b0a374c4_object_simple 
#>   description:  
#>   command:
#>     nlmixr_object_simplify(object = pheno2, directory = file.path(targets::tar_config_get("store"), 
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
#> [[2]]$data_simple
#> <tar_stem> 
#>   name: all_models_b0a374c4_data_simple 
#>   description:  
#>   command:
#>     nlmixr_data_simplify(object = all_models_b0a374c4_object_simple, 
#>         data = nlmixr2data::pheno_sd, table = nlmixr2est::tableControl(), 
#>         directory = file.path(targets::tar_config_get("store"), "user/nlmixr2")) 
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
#> [[2]]$fit_simple
#> <tar_stem> 
#>   name: all_models_b0a374c4_fit_simple 
#>   description:  
#>   command:
#>     nlmixr2_indirect(object = all_models_b0a374c4_object_simple, 
#>         data = all_models_b0a374c4_data_simple, est = "saem", control = list(), 
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
#> [[2]]$fit
#> <tar_stem> 
#>   name: all_models_b0a374c4 
#>   description:  
#>   command:
#>     nlmixr_object_complicate(fit = all_models_b0a374c4_fit_simple, 
#>         object = pheno2, data = nlmixr2data::pheno_sd) 
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
#> 
#> [[3]]
#> <tar_stem> 
#>   name: all_models 
#>   description:  
#>   command:
#>     list(`Base model` = all_models_8ae20c5c, `Alternative residual error` = all_models_b0a374c4) 
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
#>     stats
#>     graphics
#>     grDevices
#>     utils
#>     datasets
#>     methods
#>     base 
#>   library:
#>     NULL
```
